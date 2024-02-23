if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, cobalt)
install.packages("knitr")
library(knitr)
library(ggplot2)
install.packages('scales')
library(scales)


##Analysis Code

# DATA SUMMARY

# 1 How many hospitals filed more than one report in the same year?
hospital_counts = duplicate.hcris %>%
  group_by(fyear) %>%
  count()
        #why are there 16 values in 2007 lol

hospital_counts_plot = ggplot(hospital_counts, aes(x = fyear, y = n)) +
  geom_line() +
  labs(
    x = "Year",
    y = "Hospital Counts",
    title = "Number of Duplicate Hospital Ids") + 
    theme_minimal()
ggsave("hospital_counts_plot.png") 

# 2 Unique hospital IDs (Medicare provider numbers) 
# UNGROUP BY YEAR
unique_hospital_ids = final.hcris %>% ungroup() %>%
  distinct(provider_number) %>%
  n_distinct()
print(unique_hospital_ids)
        # There are 6747 unique hospital IDs

# 3 Distribution of total charges per year, SET LIMIT TO REMOVE OUTLIERS
charges_distribution <- ggplot(final.hcris.data, aes(x = as.factor(year), y = tot_charges)) +
  geom_violin(fill = "skyblue", color = "blue", alpha = 0.6, trim = 0.05) + 
  labs(
        x = "Year", 
        y = "Total Charges", 
        title = "Distribution of Total Charges by Year") + theme_minimal()
  ggsave("charges_distribution.png")

# 4 Distribution of estimated prices by year, FILTER OUT OUTLIERS
final.hcris.data = final.hcris.data %>%
        mutate(discount_factor = 1-tot_discounts/tot_charges)
final.hcris.data <- final.hcris.data %>%
  mutate(price_num = (ip_charges + icu_charges + ancillary_charges) * discount_factor - tot_mcare_payment,
    price_denom = tot_discharges - mcare_discharges,
    price = price_num / price_denom) %>%
    filter(price >=0)   

estimated_prices_plot = ggplot(final.hcris.data, aes(x = as.factor(year), y = price)) +
  geom_violin(fill = "skyblue", color = "blue") +
  labs(
    x = "Year",
    y = "Estimated Price",
    title = "Distribution of Estimated Prices by Year"
  ) +
  theme_minimal() 
ggsave("estimated_prices_plot.png")

# ATE ESTIMATES
final.hcris.data <- final.hcris.data %>% ungroup() %>%
  filter(price_denom>100, !is.na(price_denom), 
         price_num>0, !is.na(price_num),
         price<100000, 
         beds>30, year==2012) %>%  
  mutate( hvbp_payment = ifelse(is.na(hvbp_payment),0,hvbp_payment),
          hrrp_payment = ifelse(is.na(hrrp_payment),0,abs(hrrp_payment)), 
    penalty = (hvbp_payment-hrrp_payment<0)) 

# 5 Average price among penalized vs non-penalized hospitals
mean.pen <- round(mean(final.hcris.data$price[which(final.hcris.data$penalty==1)]),2)
mean.nopen <- round(mean(final.hcris.data$price[which(final.hcris.data$penalty==0)]),2)

avg_prices = final.hcris.data %>%
  group_by(year, penalty) %>%
  summarize(mean_price = mean(price, na.rm = TRUE))

avg_prices_plot <- ggplot(avg_prices, aes(x = penalty, y = mean_price, fill = penalty)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Penalty Status",
    y = "Average Hospital Price",
    title = "Average Hospital Prices by Penalty Status"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0))
ggsave("avg_prices.png")

print(avg_prices)

# 6 Average price among treated/control groups by bed size quartiles
# final.hcris %>% filter(is.na(beds)) %>% nrow()
final.hcris.data <- final.hcris.data %>%
  mutate(quartile = ntile(beds, 4)) %>%
  mutate(
    q1 = ifelse(quartile == 1, 1, 0),
    q2 = ifelse(quartile == 2, 1, 0),
    q3 = ifelse(quartile == 3, 1, 0),
    q4 = ifelse(quartile == 4, 1, 0)
  ) 

avg_price_table = final.hcris.data %>%
  group_by(quartile, penalty) %>%
  summarize(avg_price = mean(price, na.rm = TRUE))
avg_price_table = avg_price_table %>%
        mutate(penalty = ifelse(penalty, "Penalized", "Non-Penalized"))
print(avg_price_table)

# 7 ATE 
lp.vars <- final.hcris.data %>%
  filter(year == 2012) %>%
  dplyr::select(q1, q2, q3, q4, penalty, price) %>%
  filter(complete.cases(.))

lp.covs <- lp.vars %>%
  dplyr::select(-c("penalty", "price"))

# Nearest neighbor matching (1-to-1) with inverse variance distance based on bed size quartiles
install.packages('Matching')
library(Matching)
 
m.nn.var2 <- Matching::Match(Y=lp.vars$price,
                             Tr=lp.vars$penalty,
                             X=lp.covs,
                             M=1,   
                             Weight=1,
                             estimand="ATE")
summary(m.nn.var2)

# Nearest neighbor matching (1-to-1) with Mahalanobis distance based on bed size quartiles
m.nn.md <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=lp.covs,
                           M=1,
                           Weight=2,
                           estimand="ATE")  
summary(m.nn.md)

# Inverse propensity weighting, where the propensity scores are based on bed size quartiles
logit.model <- glm(penalty ~ q1 + q2+ q3 + q4, family=binomial, data=lp.vars)
ps <- fitted(logit.model)
m.nn.ps <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=ps,
                           M=1,
                           estimand="ATE")
summary(m.nn.ps)

# Simple linear regression, adjusting for quartiles of bed size using dummy variables
reg <- lm(price ~ penalty + q1 + q2 + q3 + q4,
          data=lp.vars)
summary(reg)

# table
method <- c("Nearest neighbor matching with inverse variance distance", 
            "Nearest neighbor matching with Mahalanobis distance", 
            "Inverse propensity weighting based on bed size", 
            "Simple linear regression adjusting for bed size")
ATE <- c(199.53, 199.53, 199.53, 183.5)
ate_table = data.frame(Method = method, ATE = ATE)
print(ate_table)
