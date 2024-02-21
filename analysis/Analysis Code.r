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
    title = "Number of Unique Hospital Ids") + 
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
charges_distribution <- ggplot(final.hcris.data, aes(x = year, y = tot_charges)) +
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
ggsave("avg_prices.png", avg_prices_plot)

# 6 Average price among treated/control groups by bed size quartiles
# final.hcris %>% filter(is.na(beds)) %>% nrow()
# true and false for penalty / treatment groups
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
install.packages('cobalt')
library(cobalt)

lp.vars <- final.hcris.data %>% 
  select(beds, mcaid_discharges, penalty, ip_charges, 
         mcare_discharges, tot_mcare_payment, price) %>%
  filter(complete.cases(.))
lp.covs <- lp.vars %>% select(-c("penalty","price"))

love.plot(bal.tab(lp.covs,treat=lp.vars$penalty), colors="black", shapes="circle", threshold=0.1) + 
  theme_bw() + theme(legend.position="none")

# Nearest neighbor matching (1-to-1) with inverse variance distance based on bed size quartiles
install.packages('Matching')
library(Matching)

lp.covs2 <- lp.covs[, c("beds", "mcaid_discharges")]
 
m.exact <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=lp.covs2,
                           M=1,
                           exact=TRUE,
                           estimand="ATE") 
print(m.exact)

print(ate1)

m.nn.var <- Matching::Match(Y=lp.vars$price,
                            Tr=lp.vars$penalty,
                            X=lp.covs,
                            M=4,  #<<
                            Weight=1,
                            estimand="ATE")

m.nn.var2 <- Matching::Match(Y=lp.vars$price,
                             Tr=lp.vars$penalty,
                             X=lp.covs,
                             M=1,   #<<
                             Weight=1,
                             estimand="ATE")
ate1 = m.nn.var2$estimates$ATE
print(ate1)

v.name=data.frame(new=c("Beds","Medicaid Discharges", "Inaptient Charges",
                   "Medicare Discharges", "Medicare Payments"))

# Nearest neighbor matching (1-to-1) with Mahalanobis distance based on bed size quartiles
m.nn.md <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=lp.covs,
                           M=1,
                           Weight=2,
                           estimand="ATE")  
ate2 = m.nn.md$estimates$ATE
print(ate2)

# Inverse propensity weighting, where the propensity scores are based on bed size quartiles
logit.model <- glm(penalty ~ beds + mcaid_discharges + ip_charges + mcare_discharges +
            tot_mcare_payment, family=binomial, data=lp.vars)
ps <- fitted(logit.model)
m.nn.ps <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=ps,
                           M=1,
                           estimand="ATE")
ate3 = m.nn.ps$estimates$ATE
print(ate3)

# Simple linear regression, adjusting for quartiles of bed size using dummy variables
reg.dat <- lp.vars %>% ungroup() %>% filter(complete.cases(.)) %>%
  mutate(beds_diff = penalty*(beds - mean(beds)),
         mcaid_diff = penalty*(mcaid_discharges - mean(mcaid_discharges)),
         ip_diff = penalty*(ip_charges - mean(ip_charges)),
         mcare_diff = penalty*(mcare_discharges - mean(mcare_discharges)),
         mpay_diff = penalty*(tot_mcare_payment - mean(tot_mcare_payment)))
reg <- lm(price ~ penalty + beds + mcaid_discharges + ip_charges + mcare_discharges + tot_mcare_payment + 
            beds_diff + mcaid_diff + ip_diff + mcare_diff + mpay_diff,
          data=reg.dat)
summary(reg)

ate4 = reg$estimates$ATE
print(ate4)

