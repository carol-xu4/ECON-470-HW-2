if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)
install.packages("knitr")
library(knitr)
library(ggplot2)
install.packages('scales')
library(scales)

##Analysis Code

# DATA SUMMARY

# 1 How many hospitals filed more than one report in the same year?
hospital_counts = final.hcris %>%
  group_by(fyear) %>%
  count()
        #why are there 16 values in 2007 lol

hospital_counts_plot = ggplot(hospital_counts, aes(x = fyear, y = n)) +
  geom_line() +
  labs(
    x = "Year",
    y = "Number of Hospitals",
    title = "Number of Hospitals per Year") + 
    theme_minimal()
ggsave("hospital_counts_plot.png")


# 2 Unique hospital IDs (Medicare provider numbers)
unique_hospital_ids = final.hcris %>%
  distinct(provider_number) %>%
  n_distinct()
print(unique_hospital_ids)
        # There are 48803 unique hospital IDs

# 3 Distribution of total charges per year
charges_distribution = ggplot(final.hcris, aes(x = as.factor(fyear), y = tot_charges)) +
  geom_violin(fill = "skyblue", color = "blue", alpha = 0.6) +
  labs(
    x = "Year",
    y = "Total Charges",
    title = "Distribution of Total Charges by Year"
  ) +
  theme_minimal()
  ggsave("charges_distribution.png")

# 4 Distribution of estimated prices by year
final.hcris = final.hcris %>%
        mutate(discount_factor = 1-tot_discounts/tot_charges)
final.hcris <- final.hcris %>%
  mutate(price_num = (ip_charges + icu_charges + ancillary_charges) * discount_factor - tot_mcare_payment,
    price_denom = tot_discharges - mcare_discharges,
    price = price_num / price_denom)   

estimated_prices_plot = ggplot(final.hcris, aes(x = as.factor(fyear), y = estimated_price)) +
  geom_violin(fill = "skyblue", color = "blue", alpha = 0.6) +
  labs(
    x = "Year",
    y = "Estimated Price",
    title = "Distribution of Estimated Prices by Year"
  ) +
  theme_minimal() 
print(estimated_prices_plot)

# ATE ESTIMATES

# 5 Average price among penalized vs non-penalized hospitals

# 6 Average price among treated/control groups by bed size quartiles

# 7 ATE 
# Nearest neighbor matching