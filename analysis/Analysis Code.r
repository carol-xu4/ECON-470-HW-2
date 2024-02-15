##Analysis Code

# DATA SUMMARY

# 1 How many hospitals filed more than one report in the same year?
final.hcris %>% group_by(fyear) %>% count()

# 2 Unique hospital IDs (Medicare provider numbers)

# 3 Distribution of total charges per year

# 4 Distribution of estimated prices by year

# ATE ESTIMATES

# 5 Average price among penalized vs non-penalized hospitals

# 6 Average price among treated/control groups by bed size quartiles

# 7 ATE 
# Nearest neighbor matching