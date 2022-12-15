library(plm)
library(stats)
library(dplyr)

### Fetching data ###

data = read.csv("lcudata.csv")

use_imp = TRUE # For data with imputed single NAs in ng_price. Disable line above.

# Defining exogenous variables
ex_vars <- c("price_ng", "income", "hdd")

if (use_imp) {
  subset(data, two_incomplete == FALSE & demand > 0)
  ex_vars[1] <- "price_ng_imp"
  
} else {
  data <- data.frame(data %>% drop_na(c(price_ng, income, demand, hdd))) %>% filter(demand > 0)
}

data <- subset(data, country != "ESP" | time >= "1987" )

# Log transforming data
logdata <- data
logdata[c("demand", ex_vars)] <- log(logdata[c("demand", ex_vars)])

#Transform to pdata.frame to enable appropriate lag function
data <-pdata.frame(logdata,index=c("country","time"))

#The price variable imputed/ non-imputed that we will use
price = ex_vars[1]

#Create first differences of the variables
data$ddemand = data$demand - plm::lag(data$demand) %>% replace(is.na(.),0)
data$dprice= data[,price] - plm::lag(data[,price]) %>% replace(is.na(.),0)
data$dincome = data$income - plm::lag(data$income) %>% replace(is.na(.),0)
data$dhdd= data$hdd - plm::lag(data$hdd) %>% replace(is.na(.),0)

data = data %>% replace(is.na(.),0)

#variables to be included for testing
testing_vars = c("demand", price, "income", "hdd", "ddemand","dprice","dincome","dhdd")

#initialize df
unit_root_df = data.frame(matrix(nrow=0, ncol=0))

for (var in testing_vars){
  s = data.frame(pivot_wider(data, id_cols = time, names_from = country, values_from = var) %>% na.omit)
  s = select(s,-c(1))

  #Run each test with an without a trend, for a max of lags = 2
  madwu = purtest(s, pmax=2, test="madwu", exo="intercept")$statistic
  madwu = paste(signif(madwu$statistic,digits=4), ifelse(madwu$p.value<=0.01, "***", ifelse(madwu$p.value<=0.05, "**", ifelse(madwu$p.value<0.1, "*", ""))))
  
  madwu_t = purtest(s, pmax=2, test="madwu", exo="trend")$statistic
  madwu_t = paste(signif(madwu_t$statistic,digits=4), ifelse(madwu_t$p.value<=0.01, "***", ifelse(madwu_t$p.value<=0.05, "**", ifelse(madwu_t$p.value<0.1, "*", ""))))
  
  levinlin = purtest(s, pmax=2, test="levinlin", exo="intercept")$statistic
  levinlin = paste(signif(levinlin$statistic,digits=4),ifelse(levinlin$p.value<=0.01, "***", ifelse(levinlin$p.value<=0.05, "**", ifelse(levinlin$p.value<0.1, "*", ""))))
  
  levinlin_t = purtest(s, pmax=2, test="levinlin", exo="trend")$statistic
  levinlin_t = paste(signif(levinlin_t$statistic,digits=4),ifelse(levinlin_t$p.value<=0.01, "***", ifelse(levinlin_t$p.value<=0.05, "**", ifelse(levinlin_t$p.value<0.1, "*", ""))))
  
  ips = purtest(s, pmax=2, test="ips", exo="intercept")$statistic
  ips = paste(signif(ips$statistic,digits=4),ifelse(ips$p.value<=0.01, "***", ifelse(ips$p.value<=0.05, "**", ifelse(ips$p.value<0.1, "*", ""))))
  
  ips_t = purtest(s, pmax=2, test="ips", exo="trend")$statistic
  ips_t = paste(signif(ips_t$statistic,digits=4), ifelse(ips_t$p.value<=0.01, "***", ifelse(ips_t$p.value<=0.05, "**", ifelse(ips_t$p.value<0.1, "*", ""))))
  
  unit_root_df = rbind(unit_root_df, c(var, madwu, levinlin, ips, madwu_t,levinlin_t, ips_t))
}

names(unit_root_df) = c("Variable", "Maddala Wu", "Levin & Lin", "IPS", "Maddala Wu w / trend", "Levin & Lin w/trend", "IPS w/ trend")
unit_root_df

xtable(unit_root_df, type = "latex", file = "unit_root.tex")
