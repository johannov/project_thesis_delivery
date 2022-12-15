### Script to estimate country-specific ARDL models and Mean Group results from data retrieved by running file data_prep.R ###

library(ARDL)
library(plyr)
library(tseries)
library(lmtest)
library(poolr)
library(xtable)
library(stats)
library(msm)
library(tidyverse)


# Importing data
data <-  read.csv("./output/lcudata.csv")
data

use_imp <-  TRUE # For data with imputed NAs in ng_price.

# Defining exogenous variables
ex_vars <- c("price_ng", "income", "hdd")


if (use_imp) {
  ex_vars[1] <- "price_ng_imp"
  
} else {
  data <- data.frame(data %>% drop_na(c(price_ng, income, demand, hdd))) %>% filter(demand > 0)
}


# Log transforming data
logdata <- data

logdata[c("demand", ex_vars)] <- log(logdata[c("demand", ex_vars)])

# n: Number of countries analyzed
n <-  length(unique(logdata$country))

# Initializing empty matrices to store results
df <-  data.frame(matrix(nrow=0, ncol=0))
df_result_table <-  data.frame(matrix(nrow=0, ncol=0))
df_multipliers <-  data.frame(matrix(nrow=0, ncol=0))
df_indiv_multiplier <- data.frame(matrix(nrow=0, ncol=0))
df_diagnostics <-  data.frame(matrix(ncol=0, nrow=0))
df_bounds <-  data.frame(matrix(ncol=0, nrow=0))
df_covariances <-  c()

# Estimating per country
for (state in unique(logdata$country)) {
  linform <- as.formula(paste("demand ~ ", paste(ex_vars, collapse = " + ")))
  
  countrydata <-  subset(logdata, country == state)
  
  cat(c("\n\n\n--- ", state, " --- \n\n"))
  
  #Introduce dummies for structural breaks in FRA and LUX
  if (state == "FRA"){
    countrydata$dummy <- ifelse(countrydata$time < "1990",1,0)
    linform <- as.formula(paste(c(linform), "| dummy", collapse=""))
  }
  
  if (state == "LUX"){
    countrydata$dummy <- ifelse(countrydata$time < "2000",1,0)
    linform <- as.formula(paste(c(linform), "| dummy", collapse=""))
  }
  
  # Estimating optimal ARDL lag structure using AIC
  test_ardl <- auto_ardl(linform, data = countrydata, max_order = 3)

  ardl <- test_ardl$best_model
  print(test_ardl$best_order)
  
  # Estimate conditional ECM
  test_uecm <- uecm(ardl)
  
  ### --- Bounds test for cointegration --- ###
  
  
  # Case: unrestricted trend, unrestricted constant
  btest <- bounds_f_test(test_uecm, case = "uc")
  # Storing results
  df_bounds <- rbind(df_bounds, c(state,btest$statistic))
  
  
  ### --- Reporting ARDL results --- ###
  # Coefficients from ARDL estimation
  coefs <- summary(test_uecm)$coefficients
  
  # Estimation coefficients
  estimates <- coefs[,1]
  df <- rbind(df, cbind(state, names(estimates), estimates))
  
  # Significance levels
  significance <- coefs[,4]
  
  # Standard error
  se  <-  coefs[,2]
  
  # Covariance matrix:
  cov <- stats::vcov(test_uecm)
  df_covariances <- append(df_covariances, list(data.frame(cov)))
  
  # Join estimates and significance levels to report ONE table with estimates, standard errors and significance for latex:
  # NB! This would not be numeric, thats why we need to store the tables separately to estimate means later!
  result <- paste(round(estimates,3), ifelse(significance<=0.01, "***", ifelse(significance<=0.05, "**", ifelse(significance<0.1, "*", ""))), "(",round(se,3),")", sep="")
  df_result_table <- rbind(df_result_table, cbind(state, names(estimates), result))
  
  
  
  
  ### --- Calculating country-specific multipliers --- ###
  
  mlr <- multipliers(ardl)
  #Numerical table for later calculations
  df_multipliers <- rbind(df_multipliers, cbind(state, mlr[,1:2]))
  #country-specific results

  df_indiv_multiplier <- rbind(df_indiv_multiplier, cbind(state, mlr$term, paste(round(mlr$estimate,3)
                        , ifelse(mlr$p.value<=0.01, "***", ifelse(mlr$p.value<=0.05, "**", ifelse(mlr$p.value<0.1, "*", "")))
                        , "(",round(mlr$std.error,3),")", sep="")))
  
 
  ### --- Diagnostic tests --- ###
 
  jarque_bera <- jarque.bera.test(residuals(ardl))
  ljung_box <- Box.test(residuals(ardl), type = "Ljung-Box")
  breusch_pagan <- bptest(ardl)
  
  # Table for latex, test statistics ( all chi-squared + significance levels reported in paranthesis)
  df_diagnostics <- rbind(df_diagnostics, c(state, paste(round(jarque_bera$statistic,3),"(", round(jarque_bera$p.value,3), ")", sep="")
                                            , paste(round(ljung_box$statistic,3), "(", round(ljung_box$p.value,3), ")", sep="")
                                            , paste(round(breusch_pagan$statistic,3), "(", round(breusch_pagan$p.value,3), ")", sep="")))
  
} # End for loop


### --- OVERVIEW --- ###
# Constructing table with estimate, sd and p-values
full_result_table <- pivot_wider(df_result_table, names_from=V2, values_from = result, values_fill = "-")
full_result_table <- full_result_table %>% select(-c("L(price_ng_imp, 1)","L(income, 1)","L(hdd, 1)","price_ng_imp","income","hdd"))

print("--- Country-specific ARDL results ---")
full_result_table

#--- Residual diagnostics ---#

names(df_diagnostics) <-  c("Country", "Jarque-Bera", "Ljung-Box", "Breusch-Pagan")
print("--- Country-specific residual diagnostics (p-values in paranthesis) ---")
df_diagnostics

### --- MEAN-GROUP ESTIMATES --- ###

# Retrieving strictly numeric country-specific estimates

df$estimates <- as.numeric(df$estimates)
estimation_results <- pivot_wider(df, names_from=V2, values_from = estimates, values_fill =  0)
estimation_results <- select(estimation_results, -c("state","dummy"))

# Calculating means, se and p-values for MG estimates
get_full_mean <- function(result_df){
  mean_df <- data.frame(matrix(nrow=0, ncol=0))
  
  for (var in names(result_df)){
    n <- nrow(result_df[,var])
    
    se <- sqrt(1/n) * sd(unlist(result_df[,var]))
    mean <- mean(unlist(result_df[,var]))
    t_stat <- mean/se
    p_val <- 2 * stats::pt(-abs(t_stat), df = n-1)
    
    full <- paste(round(mean,3), ifelse(p_val<=0.01, "***", ifelse(p_val<=0.05, "**", ifelse(p_val<0.1, "*", ""))), "(" , round(se,3) ,")", sep="")
    mean_df <- rbind(mean_df, c(var, full))
  }
  names(mean_df) <- c("Variable", "Estimate")
  return(mean_df)
}

# MG coefficients
estimation_mean <- get_full_mean(estimation_results)
estimation_mean <- subset(estimation_mean, estimation_mean$Variable %in% names(full_result_table))
print(" --- MG estimates --- ")
estimation_mean

# MG long-run multipliers (coefficients in long-run relationship)
long_run <- df_multipliers %>% pivot_wider(names_from ="term", values_from = "estimate") %>% select(-c("(Intercept)"))
# Return long-run multipliers from MG estimation results
long_run_mult  <- get_full_mean(long_run %>% select(c(-1)))

print(" --- Long-run multipliers ---")
long_run_mult 


# Calculating covariance matrix of MG parameters (1/n^2)sum(cov_mat(i))

# Initialize empty matrix
cov_matrix <- matrix(0*18*18, nrow=18, ncol=18)

# Rownames and colnames to match
rownames(cov_matrix) <- names(estimation_results)
colnames(cov_matrix) <- names(estimation_results)

for (i in 1:n) {
  # The covariance matrix of country n
  cov_df = data.frame(df_covariances[i])
  cov_df = cov_df[row.names(cov_df)!="dummy", colnames(cov_df)!="dummy"]
  #colnames(cov_df)= rownames(cov_df)
  # For each variable in that matrix
  for (j in rownames(cov_df)){
    for(k in rownames(cov_df)){
      #colnames get distorted for cov_df for some reason, find the appropriate index instead
      idx_k <- which(rownames(cov_df)==k)
      idx_j <- which(rownames(cov_df)==j)
      cov_matrix[j,k] <- cov_matrix[j,k] + cov_df[idx_j,idx_k]
    }
  }
}

cov_matrix <- (1/(n^2))*cov_matrix


# sr_mult_mg returns short-run multipliers from MG estimation results

sr_mult_mg <- function(coeff, ex_vars, vcov_mat, dof) {
  coeff = colMeans(coeff)
  
  # Initializing variables
  df <- setNames(data.frame(matrix(ncol = 4, nrow = length(ex_vars)), row.names = ex_vars), c("impact", "se", "t_stat", "p_val"))
  
  d_ex_vars_names <- paste0("d(", ex_vars, ")", sep ="")
  d_ex_vars_vals <- coeff[d_ex_vars_names]
  ect_val <- as.numeric(coeff['L(demand, 1)'])
  
  # Calculating short run multiplier value, "impact"
  df$impact <- d_ex_vars_vals / (- ect_val)
  
  # Calculating standard errors
  i <- 1
  
  for (var in d_ex_vars_names) {
    df[i, 'se'] <- msm::deltamethod(~ x1 / (- x2), c(d_ex_vars_vals[var], ect_val), vcov_mat[c(var, 'L(demand, 1)'), c(var, 'L(demand, 1)')])
    i <- i + 1
  }
  df
  # Calculating t-values
  df$t_stat <- df$impact / df$se
  
  # Calculating corresponding p-value
  df$p_val <- 2 * stats::pt(-abs(df$t_stat), df = dof)
  return(df)
  
}


print(" --- Short-run multipliers ---")
sr_mult= sr_mult_mg(estimation_results, ex_vars, cov_matrix, nrow(estimation_results) - 1)
short_run_mult = data.frame(matrix(ncol=0, nrow=0))

for (var in rownames(sr_mult)){
  full <- paste(round(sr_mult[var,]$impact,3), ifelse(sr_mult[var,]$p_val<=0.01, "***", ifelse(sr_mult[var,]$p_val<=0.05, "**", ifelse(sr_mult[var,]$p_val<0.1, "*", ""))), "(" , round(sr_mult[var,]$se,3) ,")", sep="")
  short_run_mult = rbind(short_run_mult, c(var, full))
}
names(short_run_mult) <- c("Variable", "Short run estimate")

# Join multipliers to one table
mult_table <- join(long_run_mult, short_run_mult, by="Variable")
names(mult_table) <- c("Variable", "Long run multiplier", "Short run multiplier")
mult_table

# Report country-specific multiplier results
names(df_bounds) <- c("Country", "Test statistic")
names(df_indiv_multiplier) <- c("Country", "Variable", "Long run coefficients")


