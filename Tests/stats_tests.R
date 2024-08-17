###This script runs statistical tests###

library(stats)
# Compute Hit Test, UC Test, CC Test, and ES
compute_var_tests <- function(combined_data, model, horizon, alpha_0.05 = 0.05, alpha_0.01 = 0.01) {
  if (horizon == 1) {
    returns_col <- "returns"
    var_0_05_col <- paste0(model, "_VaR_0.05_1d")
    var_0_01_col <- paste0(model, "_VaR_0.01_1d")
  } else if (horizon == 10) {
    returns_col <- "sum_returns"
    var_0_05_col <- paste0(model, "_VaR_0.05_10d")
    var_0_01_col <- paste0(model, "_VaR_0.01_10d")
  } else {
    stop("Invalid horizon. Please use 1 or 10.")
  }
  
  # Filter test data
  test_data <- combined_data %>% filter(TrainTest == 'Test')
  
  # Calculate hits
  hits_0_05 <- test_data[[returns_col]] < test_data[[var_0_05_col]]
  hits_0_01 <- test_data[[returns_col]] < test_data[[var_0_01_col]]
  
  # Hit Test
  hit_rate_0_05 <- mean(hits_0_05, na.rm = TRUE)
  hit_rate_0_01 <- mean(hits_0_01, na.rm = TRUE)
  
  # Unconditional Coverage Test
  uc_test_0_05 <- binom.test(sum(hits_0_05, na.rm = TRUE), sum(!is.na(hits_0_05)), alpha_0.05)
  uc_test_0_01 <- binom.test(sum(hits_0_01, na.rm = TRUE), sum(!is.na(hits_0_01)), alpha_0.01)
  
  # Conditional Coverage Test
  cc_test_0_05 <- ccTest(test_data[[returns_col]], test_data[[var_0_05_col]], alpha_0.05)
  cc_test_0_01 <- ccTest(test_data[[returns_col]], test_data[[var_0_01_col]], alpha_0.01)
  
  # Expected Shortfall
  es_0_05 <- mean(test_data[[returns_col]][hits_0_05], na.rm = TRUE)
  es_0_01 <- mean(test_data[[returns_col]][hits_0_01], na.rm = TRUE)
  
  return(list(
    hit_rate_0_05 = hit_rate_0_05,
    hit_rate_0_01 = hit_rate_0_01,
    uc_test_0_05 = uc_test_0_05,
    uc_test_0_01 = uc_test_0_01,
    cc_test_0_05 = cc_test_0_05,
    cc_test_0_01 = cc_test_0_01,
    es_0_05 = es_0_05,
    es_0_01 = es_0_01
  ))
}

# Helper function for Conditional Coverage Test (Christoffersen test)
ccTest <- function(returns, var, alpha) {
  hits <- returns < var
  T00 <- sum(!hits[-length(hits)] & !hits[-1])
  T01 <- sum(!hits[-length(hits)] & hits[-1])
  T10 <- sum(hits[-length(hits)] & !hits[-1])
  T11 <- sum(hits[-length(hits)] & hits[-1])
  
  p0 <- (T01 + T00) / (T00 + T10 + T01 + T11)
  p1 <- T11 / (T01 + T11)
  
  LRuc <- -2 * log(((1 - alpha)^(T00 + T10) * alpha^(T01 + T11)) / ((1 - p0)^(T00 + T10) * p0^(T01 + T11)))
  LRind <- -2 * log(((1 - p0)^(T00 + T10) * p0^(T01 + T11)) / ((1 - p1)^(T00 + T10) * p1^(T01 + T11)))
  LRcc <- LRuc + LRind
  p_value <- 1 - pchisq(LRcc, df = 2)
  
  return(list(LRcc = LRcc, p_value = p_value))
}

# Example usage
combined_data <- combined_results_all[["AAPL.csv"]]
model <- "realised_garch"
horizon <- 1

results_Var <- compute_var_tests(combined_data, model, horizon)
print(results_Var)



####HAVEN'T DONE ANYTHING HERE####

### This script runs metrics and statistical tests ### 
dq <- DQtest(y = test_aapl, VaR_0.01, 0.99)

##### VaR Test Statistics and Tables #####
# Function to compute Hit Test, MSE, QLIKE, UC and CC Tests
compute_var_metrics <- function(returns, VaR, alpha = 0.01) {
  # Ensure returns and VaR are of the same length
  n <- length(returns)
  if (length(VaR) != n) stop("Returns and VaR must be of the same length.")
  
  # Hit Test
  H <- ifelse(returns < VaR, 1, 0)
  
  # Expected Shortfall (ES)
  ES <- sum(returns[returns < -VaR]) / sum(H)
  
  # UC Test
  VaR_test <- VaRTest(alpha, returns, VaR, 1 - alpha)
  UC_LRstat <- VaR_test$uc.LRstat
  UC_critical <- VaR_test$uc.critical
  UC_LRp <- VaR_test$uc.LRp
  UC_Decision <- VaR_test$uc.Decision
  
  # CC Test
  CC_LRstat <- VaR_test$cc.LRstat
  CC_critical <- VaR_test$cc.critical
  CC_LRp <- VaR_test$cc.LRp
  CC_Decision <- VaR_test$cc.Decision
  
  # Violation Rate
  VRate <- mean(H)
  
  return(list(
    H = H,
    Expected_Shortfall = ES,
    VRate = VRate,
    UC_LRstat = UC_LRstat,
    UC_critical = UC_critical,
    UC_LRp = UC_LRp,
    UC_Decision = UC_Decision,
    CC_LRstat = CC_LRstat,
    CC_critical = CC_critical,
    CC_LRp = CC_LRp,
    CC_Decision = CC_Decision
  ))
}

# Example Usage
# Calculate VaR for 1-day horizon at 1% level for both in-sample and out-of-sample
params <- all_results$AAPL.csv$fitted_models$realised_har_garch$optimal_params
forecast_horizon <- 1
alpha_0.01 <- 0.01 
har_garch_var_forecast <- har_garch_var_forecast(params, "realised_har_garch", forecast_horizon, all_results$AAPL.csv, train_aapl, test_aapl, alpha_0.05, alpha_0.01, df = estimated_df)

# Combine in-sample and out-of-sample VaR
VaR_full_0.01 <- c(in_sample_var_results$VaR_0.01, har_garch_var_forecast$VaR_0.01)

# Combine in-sample and out-of-sample returns
returns_full <- c(train_aapl$returns, test_aapl$returns)

# Compute metrics
metrics <- compute_var_metrics(aapl_data$returns, combined_data_var_full$VaR_0.01, 0.01)
print(metrics)
# Print the results
print(metrics$H)
print(metrics$MSE)
print(metrics$QLIKE)
print(metrics$VRate)
print(metrics$UC_LRstat)
print(metrics$UC_critical)
print(metrics$UC_LRp)
print(metrics$UC_Decision)
print(metrics$CC_LRstat)
print(metrics$CC_critical)
print(metrics$CC_LRp)
print(metrics$CC_Decision)








xts_var <- xts(combined_data_var_full$VaR_0.01, order.by = as.Date(merge_aapl))
xts_actual <- xts(c(train_aapl$returns, test_aapl$returns), order.by = as.Date(merge_aapl)) 

merge_aapl <- c(train_aapl$Day, test_aapl$Day)

test <- VaRplot(0.01,xts_actual,xts_var , title = paste("test"),ylab = "Daily Log Returns", 
                xlab = "Time")


VaR_test <- VaRTest(0.01, aapl_data$returns, combined_data_var_full$VaR_0.01, 0.99)
VaR_test$actual.exceed
VaR_test$expected.exceed

VaR_test$uc.LRstat
VaR_test$uc.critical
VaR_test$uc.LRp
VaR_test$uc.Decision

VaR_test$cc.LRstat
VaR_test$cc.critical
VaR_test$cc.LRp
VaR_test$cc.Decision

