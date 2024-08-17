##This script performs forecasts###

# Define the standardised Student's t-distribution function
standardised_student_t <- function(nu, gamma) {
  student.t(nu = nu, gamma = gamma, mu = 0, sigma = 1)
}


######################################################################################################
### IN-SAMPLE: 1-day horizon VaR for 5% and 1% Levels ###
######################################################################################################
# Define in-sample VaR calculation function
in_sample_all_models_VaR <- function(model, params, forecast_horizon, results, alpha_0.05 = 0.05, alpha_0.01 = 0.01) {
  # Extracting fitted values
  sigma2 <- results$fitted_models[[model]]$fitted_values$sigma2
  z <- results$fitted_models[[model]]$fitted_values$z
  
  #VaR Setup:
  fit <- fit.ghypuv(data = z, symmetric = FALSE, lambda = -0.5)
  df <- fit@chi
  skew <- fit@gamma[1]
  
  # Initialize a list to store in-sample VaR
  in_sample_var_0.05 <- numeric(length(sigma2))
  in_sample_var_0.01 <- numeric(length(sigma2))
  
  # Calculate VaR for each day in the training data
  for (t in 1:length(sigma2)) {
    sigma_t <- sqrt(sigma2[t])
    VaR_dist <- standardised_student_t(df, skew)
    
    in_sample_var_0.05[t] <- - qghyp(1 - alpha_0.05, VaR_dist) * sigma_t * sqrt(forecast_horizon)
    in_sample_var_0.01[t] <- - qghyp(1 - alpha_0.01, VaR_dist) * sigma_t * sqrt(forecast_horizon)
  }
  
  return(list(VaR_0.05 = in_sample_var_0.05, VaR_0.01 = in_sample_var_0.01))
}

######################################################################################################
### Out of Sample VaR Forecasts (1-day ahead) Function ###
######################################################################################################
realised_garch_var_forecast <- function(params, model, forecast_horizon, results, train_data, test_data, alpha_0.05 = 0.05, alpha_0.01 = 0.01) {
  # Extracting fitted values
  z <- results$fitted_models[[model]]$fitted_values$z
  ut <- results$fitted_models[[model]]$fitted_values$ut
  log_sigma <- results$fitted_models[[model]]$fitted_values$log_sigma2
  
  #VaR Setup:
  fit <- fit.ghypuv(data = z, symmetric = FALSE, lambda = -0.5)
  df <- fit@chi
  skew <- fit@gamma[1]
  
  # Getting our parameters
  omega <- params[1]
  beta <- params[2]
  gamma <- params[3]
  xi <- params[4]
  phi <- params[5]
  tau1 <- params[6]
  tau2 <- params[7]
  sigma_ut <- params[8]
  
  # Calculating mu_x
  mu_x <- phi * omega + xi * (1 - beta)
  
  # Initialise a list to store forecasts
  rolling_forecasts <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_VaR_0.05 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_VaR_0.01 <- numeric(nrow(test_data) - forecast_horizon + 1)
  
  # Perform rolling forecast
  for (start in 1:(nrow(test_data) - forecast_horizon + 1)) { 
    end <- start + forecast_horizon - 1
    
    # Use the training data up to the current point
    current_train_data <- rbind(train_data, test_data[1:(start-1), ])
    log_sigma_t <- log_sigma[length(log_sigma)]
    log_x_t <- current_train_data$log_x[nrow(current_train_data)]
    z_t_prev <- z[length(z)]
    u_t_prev <- ut[length(ut)]
    epsilon_t_prev <- tau1 * z_t_prev + tau2 * (z_t_prev^2 - 1) + u_t_prev
    
    # Combining z and ut so we can simulate the values
    Wt <- cbind(z, ut)
    
    for (m in 1:forecast_horizon) {
      # Drawing 5000 random pairs
      sim_pairs <- Wt[sample(nrow(Wt), 5000, replace = TRUE), ]
      
      simulated_log_x <- numeric(5000)
      simulated_log_sigma <- numeric(5000)
      
      # Getting epsilon and log_x for each simulation pair
      for (i in 1:5000) {
        z_i <- sim_pairs[i, 1]
        u_i <- sim_pairs[i, 2]
        
        # Compute epsilon_t
        epsilon_t <- tau1 * z_i + tau2 * (z_i^2 - 1) + u_i
        
        # Update log_x using the measurement equation
        log_x_i <- mu_x + (beta + phi * gamma) * log_x_t + epsilon_t - beta * epsilon_t_prev
        simulated_log_x[i] <- log_x_i
        
        # Calculate log_sigma
        log_sigma_i <- omega + beta * log_sigma_t + gamma * log_x_i
        simulated_log_sigma[i] <- log_sigma_i
      }
      
      # Calculate the mean of the simulated log_sigma values
      log_sigma_t <- mean(simulated_log_sigma)
      log_x_t <- mean(simulated_log_x)
      
      # Update epsilon_t_prev for the next iteration using the mean of the current iteration's values
      epsilon_t_prev <- mean(tau1 * sim_pairs[, 1] + tau2 * (sim_pairs[, 1]^2 - 1) + sim_pairs[, 2])
      
      #Get a sample of skewed dist
      VaR_dist <- standardised_student_t(df, skew)
      sample_t_5 <- - qghyp(1 - alpha_0.05, VaR_dist)
      sample_t_1 <- - qghyp(1 - alpha_0.01, VaR_dist)
    }
    
    # Store the forecast
    rolling_forecasts[start + forecast_horizon - 1] <- log_sigma_t
    
    # Calculate VaR using standardised skewed student's t-distribution
    sigma_t <- sqrt(exp(log_sigma_t))
    dist_5 <-mean(sample_t_5)
    dist_1 <- mean(sample_t_1)
    
    rolling_VaR_0.05[start] <- dist_5 * sigma_t * sqrt(forecast_horizon)
    rolling_VaR_0.01[start] <- dist_1 * sigma_t * sqrt(forecast_horizon)
  }
  
  return(list(rolling_forecasts = rolling_forecasts, VaR_0.05 = rolling_VaR_0.05, VaR_0.01 = rolling_VaR_0.01))
}

har_garch_var_forecast <- function(params, model, forecast_horizon, results, train_data, test_data, alpha_0.05 = 0.05, alpha_0.01 = 0.01) {
  # Extracting fitted values
  z <- results$fitted_models[[model]]$fitted_values$z
  ut <- results$fitted_models[[model]]$fitted_values$ut
  log_sigma <- results$fitted_models[[model]]$fitted_values$log_sigma2
  
  #VaR Setup:
  fit <- fit.ghypuv(data = z, symmetric = FALSE, lambda = -0.5)
  df <- fit@chi
  skew <- fit@gamma[1]
  
  # Getting our parameters
  omega <- params[1]
  beta <- params[2]
  gamma_d <- params[3]
  gamma_w <- params[4]
  gamma_m <- params[5]
  xi <- params[6]
  phi <- params[7]
  tau1 <- params[8]
  tau2 <- params[9]
  sigma_ut <- params[10]
  
  # Calculating mu_x
  mu_x <- phi * omega + xi * (1 - beta)
  
  # Initialise a list to store forecasts
  rolling_forecasts <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_VaR_0.05 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_VaR_0.01 <- numeric(nrow(test_data) - forecast_horizon + 1)
  
  # Buffers to store past RM_day values
  rm_history <- as.list(train_data$RM_day)
  
  # Perform rolling forecast
  for (start in 1:(nrow(test_data) - forecast_horizon + 1)) { 
    end <- start + forecast_horizon - 1
    
    # Use the training data up to the current point
    current_train_data <- rbind(train_data, test_data[1:(start-1), ])
    log_sigma_t <- log_sigma[length(log_sigma)]
    RM_day <- current_train_data$RM_day[nrow(current_train_data)]
    z_t_prev <- z[length(z)]
    u_t_prev <- ut[length(ut)]
    epsilon_t_prev <- tau1 * z_t_prev + tau2 * (z_t_prev^2 - 1) + u_t_prev
    
    # Update RM_week and RM_month
    rm_history <- append(rm_history, list(RM_day))
    # Calculate RM_week
    RM_week <- mean(unlist(rm_history[(length(rm_history)-5):(length(rm_history)-2)]))
    # Calculate RM_month
    RM_month <- mean(unlist(rm_history[(length(rm_history)-22):(length(rm_history)-6)]))
    
    # Combining z and ut so we can simulate the values
    Wt <- cbind(z, ut)
    
    for (m in 1:forecast_horizon) {
      # Drawing 5000 random pairs
      sim_pairs <- Wt[sample(nrow(Wt), 5000, replace = TRUE), ]
      
      simulated_log_x <- numeric(5000)
      simulated_log_sigma <- numeric(5000)
      
      # Getting epsilon and log_x for each simulation pair
      for (i in 1:5000) {
        z_i <- sim_pairs[i, 1]
        u_i <- sim_pairs[i, 2]
        
        # Compute epsilon_t
        epsilon_t <- tau1 * z_i + tau2 * (z_i^2 - 1) + u_i
        
        # Update log_x using the measurement equation
        log_x_i <- mu_x + (beta + phi * gamma_d) * RM_day + (phi * gamma_w * RM_week) + (phi * gamma_m * RM_month) + epsilon_t - beta * epsilon_t_prev
        simulated_log_x[i] <- log_x_i
        
        # Calculate log_sigma
        log_sigma_i <- omega + beta * log_sigma_t + gamma_d * RM_day + gamma_w * RM_week + gamma_m * RM_month
        simulated_log_sigma[i] <- log_sigma_i
      }
      
      # Calculate the mean of the simulated log_sigma values
      log_sigma_t <- mean(simulated_log_sigma)
      RM_day <- mean(simulated_log_x)
      
      # Update RM_week and RM_month dynamically for the next step
      rm_history <- append(rm_history, list(RM_day))
      
      RM_week <- mean(unlist(rm_history[(length(rm_history)-5):(length(rm_history)-2)]))
      RM_month <- mean(unlist(rm_history[(length(rm_history)-22):(length(rm_history)-6)]))
      
      # Update epsilon_t_prev for the next iteration using the mean of the current iteration's values
      epsilon_t_prev <- mean(tau1 * sim_pairs[, 1] + tau2 * (sim_pairs[, 1]^2 - 1) + sim_pairs[, 2])
    }
    
    # Store the forecast
    rolling_forecasts[start + forecast_horizon - 1] <- log_sigma_t
    
    # Calculate VaR using standardised skewed student's t-distribution
    sigma_t <- sqrt(exp(log_sigma_t))
    VaR_dist <- standardised_student_t(df, skew)
    
    rolling_VaR_0.05[start] <- - qghyp(1 - alpha_0.05, VaR_dist) * sigma_t * sqrt(forecast_horizon)
    rolling_VaR_0.01[start] <- - qghyp(1 - alpha_0.01, VaR_dist) * sigma_t * sqrt(forecast_horizon)
  }
  
  return(list(rolling_forecasts = rolling_forecasts, VaR_0.05 = rolling_VaR_0.05, VaR_0.01 = rolling_VaR_0.01))
}

train_aapl <- all_results$AAPL.csv$train_data
test_aapl <- all_results$AAPL.csv$test_data
realised_garch_var_forecast <- realised_garch_var_forecast(params, "realised_garch", forecast_horizon, all_results$AAPL.csv, train_aapl, test_aapl, alpha_0.05, alpha_0.01)
realised_garch_var_forecast$VaR_0.05

######################################################################################################
### FORECAST for Realised-GARCH: 1-day horizon VaR for 5% and 1% Levels ###
######################################################################################################
params <- all_results$AAPL.csv$fitted_models$realised_garch$optimal_params
forecast_horizon <- 1  # 1-day ahead forecast
alpha_0.05 <- 0.05
alpha_0.01 <- 0.01 
realised_garch_var_forecast <- realised_garch_var_forecast(params, "realised_garch", forecast_horizon, all_results$AAPL.csv, train_aapl, test_aapl, alpha_0.05, alpha_0.01)
realised_garch_var_forecast$rolling_forecasts
# Extracting log_sigma forecasts and VaR forecasts
rolling_forecasts <- realised_garch_var_forecast$rolling_forecasts
VaR_0.05 <- -realised_garch_var_forecast$VaR_0.05
VaR_0.01 <- -realised_garch_var_forecast$VaR_0.01

### Plotting VaR: ###
# Combine training data, test data, and VaR for plotting
aapl_data_len <- nrow(aapl_data) - (forecast_horizon - 1)


######################################################################################################
### FORECAST for HAR-GARCH: 1-day horizon VaR for 5% and 1% Levels ###
######################################################################################################
params <- all_results$AAPL.csv$fitted_models$realised_har_garch$optimal_params
forecast_horizon <- 1  # 1-day ahead forecast
alpha_0.05 <- 0.05
alpha_0.01 <- 0.01 
har_garch_var_forecast <- har_garch_var_forecast(params, "realised_har_garch", forecast_horizon, all_results$AAPL.csv, train_aapl, test_aapl, alpha_0.05, alpha_0.01)

# Extracting forecasts and VaR
rolling_forecasts <- har_garch_var_forecast$rolling_forecasts

VaR_0.05 <- -har_garch_var_forecast$VaR_0.05
#VaR_0.05 <- VaR_0.05[VaR_0.05 != 0]
VaR_0.01 <- -har_garch_var_forecast$VaR_0.01
#VaR_0.01 <- VaR_0.01[VaR_0.01 != 0]

### Plotting VaR: ###
# Combine training data, test data, and VaR for plotting
aapl_data_len <- nrow(aapl_data) - (forecast_horizon - 1)
train_data_len <- nrow(train_aapl)
# combined_data_var_1day <- data.frame(
#   Day = 1:(train_data_len + test_data_len),
#   Log_x = c(exp(train_aapl$log_x), exp(test_aapl$log_x)),
#   Type = rep(c("Train", "Test"), times = c(train_data_len, test_data_len))
# )

# # Add 1-day VaR (5% level) for test period
# combined_data_var_1day$VaR_0.05 <- NA
# combined_data_var_1day$VaR_0.05[(train_data_len + forecast_horizon):(train_data_len + forecast_horizon + length(VaR_0.05) - 1)] <- VaR_0.05
# 
# # Add 1-day VaR (1% level) for test period
# combined_data_var_1day$VaR_0.01 <- NA
# combined_data_var_1day$VaR_0.01[(train_data_len + forecast_horizon):(train_data_len + forecast_horizon + length(VaR_0.01) - 1)] <- VaR_0.01


######################################################################################################
### IN-SAMPLE: 1-day horizon VaR for 5% and 1% Levels ###
######################################################################################################
# Define in-sample VaR calculation function
in_sample_all_models_VaR <- function(model, params, forecast_horizon, results, alpha_0.05 = 0.05, alpha_0.01 = 0.01) {
  # Extracting fitted values
  sigma2 <- results$fitted_models[[model]]$fitted_values$sigma2
  z <- results$fitted_models[[model]]$fitted_values$z
  
  #VaR Setup:
  fit <- fit.ghypuv(data = z, symmetric = FALSE, lambda = -0.5)
  df <- fit@chi
  skew <- fit@gamma[1]
  
  # Initialize a list to store in-sample VaR
  in_sample_var_0.05 <- numeric(length(sigma2))
  in_sample_var_0.01 <- numeric(length(sigma2))
  
  # Calculate VaR for each day in the training data
  for (t in 1:length(sigma2)) {
    sigma_t <- sqrt(sigma2[t])
    VaR_dist <- standardised_student_t(df, skew)
    
    in_sample_var_0.05[t] <- - qghyp(1 - alpha_0.05, VaR_dist) * sigma_t * sqrt(forecast_horizon)
    in_sample_var_0.01[t] <- - qghyp(1 - alpha_0.01, VaR_dist) * sigma_t * sqrt(forecast_horizon)
  }
  
  return(list(VaR_0.05 = in_sample_var_0.05, VaR_0.01 = in_sample_var_0.01))
}

##########################################
### Realised-GARCH: Estimate in-sample VaR
##########################################
in_sample_var_results <- in_sample_all_models_VaR("realised_garch", params, forecast_horizon, all_results$AAPL.csv, alpha_0.05, alpha_0.01)

# Combine in-sample and out-of-sample VaR
VaR_full_0.05 <- c(in_sample_var_results$VaR_0.05, realised_garch_var_forecast$VaR_0.05)
VaR_full_0.01 <- c(in_sample_var_results$VaR_0.01, realised_garch_var_forecast$VaR_0.01)
in_sample_var_results$VaR_0.05



#########################################
### HAR-GARCH: Estimate in-sample VaR
#########################################
in_sample_var_results <- in_sample_all_models_VaR("realised_har_garch", params, forecast_horizon, all_results$AAPL.csv, alpha_0.05, alpha_0.01)

# Combine in-sample and out-of-sample VaR
VaR_full_0.05 <- c(in_sample_var_results$VaR_0.05, har_garch_var_forecast$VaR_0.05)
VaR_full_0.01 <- c(in_sample_var_results$VaR_0.01, har_garch_var_forecast$VaR_0.01)

# VaR_full_0.05 <- VaR_full_0.05[VaR_full_0.05 != 0]
# VaR_full_0.01 <- VaR_full_0.01[VaR_full_0.01 != 0]

aapl_data$sum_returns <- numeric(length(aapl_data$returns))

# Loop through the train_aapl$returns and calculate the rolling sum
for (i in 1:(length(aapl_data$returns))) {
  aapl_data$sum_returns[i] <- sum(aapl_data$returns[i:(i + 9)])
}

#### PLOT the above
# Combine training data, test data, VaR, and daily returns for plotting

##### USE THIS FOR 1-day FORECASTS #####
combined_data_var_full <- data.frame(
  Day = 1:aapl_data_len - (forecast_horizon - 1),
  Log_x = aapl_data$log_x,
  Returns = aapl_data$returns
)


##### USE THIS FOR 10-day FORECASTS #####
combined_data_var_full <- data.frame(
  Day = 1:aapl_data_len - (forecast_horizon - 1),
  Log_x = head(aapl_data$log_x, - (forecast_horizon - 1)),
  Returns = head(aapl_data$returns, - (forecast_horizon - 1))#,
  #Type = rep(c("Train", "Test"), times = aapl_data_len)
)
# Add VaR for the full period
combined_data_var_full$VaR_0.05 <- VaR_full_0.05
combined_data_var_full$VaR_0.01 <- VaR_full_0.01
#combined_data_var_full$VaR_10day <- VaR_10day_full

# Plot VaR and daily returns
p_var_full <- ggplot(combined_data_var_full, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = VaR_0.05, color = "VaR 1-day 5%"), na.rm = TRUE, linetype = "dotted") +
  geom_line(aes(y = VaR_0.01, color = "VaR 1-day 1%"), na.rm = TRUE, linetype = "dotted") +
  #geom_line(aes(y = VaR_10day, color = "VaR 10-day"), na.rm = TRUE, linetype = "dashed") +
  geom_vline(xintercept = train_data_len, linetype = "dashed", color = "black") +
  labs(title = "VaR: 1-Day Forecasts at 5% and 1% Level with Daily Returns", x = "Day", y = "Value") +
  scale_color_manual(values = c("Daily Returns" = "black", "VaR 1-day 5%" = "purple", "VaR 1-day 1%" = "orange")) +
  scale_y_continuous(
    limits = c(-1, 1),  # Adjust these limits based on your data range
    #breaks = seq(-2, 2, by = 0.5),  # Set breaks at 0.5 intervals
    #labels = function(x) paste0(x, "%")  # Add percentage sign to labels
  ) +
  theme_minimal()

print(p_var_full)





























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

