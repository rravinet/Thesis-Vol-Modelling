##This script calculate the results for each dataset and put it into a table###
# Load necessary libraries
library(dplyr)
library(readr)
library(MCS)
install.packages('MCS')


testing <- read.csv('/Users/raphaelravinet/Code/BSE/Thesis/KAI_folder/Forecast_results/AAPL.csv/realised_garch_horizon_5.csv')

# Path to your forecast results directory
forecast_dir <- "/Users/raphaelravinet/Code/BSE/Thesis/KAI_folder/Forecast_results_final/"

QLIKE <- function(actual, forecast) {
  term_1 <- actual / forecast
  term_2 <- - log((actual / forecast))
  QLIKE <- sum(term_1 + term_2 - 1)
  return(QLIKE)
}

# Function to calculate MSE, MAE, and RMSE
calculate_metrics <- function(actual, forecast) {
  mse <- mse(actual, forecast)
  qlike <- QLIKE(actual,forecast)
  rmse <- rmse(actual, forecast)
  return(data.frame(MSE = mse, QLIKE = qlike, RMSE = rmse))
}

write.csv(results_wide, "/Users/raphaelravinet/Code/BSE/Thesis/Results/results.csv", row.names = FALSE)

head(rg_pred_h10_aapl,11)


mse(all_results$AAPL.csv$test_data$log_x_adj[10:(length(all_results$AAPL.csv$test_data$log_x_adj))],rg_pred_h10_aapl$rolling_forecasts[10:(length(all_results$AAPL.csv$test_data$log_x_adj))])
length(rg_pred_h10_aapl$rolling_forecasts)
rg_pred_h10_aapl$rolling_forecasts[10:(length(all_results$AAPL.csv$test_data$log_x_adj))]


actual_ap <- all_results$AAPL.csv$test_data$log_x_adj
actual_h5 <- actual_ap[1:(length(actual_ap) - (5 - 1))]
h1_pred <- calculate_metrics(actual_ap, testing$AAPL.csv$realised_har_iv_garchhorizon1)
h5_pred <- calculate_metrics(actual_h5, testing$AAPL.csv$realised_har_iv_garchhorizon5)
print(h1_pred)
print(h5_pred)
all_results$AAPL.csv$test_data$log_x_adj
##USING THIS ONE###
results_df <- data.frame()

# Loop through each stock and each model to calculate metrics
for (stock in names(all_results)) {
  actual <- all_results[[stock]]$test_data$log_x_adj
  for (model in c("realised_garch_horizon_", "realised_har_garch_horizon_")) {
    for (horizon in c(1, 5, 10, 20)) {
      model_name <- paste0(model, horizon)
      forecast_file <- file.path(forecast_dir, stock, paste0(model_name, ".csv"))
      if (file.exists(forecast_file)) {
        forecast_data <- read_csv(forecast_file)
        forecast <- forecast_data$forecast_data
        # Adjust actual data length based on horizon
        actual_adjusted <- actual[1:(length(actual) - (horizon - 1))]
        # Ensure the forecast length matches the adjusted actual length
        forecast <- forecast[1:length(actual_adjusted)]
        metrics <- calculate_metrics(actual_adjusted, forecast)
        metrics$Stock <- stock
        metrics$Model <- model_name
        results_df <- rbind(results_df, metrics)
      }
    }
  }
}

# Convert Model column to separate columns for each horizon and metric
results_df <- results_df %>%
  separate(Model, into = c("Model", "Horizon"), sep = "_horizon_") %>%
  unite("Metric_Model_Horizon", Model, Horizon, sep = "_horizon_") %>%
  pivot_wider(names_from = Metric_Model_Horizon, values_from = c(MSE, QLIKE, RMSE))

# Ensure all columns are atomic vectors
results_wide <- data.frame(lapply(results_df, as.vector))


write.csv(results_wide, "/Users/raphaelravinet/Code/BSE/Thesis/Results/results.csv", row.names = FALSE)
