##Fitting the models to our datasets ###
data_dir <- "/Users/raphaelravinet/Code/BSE/Thesis/final_datasets"
# Function to apply models to all datasets with train-test split
apply_models_to_all_datasets <- function(data_dir, model_types, param_names) {
  file_list <- list.files(data_dir, full.names = TRUE)
  all_results <- list()
  
  for (file in file_list) {
    data <- read.csv(file)  # Assuming the datasets are in CSV format
    
    # Split the data into train (first 80%) and test (remaining 20%) sets
    split_index <- floor(0.8 * nrow(data))
    train_data <- data[1:split_index, ]
    test_data <- data[(split_index + 1):nrow(data), ]
    
    results <- fitted_models(train_data, model_types, param_names)
    results$train_data <- train_data
    results$test_data <- test_data
    all_results[[basename(file)]] <- results
  }
  
  return(all_results = all_results)
}

# Running the function
all_results <- apply_models_to_all_datasets(data_dir, model_types, param_names)


##FORECASTING###
apply_forecasts_to_all_datasets <- function(results, model_types, forecast_horizon) {
  all_forecasts <- list()
  
  for (dataset_name in names(results)) {
    print(paste("Processing dataset:", dataset_name))
    train_data <- results[[dataset_name]]$train_data
    test_data <- results[[dataset_name]]$test_data
    dataset_forecasts <- list()
    
    for (model_type in model_types) {
      params <- results[[dataset_name]]$fitted_models[[model_type]]$optimal_params
    
      if (model_type == "realised_har_garch") {
        forecasted_values <- multi_period_har_garch_forecast(params, model_type, forecast_horizon, results[[dataset_name]], train_data, test_data)
      } else if (model_type == "realised_garch") {
        forecasted_values <- multi_period_realised_garch_forecast(params, model_type, forecast_horizon, results[[dataset_name]], train_data, test_data)
      } else {
        next
      }
      
      dataset_forecasts[[model_type]] <- forecasted_values
    }
    
    all_forecasts[[dataset_name]] <- dataset_forecasts
  }
  
  return(all_forecasts)
}

# Assuming all_results, model_types, and forecast_horizon are defined somewhere in your environment
all_forecasts <- apply_forecasts_to_all_datasets(all_results, model_types, forecast_horizon)
forecast_horizon <- 5