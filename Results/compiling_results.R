##This script calculate the results for each dataset and put it into a table###



# calculate_mse <- function(actual, fitted) {
#   mse(actual - mean(actual), fitted - mean(fitted))
# }

calculate_metrics <- function(actual, predicted){
  mse <- mse(actual - mean(actual), predicted - mean(predicted))
  rmse <- rmse(actual - mean(actual), predicted - mean(predicted))
  qlike <- mean((actual / predicted) - log(actual / predicted) - 1)
  return(list(mse = mse, rmse = rmse, qlike = qlike))
}

compile_results_table <- function(all_results, all_forecasts, model_types, forecast_horizon) {
  compiled_results <- list()
  
  for (dataset_name in names(all_results)) {
    train_data <- all_results[[dataset_name]]$train_data
    test_data <- all_results[[dataset_name]]$test_data
    
    dataset_results <- data.frame(Model = character(),
                                  Dataset = character(),
                                  Metric = character(),
                                  Value = numeric(),
                                  stringsAsFactors = FALSE)
    
    for (model_type in model_types) {
      fitted_values <- all_results[[dataset_name]]$fitted_models[[model_type]]$fitted_values
      in_sample_metrics <- calculate_metrics(train_data$log_x_adj, fitted_values$log_sigma2)
      
      for (metric_name in names(in_sample_metrics)) {
        dataset_results <- rbind(dataset_results, 
                                 data.frame(Model = model_type,
                                            Dataset = dataset_name,
                                            Metric = paste0("In-sample ", metric_name),
                                            Value = in_sample_metrics[[metric_name]]))
      }
      
      # Out-of-sample metrics
      if (model_type %in% names(all_forecasts[[dataset_name]])) {
        forecasted_values <- all_forecasts[[dataset_name]][[model_type]][[rolling_forecasts]]
        out_sample_metrics <- calculate_metrics(test_data$log_x_adj, forecasted_values)
        
        for (metric_name in names(out_sample_metrics)) {
          dataset_results <- rbind(dataset_results, 
                                   data.frame(Model = model_type,
                                              Dataset = dataset_name,
                                              Metric = paste0("Out-of-sample ", metric_name),
                                              Value = out_sample_metrics[[metric_name]]))
        }
      }
    }
    
    compiled_results[[dataset_name]] <- dataset_results
  }
  
  final_results <- do.call(rbind, compiled_results)
  
  return(final_results)
}
all_forecasts$AAPL$realised_har_garch$
b <- all_results$AAPL.csv$train_data

test <-  all_results$AAPL.csv$train_data$log_x_adj
test1 <- all_results$AAPL.csv$fitted_models$realised_har_garch$fitted_values$log_sigma2
in_sample_metrics <- calculate_metrics(all_results$AAPL.csv$train_data$log_x_adj, all_results$AAPL.csv$fitted_models$realised_har_garch$fitted_values$log_sigma2)
in_sample_metrics
out_sample_metrics <- calculate_metrics(all_results$AAPL.csv$test_data$log_x_adj, all_forecasts$AAPL$realised_har_garch$rolling_forecasts)
out_sample_metrics
# Example usage of the function
compiled_results <- compile_results_table(all_results, all_forecasts, model_types, forecast_horizon)
##### testing ####

compile_results_table <- function(all_results, all_forecasts, model_types, forecast_horizon) {
  compiled_results <- data.frame(Model = character(),
                                 Dataset = character(),
                                 Metric = character(),
                                 Value = numeric(),
                                 stringsAsFactors = FALSE)
  
  for (dataset_name in names(all_results)) {
    train_data <- all_results[[dataset_name]]$train_data
    test_data <- all_results[[dataset_name]]$test_data
    
    # Loop through each model type
    for (model_type in model_types) {
      fitted_values <- all_results[[dataset_name]]$fitted_models[[model_type]]$fitted_values
      in_sample_metrics <- calculate_metrics(train_data$log_x_adj, fitted_values$log_sigma2)
      
      # Add in-sample metrics to the results
      for (metric_name in names(in_sample_metrics)) {
        compiled_results <- rbind(compiled_results, 
                                  data.frame(Model = model_type,
                                             Dataset = dataset_name,
                                             Metric = paste0("In-sample ", metric_name),
                                             Value = in_sample_metrics[[metric_name]]))
      }
      
      # Out-of-sample metrics
      if (model_type %in% names(all_forecasts[[dataset_name]])) {
        forecasted_values <- all_forecasts[[dataset_name]][[model_type]][["rolling_forecasts"]]
        out_sample_metrics <- calculate_metrics(test_data$log_x_adj, forecasted_values)
        
        # Add out-of-sample metrics to the results
        for (metric_name in names(out_sample_metrics)) {
          compiled_results <- rbind(compiled_results, 
                                    data.frame(Model = model_type,
                                               Dataset = dataset_name,
                                               Metric = paste0("Out-of-sample ", metric_name),
                                               Value = out_sample_metrics[[metric_name]]))
        }
      }
    }
  }
  
  return(compiled_results)
}
