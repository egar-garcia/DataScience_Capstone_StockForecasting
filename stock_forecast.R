#-----------------------------------
# Required packages
#-----------------------------------

# Installing that required packages that are not jet installed
if(!require(tidyverse)) install.packages('tidyverse')
if(!require(httr)) install.packages('httr')
if(!require(jsonlite)) install.packages('jsonlite')
if(!require(ggcorrplot)) install.packages('ggcorrplot')
if(!require(cowplot)) install.packages('cowplot')
if(!require(directlabels)) install.packages('directlabels')
if(!require(caret)) install.packages('caret')
if(!require(mgcv)) install.packages('mgcv')
if(!require(kernlab)) install.packages('kernlab')
if(!require(nlme)) install.packages('nlme')
if(!require(forecast)) install.packages('forecast')
if(!require(prophet)) install.packages('prophet')
if(!require(keras)) install.packages('keras')


# Loading the required packages
library(tidyr)
library(tidyverse)
library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(ggcorrplot)
library(cowplot)
library(directlabels)
library(httr)
library(jsonlite)
library(kernlab)
library(mgcv)
library(caret)
library(nlme)
library(forecast)
library(prophet)
library(keras)

# Install Keras if it has not been installed before
install_keras()



#-----------------------------------
# Dataset management
#-----------------------------------

#' Loading the set of ticker symbols of the companies contemplated in
#' the  Dow Jones Industrial Average.
dow_jones_stocks <- read.csv('dow_jones_stocks.csv', stringsAsFactors = FALSE)

#' This is the symbol used by the actual average, i.e. the Dow Jones Industrial Average.
djia_symbol <- 'DIA'

#' This is the template to create the URL to extract historical stock prices
#' from the IEX API.
iex_api_url_template <- 'https://api.iextrading.com/1.0/stock/%s/chart/%s'

#' Retrieves the historic prices for a particuler stock from the data source.
#'
#' @param ticker_symbol The ticker symbol or symbols to filter the data.
#' @param hist_period   The period to retrieve historical records,
#'    p.e '5y' for 5 years, '1y' for 1 year, '1m' for 1 month, etc.
#' @return The dataframe containing the historic prices.
get_historical_records <- function(ticker_symbol, hist_period = '5y') {
  # Getting the historic records from IEX into a dataframe
  call_url <- sprintf(iex_api_url_template, ticker_symbol, hist_period)
  resp <- GET(call_url)
  historical_prices <- as.data.frame(fromJSON(content(resp, 'text'), flatten = TRUE))

  # Adding the ticker symbol as a column
  historical_prices['symbol'] = ticker_symbol
  # Converting the date column to a Date type
  historical_prices$date <- as.Date(historical_prices$date, format='%Y-%m-%d')

  # Only chosing the columns: symbol, date, close
  historical_prices %>% select(symbol, date, close)
}

#' Gets a dataframe containing historic prices for stocks in
#' the Dow Jones Industrial Average.
#'
#' @param hist_period The period to retrieve historical records,
#'    p.e '5y' for 5 years, '1y' for 1 year, '1m' for 1 month, etc.
#' @return The dataframe containing the historic prices.
get_dow_jones_dataframe <- function(hist_period = '5y') {
  # Getting the historic records of the Dow Jones Industrial Average
  historical_prices <- get_historical_records(djia_symbol, hist_period = hist_period)

  # Retrieves the historic records for each one of the ticker symbols in the
  # Dow Jones Industrial Average
  for (tickers_symbol in dow_jones_stocks$symbol) {
    historical_prices <- rbind(historical_prices,
                               get_historical_records(tickers_symbol,
                                                      hist_period = hist_period))
  }

  historical_prices
}

#' Updates a dataframe containing historic prices for stocks in
#' the Dow Jones Industrial Average,
#' by retrieving the most recent records from the information source.
#'
#' @param historical_prices The dataframe containing stock price historical records.
#' @return The dataframe containing the historic prices.
update_dow_jones_dataframe <- function(historical_prices) {
  # Getting the amount of days that need to be updated
  last_recorded_day = max(historical_prices$date)
  today = Sys.Date()
  days_to_update = difftime(today, last_recorded_day, units="days")

  # Deciding the historic period to request the source according
  # to the days that need to be updated
  hist_period = '5y'
  if (days_to_update < 1) {
    return(historical_prices)
  } else if (days_to_update < 28) {
    hist_period = '1m'
  } else if (days_to_update < 365) {
    hist_period = '1y'
  }

  # Getting the data frame containing the missing records
  last_historical_prices <- get_dow_jones_dataframe(hist_period = hist_period)

  # Adding the missing records and removing duplicates
  historical_prices <- rbind(historical_prices, last_historical_prices)
  historical_prices[!duplicated(historical_prices[c('symbol', 'date')]),]
}

#' Gets a dataframe containing a subset of the records of the current dataset,
#' which is obtained by filtering by a ticker symbol and/or a date range.
#' 
#' @param ticker_symbol The ticker symbol to filter the data.
#' @param from_date The minimum date to appear in the records of the subset.
#' @param to_date The maximum date to appear in the records of the subset.
#' @return The dataframe with the subset resulted of filtering the dataset.
filter_historical_records <- function(historical_prices, 
                                      ticker_symbol = NULL,
                                      start_date = NULL, end_date = NULL) {
  df <- historical_prices
  
  # Filtering by ticker symbol if exists
  if (!is.null(ticker_symbol)) {
    df <- df %>% filter(symbol == ticker_symbol)
  }
  # Filtering by start date if exists
  if (!is.null(start_date)) {
    df <- df %>% filter(date >= start_date)
  }
  # Filtering by end date if exists
  if (!is.null(end_date)) {
    df <- df %>% filter(date <= end_date)
  }
  
  df
}

#' The file's name where the dataset is stored.
dow_jones_dataframe_filename <- 'dow_jones_dataframe.Rda'

#--------------------------------
# Examples of dataset management
#--------------------------------

# Creating a new dataset
#dow_jones_historical_records <- get_dow_jones_dataframe()
#save(dow_jones_historical_records, file = dow_jones_dataframe_filename)

# Loading, updating and saving the dataset
#load(file = dow_jones_dataframe_filename)
#dow_jones_historical_records <- update_dow_jones_dataframe(dow_jones_historical_records)
#save(dow_jones_historical_records, file = dow_jones_dataframe_filename)

# Loading dataset
load(file = dow_jones_dataframe_filename)



#-----------------------------------
# Visualization
#-----------------------------------

# Plotting the historical prices of all the 30 companies in the Dow Jones
dow_jones_historical_records %>%
  filter(symbol != djia_symbol) %>%
  left_join(dow_jones_stocks, by = 'symbol') %>%
  select(date, close, symbol, company) %>%
  ggplot(aes(x = date, y = close, group = symbol,
             color = sprintf('%s (%s)', symbol, company))) +
  geom_line(size = 0.3) +
  labs(colour = '') +
  theme(legend.position = 'bottom') +
  geom_dl(aes(label = symbol), method = 'angled.boxes')

# Plotting the historical prices grouped by industry
dow_jones_historical_records %>%
  filter(symbol != djia_symbol) %>%
  left_join(dow_jones_stocks, by = 'symbol') %>%
  select(date, close, symbol, industry, company) %>%
  filter(symbol != djia_symbol) %>%
  ggplot(aes(x = date, y = close)) +
  geom_line(aes(group = symbol, color = symbol)) +
  geom_dl(aes(label = symbol, color = symbol), method = 'smart.grid') +
  facet_wrap(~industry, ncol = 4) +
  theme(legend.position = 'none')

# Plotting in grey the historical prices of all the 30 companies
# and contrasting them with the historical prices of the actual
# Dow Jones Industrial Average
dow_jones_historical_records %>%
  mutate(index = ifelse(symbol == djia_symbol, 'DJIA', 'Stock in Dow Jones'),
         # Hack alert: Prefixing the DJIA with 'zzz_' in order be plotted at the end
         symbol = ifelse(symbol == djia_symbol, paste('zzz_', djia_symbol), symbol)) %>%
  ggplot(aes(x = date, y = close, group = symbol, color = index, size = index)) +
  geom_line() +
  scale_color_manual(values = c('black', 'gray')) +
  scale_size_manual(values = c(0.5, 0.25)) +
  labs(colour = '') +
  theme(legend.position = 'bottom') +
  guides(size = FALSE)

# Plotting the correlations among the stoks and the DJIA as a matrix heat-map
dow_jones_historical_records %>%
  # Hack alert: Prefixing the DJIA with ' ' in order to place it firts
  mutate(symbol = ifelse(symbol != djia_symbol, symbol, paste(' ', djia_symbol))) %>%
  spread(symbol, close) %>% 
  select(-date) %>%
  cor(method = 'pearson', use = 'complete.obs') %>%
  ggcorrplot(lab = TRUE)

# Plotting the most, least and non correlated stocks against the DJIA
dow_jones_historical_records %>%
  filter(symbol == djia_symbol | symbol == 'CAT' | symbol == 'WBA' | symbol == 'UNH') %>%
  mutate(index = ifelse(symbol == djia_symbol, 'DJIA', 'Dow Jones stock')) %>%
  ggplot(aes(x = date, y = close, group = symbol, color = symbol, size = index)) +
  geom_line() +
  scale_color_manual(values = c(DIA='black', CAT='red', WBA='blue', UNH='gray')) +
  scale_size_manual(values = c(0.5, 0.25)) +
  labs(colour = '') +
  theme(legend.position = 'none') +
  geom_dl(aes(label = symbol, color = symbol), method = 'last.polygons')



#-----------------------------------
# Trading days retrieval
#-----------------------------------

#' Market holidays loaded from file 'market_holidays.csv' 
market_holidays <- read.csv('market_holidays.csv') %>%
  mutate(date = as.Date(date, format='%Y-%m-%d'))


#' Getting the trading days existing in a date range.
#'
#' @param from_date The starting date of the range.
#' @param datetime to_date The ending date of the range.
#' @return A list of the trading days in the specified date range.
get_trading_days_in_range <- function(from_date, to_date) {
  # Checking if the data range is valid
  if (from_date > to_date) {
    stop('Invalid date range')
  }
  
  data.frame('date' = seq(from_date, to_date, 'days')) %>% # Sequene of dates in the range
    filter(!(wday(date) %in% c(1, 7))) %>% # Filtering by weekdays (Mon to Fri)
    anti_join(market_holidays, by = 'date')  # Removing holidays
}


#' Getting a specific number of trading days after a given date.
#'
#' @param after_date The date after which training days are going to be retrieved.
#' @param num_trading_days The number of training days to get.
#' @return A list containing the trading days.
get_trading_days_after <- function(after_date, no_trading_days) {
  trading_days <- data.frame(date = as.Date(character()))
  counter <- 0
  current_date <- after_date
  
  # Getting each day after the given date till getting 
  # the requested number of days
  while (counter < no_trading_days) {
    current_date <- current_date + ddays(1)
    
    # If not in a weekend nor a holiday adding the date to the set of trading days
    if (!(wday(current_date) %in% c(1, 7) | current_date %in% market_holidays$date)) {
      counter <- counter + 1
      trading_days[counter,] <- c(current_date)
    }
  }
  
  trading_days
}


#-----------------------------------
# Training and test sets
#-----------------------------------

#' Extracts training and test sets from a given data set.
#' The training set is created with records in the given date range
#' for the training. The test set is created with records 
#' from the day after the training day and to the date in which 
#' the number of requested test days is covered with trading days.
#'
#' @param historical_prices The dataset of stock historical prices used 
#'    to extract the training and test sets from.
#' @param ticker_symbol The ticker symbol to perform predictions for.
#' @param start_training The minimum date for the records used in the training set.
#' @param end_training The maximum date for the records used in the training set.
#' @param num_test_days The number of trading days after end_training
#'    used to create the test set.
#' @return A list containing the ticker symbol, training set and test set.
get_train_and_test_sets <- function(
  historical_prices, ticker_symbol, start_training, end_training, num_test_days) {
  
  # Filtering the data set to only contains the records related to the
  # given ticker symbol
  df <- historical_prices %>%
    filter(symbol == ticker_symbol)
  
  # Getting the training set
  training_set <- df %>%
    filter(date >= start_training & date <= end_training)
  
  # Getting the specific dates for test which size is 'test_days'
  test_days <- get_trading_days_after(end_training, num_test_days)
  
  # Getting the test set
  test_set <- df %>%
    filter(date >= min(test_days$date) & date <= max(test_days$date))
  
  list(symbol = ticker_symbol, training = training_set, test = test_set)
}


#-------------------------------------------------------
# Chosing a random ticker symbol, traning and test sets
# to perform evaluation
#-------------------------------------------------------

# Choosing a random ticker symbol, except 'DOW' since it doesn't have enough data
eval_ticker_symbol <- sample(filter(dow_jones_stocks, symbol != 'DOW')$symbol , 1)
# Getting the available dates for that ticker symbol
random_dates <- filter(dow_jones_historical_records, symbol == eval_ticker_symbol)
# Getting a random training end date such as there are 
# at least 750 historical records up to it (for training) and
# at least 120 after (for testing )
random_idx_end_training <- sample(750:(nrow(random_dates) - 120), 1)
eval_training_end <- random_dates[random_idx_end_training, 'date']
# Calculating the training start date such as there are 750 records for training set
eval_training_start <- random_dates[(random_idx_end_training - 750 + 1), 'date']

# Extracting train and test sets
eval_sets <- get_train_and_test_sets(dow_jones_historical_records,
                                     eval_ticker_symbol,
                                     eval_training_start,
                                     eval_training_end,
                                     120)

# Getting the test date range
eval_test_start <- min(eval_sets$test$date)
eval_test_end <- max(eval_sets$test$date)

# Cleaning intermediate variables
rm(random_dates, random_idx_end_training)


# Printing the values obtained by the random selection
print(sprintf('Symbol: %s, Training: [%s, %s], Test: [%s, %s]',
              eval_ticker_symbol, eval_training_start, eval_training_end,
              eval_test_start, eval_test_end))

# Plotting the randomly generated training and test sets
ggplot() +
  geom_line(data = eval_sets$training, aes(x = date, y = close, color = 'Training Set')) +
  geom_line(data = eval_sets$test, aes(x = date, y = close, color = 'Test Set')) +
  scale_color_manual(values = c('Training Set' = 'blue', 'Test Set' = 'green')) +
  labs(color = '') +
  theme(legend.position = 'top')


#-------------------------------------------------------
# Useful fuctions to visualize and record the evaluation
# of performance for the different methods
#-------------------------------------------------------

#' Plots the predictions in comparison with the training and validation sets,
#' in order to provide a visualization of a model's prediction performance.
#'
#' @param sets A list containing the training and test sets.
#' @param predictions The predictions against the test set.
#' @param training_predictions The predictions against the training set (optional). 
plot_predictions <- function(sets, predictions, training_predictions = NULL,
                             legend_pos = 'top') {
  plot <- ggplot()
  
  if (!is.null(training_predictions)) {
    plot <- plot + 
      geom_line(data = training_predictions,
                aes(x = date, y = close, color = 'Training Prediction'))
  }
  
  plot <- plot +
    geom_line(data = sets$training, aes(x = date, y = close, color = 'Training Set')) +
    geom_line(data = sets$test, aes(x = date, y = close, color = 'Test Set')) +
    geom_line(data = predictions, aes(x = date, y = close, color = 'Test Prediction')) +
    scale_color_manual(values = c('Training Set' = 'blue',
                                  'Training Prediction' = 'cyan',
                                  'Test Set' = 'green',
                                  'Test Prediction' = 'red')) +
    labs(color = '') +
    theme(legend.position = legend_pos)
  
  plot
}

#' Creates an empty dataframe to report the evaluation results
create_results_dataframe <- function() {
  data.frame('Method' = character(),
             'Number of trading days ahead' = integer(), 
             'RMSE' = numeric(),
             stringsAsFactors = FALSE, check.names = FALSE)
}

#' Dataframe to store all the evaluation results (for each method)
results <- create_results_dataframe()

#' This is a helper function used to support the displaying of the
#' evaluation results for a particular method, and at the same time
#' adding them to the dataframe that contains all the results.
get_evaluation_results <- function(method, predictions) {
  eval_results <- create_results_dataframe()
  
  for (i in c(1, 5, 10, 20, 40, 60, 120)) {
    n <- nrow(eval_results) + 1
    eval_results[n, 'Method'] <- method
    eval_results[n, 'Number of trading days ahead'] <- i
    eval_results[n, 'RMSE'] <- RMSE(predictions$close[1:i], eval_sets$test$close[1:i])
  }
  
  results <<- rbind(results, eval_results)
  
  eval_results %>% select(-'Method')
}



#-----------------------------------
# Models' implementation
#-----------------------------------


#' This function represents a constructor 
#' for a stock forecaster model based on Linear Regression.
#'
#' @param base_dataset The dataframe used to extract the training set
#'    in accordance with the date range. 
#' @param ticker_symbol The ticker symbol to perform predictions for.
#' @param training_start The minimum date for the records used in the training set.
#' @param training_end The maximum date for the records used in the training set.
#' @return The model based on Linear Regression.
LinearRegressionStockForecaster <- function(
  base_dataset, ticker_symbol, training_start = NULL, training_end = NULL) {

  model <- list()

  # Extracting the training set from the base dataset,
  # i.e. filtering by ticker symbol and date range
  training_set <- filter_historical_records(
    base_dataset, ticker_symbol, training_start, training_end) %>%
    select(date, close)

  # Fitting a Linear Regression model where
  # 'date' is the predictor and 'close' is the predicted value
  model$model <- train(close~date, data = training_set, method = 'lm')

  #' The predicting function
  #'
  #' @param from_date The initial date of the date range to predict.
  #' @param to_date The final date of the date range to predict.
  #' @return A dataframe containing the dates in the range to predict
  #'    with their respective predicted closing price.
  model$predict <- function(from_date, to_date = NULL) {
    # If final date is null, then using the initial date, i.e predicting for 1 day
    if (is.null(to_date)) {
      to_date <- from_date
    }

    # Getting a dataframe containing the trading days to make predictions for
    trading_days <- get_trading_days_in_range(from_date, to_date)
    # Getting the predicted stock closing values
    preds <- predict(model$model, trading_days)
    # Creating the dataframe with the predicted values per trading day
    data.frame(date = trading_days$date, close = preds)
  }

  model
}


#' This function represents a constructor 
#' for a stock forecaster model based on Generalized Additive Model.
#'
#' @param base_dataset The dataframe used to extract the training set
#'    in accordance with the date range. 
#' @param ticker_symbol The ticker symbol to perform predictions for.
#' @param training_start The minimum date for the records used in the training set.
#' @param training_end The maximum date for the records used in the training set.
#' @return The model based on Generalized Additive Model.
GeneralizedAdditiveModelStockForecaster <- function(
  base_dataset, ticker_symbol, training_start = NULL, training_end = NULL) {
  
  model <- list()
  
  # Extracting the training set from the base dataset,
  # i.e. filtering by ticker symbol and date range
  training_set <- filter_historical_records(
    base_dataset, ticker_symbol, training_start, training_end) %>%
    select(date, close)
  
  # Fitting a GAM model using caret to tune the parameters,
  # 'date' is the predictor and 'close' is the predicted value
  model$model <- train(close~date, data = training_set, method = 'gam',
                       trControl = trainControl(method = 'cv', number = 6,
                                                summaryFunction = defaultSummary),
                       tuneGrid = expand.grid(select = c(TRUE, FALSE),
                                              method = c("GCV.Cp", "REML", "P-REML",
                                                         "ML", "P-ML")),
                       metric = 'RMSE')
  
  #' The predicting function
  #'
  #' @param from_date The initial date of the date range to predict.
  #' @param to_date The final date of the date range to predict.
  #' @return A dataframe containing the dates in the range to predict
  #'    with their respective predicted closing price.
  model$predict <- function(from_date, to_date = NULL) {
    # If final date is null, then using the initial date, i.e predicting for 1 day
    if (is.null(to_date)) {
      to_date <- from_date
    }
    
    # Getting a dataframe containing the trading days to make predictions for
    trading_days <- get_trading_days_in_range(from_date, to_date)
    # Getting the predicted stock closing values
    preds <- predict(model$model, trading_days)
    # Creating the dataframe with the predicted values per trading day
    data.frame(date = trading_days$date, close = preds)
  }
  
  model
}


#' This function represents a constructor 
#' for a stock forecaster model based on Support-Vector Machine.
#'
#' @param base_dataset The dataframe used to extract the training set
#'    in accordance with the date range. 
#' @param ticker_symbol The ticker symbol to perform predictions for.
#' @param training_start The minimum date for the records used in the training set.
#' @param training_end The maximum date for the records used in the training set.
#' @return The model based on Support-Vector Machine.
SupportVectorMachineStockForecaster <- function(
  base_dataset, ticker_symbol, training_start = NULL, training_end = NULL) {
  
  model <- list()
  
  # Extracting the training set from the base dataset,
  # i.e. filtering by ticker symbol and date range
  training_set <- filter_historical_records(
    base_dataset, ticker_symbol, training_start, training_end) %>%
    select(date, close)
  
  # Fitting a Linear Regression model where
  # 'date' is the predictor and 'close' is the predicted value
  model$model <- train(close~date, data = training_set, method = 'svmRadial',
                       trControl = trainControl(method = 'cv', number = 6,
                                                summaryFunction = defaultSummary),
                       tuneGrid = expand.grid(sigma= 2^c(-25, -20, -15,-10, -5, 0),
                                              C= 2^c(0:5)),
                       metric = "RMSE")
  
  #' The predicting function
  #'
  #' @param from_date The initial date of the date range to predict.
  #' @param to_date The final date of the date range to predict.
  #' @return A dataframe containing the dates in the range to predict
  #'    with their respective predicted closing price.
  model$predict <- function(from_date, to_date = NULL) {
    # If final date is null, then using the initial date, i.e predicting for 1 day
    if (is.null(to_date)) {
      to_date <- from_date
    }

    # Getting a dataframe containing the trading days to make predictions for
    trading_days <- get_trading_days_in_range(from_date, to_date)
    # Getting the predicted stock closing values
    preds <- predict(model$model, trading_days)
    # Creating the dataframe with the predicted values per trading day
    data.frame(date = trading_days$date, close = preds)
  }
  
  model
}


#' This function represents a constructor 
#' for a stock forecaster model based on ARIMA.
#'
#' @param base_dataset The dataframe used to extract the training set
#'    in accordance with the date range. 
#' @param ticker_symbol The ticker symbol to perform predictions for.
#' @param training_start The minimum date for the records used in the training set.
#' @param training_end The maximum date for the records used in the training set.
#' @return The model based on ARIMA.
ArimaStockForecaster <- function(
  base_dataset, ticker_symbol, training_start = NULL, training_end = NULL) {

  model <- list()

  # Extracting the training set from the base dataset,
  # i.e. filtering by ticker symbol and date range
  training_set <- filter_historical_records(
    base_dataset, ticker_symbol, training_start, training_end) %>%
    select(date, close)

  # Keeping track of the training end date
  model$training_end <- max(training_set$date)

  # Fitting an ARIMA model
  # (using auto.arima which automatically tunes the parameters)
  # to predict it takes a time series containing the stock closing prices
  model$model <- auto.arima(
    training_set %>% column_to_rownames(var = 'date') %>% .$close,
    start.p = 0, start.q = 0, max.p = 5, max.q = 5,
    start.P = 0, start.Q = 0, max.P = 5, max.Q = 5, 
    d = 1, D = 1,
    seasonal = TRUE, 
    trace = FALSE)

  #' The predicting function
  #'
  #' @param from_date The initial date of the date range to predict.
  #' @param to_date The final date of the date range to predict.
  #' @return A dataframe containing the dates in the range to predict
  #'    with their respective predicted closing price.
  model$predict <- function(from_date, to_date = NULL) {
    # If final date is null, then using the initial date, i.e predicting for 1 day
    if (is.null(to_date)) {
      to_date <- from_date
    }
    # Checking that date range is valid
    if (from_date > to_date) {
      stop('Invalid date range')
    }
    # Checking that prediction range is after training
    if (from_date <= model$training_end) {
      stop('Prediction range should be after training')
    }

    # Getting a dataframe containing the trading days to make predictions for,
    # including the days that might be missing after the end of training
    # and the begining of the predicting range
    trading_days <- get_trading_days_in_range(model$training_end + ddays(1), to_date)
    # Getting the predicted stock closing values
    preds <- forecast(model$model, nrow(trading_days))
    # Creating the dataframe with the predicted values per trading day
    # and filtering to include just the trading days in the given range
    data.frame(date = trading_days, close = preds$mean) %>%
      filter(date >= from_date)
  }

  model
}


#' This function represents a constructor 
#' for a stock forecaster model based on Prophet.
#'
#' @param base_dataset The dataframe used to extract the training set
#'    in accordance with the date range. 
#' @param ticker_symbol The ticker symbol to perform predictions for.
#' @param training_start The minimum date for the records used in the training set.
#' @param training_end The maximum date for the records used in the training set.
#' @return The model based on Prophet
ProphetStockForecaster <- function(
  base_dataset, ticker_symbol, start_date = NULL, end_date = NULL) {

  model <- list()

  # Extracting the training set from the base dataset)
  # (i.e. filtering by ticker symbol and date range),
  # and changing the column names to 'ds' and 'y' which are the ones Prophet uses
  training_set <- filter_historical_records(
    base_dataset, ticker_symbol, start_date, end_date) %>%
    select(date, close) %>%
    setnames(old = c('date', 'close'), new = c('ds', 'y'))

  # Fitting an Prophet model
  model$model <- prophet(training_set, daily.seasonality = TRUE)

  #' The predicting function
  #'
  #' @param from_date The initial date of the date range to predict.
  #' @param to_date The final date of the date range to predict.
  #' @return A dataframe containing the dates in the range to predict
  #'    with their respective predicted closing price.
  model$predict <- function(from_date, to_date = NULL) {
    # If final date is null, then using the initial date, i.e predicting for 1 day
    if (is.null(to_date)) {
      to_date <- from_date
    }

    # Getting a dataframe containing the trading days to make predictions for,
    # and changing the column name to 'ds' which is the one Prophet uses
    trading_days <- get_trading_days_in_range(from_date, to_date) %>%
      setnames(old = c('date'), new = c('ds'))
    # Getting the predicted stock closing values
    preds <- predict(model$model, trading_days)
    # Creating the dataframe with the predicted values per trading day
    data.frame(date = trading_days$ds, close = preds$yhat)
  }

  model
}


#' This function represents a constructor 
#' for a stock forecaster model based on Long Short-Term Memory.
#'
#' @param base_dataset The dataframe used to extract the training set
#'    in accordance with the date range. 
#' @param ticker_symbol The ticker symbol to perform predictions for.
#' @param training_start The minimum date for the records used in the training set.
#' @param training_end The maximum date for the records used in the training set.
#' @return The model based on Long Short-Term Memory
LongShortTermMemoryStockForecaster <- function(
  base_dataset, ticker_symbol, start_date = NULL, end_date = NULL) {

  model <- list()

  # The ticker symbol used to fit the model
  model$ticker_symbol <- ticker_symbol

  # Size of the time sub-series used to feed the LSTM network
  model$timesteps <- 60
  # Number of epochs used to train the LSTM network
  model$epochs <- 2

  # Extracting the training set from the base dataset)
  # i.e. filtering by ticker symbol and date range.
  model$training_set <- filter_historical_records(
    base_dataset, model$ticker_symbol, start_date, end_date) %>%
    select(date, close)

  # Keeping track of the training end date
  model$training_end <- max(model$training_set$date)

  # Scale factor used to normalize the timeseries used as input for the 
  # LSTM network to contain values in the range [0, 1]
  model$scale_factor <- max(model$training_set$close) * 2.0

  # Constructing the inputs used to traing the LSTM network
  # The predictor is a collection of time sub-series of size 'timesteps'
  train_X <- t(sapply((model$timesteps + 1): nrow(model$training_set),
                      function(i) {
                        model$training_set$close[(i - model$timesteps) : (i - 1)]
                      }))
  train_X <- train_X / model$scale_factor
  dim(train_X) <- c(dim(train_X)[1], dim(train_X)[2], 1)
  # The outcome are the closing prices normalized/scaled to the range [0, 1]
  train_Y <- model$training_set$close[(model$timesteps + 1) : nrow(model$training_set)]
  train_Y <- train_Y / model$scale_factor

  # Constructing the LSTM neural network, with an architecture of two LSTM layers
  model$model <- keras_model_sequential() %>%
    layer_lstm(units = 50, return_sequences = TRUE,
               input_shape = c(model$timesteps, 1)) %>%
    layer_lstm(units = 50) %>%
    layer_dense(1) %>%
    compile(loss = 'mean_squared_error', optimizer = 'adam')

  # Training the LSTM network
  model$training_history <- model$model %>%
    fit(x = train_X, y = train_Y, epochs = model$epochs, batch_size = 1, verbose = 2)

  #' The predicting function
  #'
  #' @param from_date The initial date of the date range to predict.
  #' @param to_date The final date of the date range to predict.
  #' @param base_dataset The data set used to support the prediction process.
  #' @return A dataframe containing the dates in the range to predict
  #'    with their respective predicted closing price.
  model$predict <- function(from_date, to_date = NULL, base_dataset = NULL)  {
    # If final date is null, then using the initial date, i.e predicting for 1 day
    if (is.null(to_date)) {
      to_date <- from_date
    }
    # Checking that date range is valid
    if (from_date > to_date) {
      stop('Invalid date range')
    }
    
    # If not base dataset to support predictions is provided,
    # then using the training set
    if (is.null(base_dataset)) {
      base_dataset <- model$training_set
    } else {
      base_dataset <- filter(base_dataset, symbol == model$ticker_symbol)
    }
    
    # Only the historical records on or before the end prediction date are needed
    base_dataset <- filter(base_dataset, date <= to_date)
    
    # Reducing the base dataset just to contain the historical records
    # necessary to perform the prediction
    idx <- which(base_dataset$date >= from_date)
    if (length(idx) > 0) {
      # If there are records that already exist in the given date range ...
      idx <- min(idx)
      
      if (idx <= model$timesteps) {
        stop('Not enough records to perform predictions')
      }
      
      # ... keeping the previous 'timesteps' records before the begining of 
      # of the prediction date range
      base_dataset <- base_dataset[(idx - model$timesteps) : nrow(base_dataset),]
    } else {
      # If there are not records already existing in the given date range,
      # just taking the last 'timesteps' records
      base_dataset <- tail(base_dataset, n = model$timesteps)
      
      if (nrow(base_dataset) < model$timesteps) {
        stop('Not enough records to perform predictions')
      }
    }
    
    # Getting the missing days (for which there are not historical records) 
    # needed to fulfill the predictions in the given range 
    missing_start <- max(base_dataset$date) + ddays(1)
    if (missing_start <= to_date) {
      missing_days <- get_trading_days_in_range(missing_start, to_date)
    } else {
      missing_days <- NULL
    }
    
    # Normalizing the time series used to perform the predictions 
    # to be in the range [0, 1]
    inputs <- base_dataset$close / model$scale_factor
    
    trading_days <- c() # The trading days in the date range to predict
    preds <- c() # The predicted closing prices in the date range
    
    count <- 1
    
    # First using the historical records that already exist to 
    # suuport the prediction in the given date range
    i <- model$timesteps + 1
    while (i <= nrow(base_dataset)) {
      trading_days[count] <- base_dataset[i, 'date']
      
      # Feeding the LSTM network with the previous subseries of size 'timesteps'
      x <- inputs[(i - model$timesteps) : (i - 1)]
      dim(x) <- c(1, model$timesteps, 1)
      y <- predict(model$model, x)
      
      # Scaling back the output to be the prediction
      preds[count] <- y * model$scale_factor
      
      i <- i + 1
      count <- count + 1
    }
    
    # Secondly, fulfiling the missing days by completing the timeseries
    # with the prediction for the previous day
    j <- 1
    while (!is.null(missing_days) && j <= nrow(missing_days)) {
      trading_days[count] <- missing_days[j, 'date']
      
      # Feeding the LSTM network with the previous subseries of size 'timesteps'
      x <- inputs[(length(inputs) - model$timesteps + 1) : length(inputs)]
      dim(x) <- c(1, model$timesteps, 1)
      y <- predict(model$model, x)
      
      # Completing the timeseries with the predicted value for the current day
      inputs[length(inputs) + 1] <- y
      
      # Scaling back the output to be the prediction
      preds[count] <- y * model$scale_factor
      
      j <- j + 1
      count <- count + 1
    }
    
    # Returning the results just for the given prediction data range
    data.frame(date = as_date(trading_days), close = preds) %>%
      filter(date >= from_date)
  }

  model
}


#-----------------------------------
# Evaluation
#-----------------------------------

# Evaluation of Linear Regression

lr_forecaster <- LinearRegressionStockForecaster(eval_sets$training, eval_ticker_symbol)
lr_predictions <- lr_forecaster$predict(eval_test_start, eval_test_end)

lr_train_predictions <- lr_forecaster$predict(eval_training_start, eval_training_end)

plot_predictions(eval_sets, lr_predictions, lr_train_predictions)

get_evaluation_results('Linear Regression', lr_predictions)


# Evaluation of GAM

gam_forecaster <- GeneralizedAdditiveModelStockForecaster(
  eval_sets$training, eval_ticker_symbol)
gam_predictions <- gam_forecaster$predict(eval_test_start, eval_test_end)

gam_train_predictions <- gam_forecaster$predict(eval_training_start, eval_training_end)

plot_predictions(eval_sets, gam_predictions, gam_train_predictions)

get_evaluation_results('GAM', gam_predictions)


# Evaluation of SVM

svm_forecaster <- SupportVectorMachineStockForecaster(
  eval_sets$training, eval_ticker_symbol)
svm_predictions <- svm_forecaster$predict(eval_test_start, eval_test_end)

svm_train_predictions <- svm_forecaster$predict(eval_training_start, eval_training_end)

plot_predictions(eval_sets, svm_predictions, svm_train_predictions)

get_evaluation_results('SVM', gam_predictions)


# Evaluation of ARIMA

arima_forecaster <- ArimaStockForecaster(eval_sets$training, eval_ticker_symbol)
arima_predictions <- arima_forecaster$predict(eval_test_start, eval_test_end)

plot_predictions(eval_sets, arima_predictions)

get_evaluation_results('ARIMA', arima_predictions)


# Evaluation of Prophet

prophet_forecaster <- ProphetStockForecaster(eval_sets$training, eval_ticker_symbol)
prophet_predictions <- prophet_forecaster$predict(eval_test_start, eval_test_end)

prophet_train_predictions <- prophet_forecaster$predict(eval_training_start, eval_training_end)

plot_predictions(eval_sets, prophet_predictions, prophet_train_predictions)

get_evaluation_results('Prophet', prophet_predictions)


# Evaluation of LSTM

lstm_forecaster <- LongShortTermMemoryStockForecaster(eval_sets$training, eval_ticker_symbol)

# Prediction on the training set
lstm_train_predictions <- lstm_forecaster$predict(eval_sets$training[61,]$date, eval_training_end)

# Prediction and evaluation when the prediction range is completely unknown
lstm_predictions <- lstm_forecaster$predict(eval_test_start, eval_test_end)

plot_predictions(eval_sets, lstm_predictions, lstm_train_predictions)

get_evaluation_results('LSTM', lstm_predictions)

# Prediction and evaluation when the dataset is updated daily and the prediction done
# for the next day
lstm_daily_predictions <- lstm_forecaster$predict(eval_test_start, eval_test_end,
                                                  dow_jones_historical_records)

plot_predictions(eval_sets, lstm_daily_predictions, lstm_train_predictions)

get_evaluation_results('LSTM - Updated DS', lstm_predictions)


#-----------------------------------
# Results
#-----------------------------------

# Plotting the evaluation's predictions in comparison for with the tests and
# training sets in one big plot containing all the methods
lr_plot <- plot_predictions(eval_sets, lr_predictions, lr_train_predictions, legend_pos = 'right')
legend <- get_legend(lr_plot)

plot_grid(
  plot_grid(
    plot_predictions(eval_sets, lr_predictions, lr_train_predictions, legend_pos = 'none'),
    #lr_plot,
    plot_predictions(eval_sets, gam_predictions, gam_train_predictions, legend_pos = 'none'),
    plot_predictions(eval_sets, svm_predictions, svm_train_predictions, legend_pos = 'none'),
    plot_predictions(eval_sets, arima_predictions, legend_pos = 'none'),
    plot_predictions(eval_sets, prophet_predictions, prophet_train_predictions, legend_pos = 'none'),
    plot_predictions(eval_sets, lstm_predictions, lstm_train_predictions, legend_pos = 'none'),
    plot_predictions(eval_sets, lstm_daily_predictions, lstm_train_predictions, legend_pos = 'none'),
    ncol = 1,
    labels = c('LR', 'GAM', 'SVM', 'ARIMA', 'Prophet', 'LSTM', 'LSTM - Updated DS')),
  legend,
  ncol = 2,  rel_widths = c(1, .4))

rm(lr_plot, legend)


# Displaying the results by RMSE
results %>% spread('Number of trading days ahead', 'RMSE')

# This is the function to do the benchmarking of the results
benchmark_against_linear_regression <- function(results) {
  filter(results, Method != 'Linear Regression') %>%
    left_join(results %>%
                filter(Method == 'Linear Regression') %>%
                select(-Method) %>%
                setnames(old = c('RMSE'), new = c('LR_RMSE')),
              by = 'Number of trading days ahead') %>%
    mutate('Improvement Ratio' = (LR_RMSE - RMSE) / LR_RMSE) %>%
    select(-c('RMSE', 'LR_RMSE'))  
}

# Benchmarking against linear regression
benchmark_against_linear_regression(results) %>%
  spread('Number of trading days ahead', 'Improvement Ratio')
