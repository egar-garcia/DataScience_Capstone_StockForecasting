#-----------------------------------
# Required packages and libraries
#-----------------------------------

if(!require(tidyverse)) install.packages('tidyverse')
if(!require(httr)) install.packages('httr')
if(!require(jsonlite)) install.packages('jsonlite')
if(!require(ggcorrplot)) install.packages('ggcorrplot')
if(!require(directlabels)) install.packages('directlabels')
if(!require(caret)) install.packages('caret')
if(!require(forecast)) install.packages('forecast')
if(!require(prophet)) install.packages('prophet')
if(!require(keras)) install.packages('keras')


library(tidyr)
library(tidyverse)
library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(ggcorrplot)
library(directlabels)
library(httr)
library(jsonlite)
library(caret)
library(forecast)
library(keras)

# Install Keras if you have not installed before
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

dow_jones_dataframe_filename <- 'dow_jones_dataframe.Rda'


#-----------------------------------
# Trading days management
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


#--------------------
# Examples of usage
#--------------------

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
  geom_line() +
  labs(colour = '') +
  theme(legend.position = 'bottom') +
  geom_dl(aes(label = symbol), method = 'angled.boxes')


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
  theme(legend.position = "bottom") +
  guides(size = FALSE)


# Plotting the correlations among the stoks and the DJIA as a matrix heat-map
dow_jones_historical_records %>%
  # Hack alert: Prefixing the DJIA with ' ' in order to place it firts
  mutate(symbol = ifelse(symbol != djia_symbol, symbol, paste(' ', djia_symbol))) %>%
  spread(symbol, close) %>% 
  select(-date) %>%
  cor(method = 'pearson', use = 'complete.obs') %>%
  ggcorrplot()


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



#-----------------------------------
# Training and validation sets
#-----------------------------------

#' Extract training and validation sets from a given data set.
#' The training set is created with records in the given date range
#' for the training. The validation set is created with records 
#' from the day after the training day and to the date in which 
#' the number of requested validation days is covered with trading days.
#'
#' @param historical_prices The dataset of stock historical prices used 
#'    to extract the training and validation sets from.
#' @param ticker_symbol The ticker symbol to perform predictions for.
#' @param start_training The minimum date for the records used in the training set.
#' @param end_training The maximum date for the records used in the training set.
#' @param validation_days The number of trading days after end_training
#'    used to create the validation set.
get_train_and_validation_sets <- function(
  historical_prices, ticker_symbol, start_training, end_training, validation_days) {

  # Filtering the data set to only contains the records related to the
  # given ticker symbol
  df <- historical_prices %>%
    filter(symbol == ticker_symbol)

  # Getting the training set
  training_set <- df %>%
    filter(date >= start_training & date <= end_training)

  # Getting the specific dates for validation whic size is validation_days
  validation_days <- get_trading_days_after(end_training, validation_days)

  # Getting the validation set
  validation_set <- df %>%
    filter(date >= min(validation_days$date) & date <= max(validation_days$date))

  list(training = training_set, validation = validation_set)
}


#' Gets a dataframe containing a subset of the records of the current dataset,
#' which is obtained by filtering by a ticker symbol and/or a date range.
#' 
#' @param ticker_symbol The ticker symbol to filter the data.
#' @param from_date The minimum date to appear in the records of the subset.
#' @param to_date The maximum date to appear in the records of the subset.
#' @return The dataframe with the subset resulted of filtering the dataset.
filter_historical_prices <- function(historical_prices, 
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


#-----------------------------------
# Models
#-----------------------------------

#' This function represents a constructor 
#' for a stock forecaster model based on Linear Regression.
#'
#' @param base_dataset The dataframe used to extract the training set
#'    in accordance with the date range. 
#' @param ticker_symbol The ticker symbol to perform predictions for.
#' @param training_start The minimum date for the records used in the training set.
#' @param training_end The maximum date for the records used in the training set.
#' @return The model based on Linear Regression
LinearRegressionStockForecaster <- function(
  base_dataset, ticker_symbol, training_start = NULL, training_end = NULL) {

  model <- list()

  # Extracting the training set from the base dataset,
  # i.e. filtering by ticker symbol and date range
  training_set <- filter_historical_prices(
    base_dataset, ticker_symbol, training_start, training_end) %>%
    select(date, close)

  # Fitting a Linear Regression model where
  # 'date' is the predictor and 'close' is the predicted value
  model$model <- lm(close~date, training_set)

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
#' @return The model based on ARIMA
ArimaStockForecaster <- function(
  base_dataset, ticker_symbol, training_start = NULL, training_end = NULL) {

  model <- list()

  # Extracting the training set from the base dataset,
  # i.e. filtering by ticker symbol and date range
  training_set <- filter_historical_prices(
    base_dataset, ticker_symbol, training_start, training_end) %>%
    select(date, close)

  # Keeping track of the training end date
  model$training_end <- max(training_set$date)

  # Fitting an ARIMA model
  # (using auto.arima which automatically tunes the parameters)
  # to predict it takes a time series containing the stock closing prices
  model$model <- auto.arima(
    training_set %>% column_to_rownames(var = 'date') %>% .$close,
    D = 1)

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
  training_set <- filter_historical_prices(
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


LongShortTermMemoryStockForecaster <- function(
  base_dataset, ticker_symbol, start_date = NULL, end_date = NULL) {

  model <- list()

  model$ticker_symbol <- ticker_symbol

  model$timesteps <- 60
  model$epochs <- 2

  # Extracting the training set from the base dataset)
  # i.e. filtering by ticker symbol and date range.
  model$training_set <- filter_historical_prices(
    base_dataset, model$ticker_symbol, start_date, end_date) %>%
    select(date, close)

  # Keeping track of the training end date
  model$training_end <- max(training_set$date)

  model$scale_factor <- max(model$training_set$close) * 2.0

  train_X <- t(sapply((model$timesteps + 1): nrow(model$training_set),
                      function(i) {
                        model$training_set$close[(i - model$timesteps) : (i - 1)]
                      }))
  train_X <- train_X / model$scale_factor
  dim(train_X) <- c(dim(train_X)[1], dim(train_X)[2], 1)

  train_Y <- model$training_set$close[(model$timesteps + 1) : nrow(model$training_set)]
  train_Y <- train_Y / model$scale_factor

  model$model <- keras_model_sequential() %>%
    layer_lstm(units = 50, return_sequences = TRUE,
               input_shape = c(model$timesteps, 1)) %>%
    layer_lstm(units = 50) %>%
    layer_dense(1) %>%
    compile(loss = 'mean_squared_error', optimizer = 'adam')

  model$training_history <- model$model %>%
    fit(x = train_X, y = train_Y, epochs = model$epochs, batch_size = 1, verbose = 2)


  model
}



#-----------------------------------
# https://www.datascience.com/blog/stock-price-time-series-arima

sets <- get_train_and_validation_sets(dow_jones_historical_records,
                                      'BA',
                                      as.Date('2015-07-01', '%Y-%m-%d'),
                                      as.Date('2018-06-30', '%Y-%m-%d'),
                                      c(120))

#m <- LinearRegressionStockForecaster(
#m <- ArimaStockForecaster(
#m <- ProphetStockForecaster(
m <- LongShortTermMemoryStockForecaster(
  dow_jones_historical_records,
  'BA', min(sets$training$date), max(sets$training$date))
preds <- m$predict(min(sets$validation$date), max(sets$validation$date))
preds2 <- m$predict(min(sets$validation$date) + ddays(30), max(sets$validation$date))
#preds3 <- m$predict(max(sets$validation$date) + ddays(30), max(sets$validation$date))

ggplot() +
  #expand_limits(y = 0) +
  geom_line(data = sets$training, aes(x = date, y = close), color = 'blue') +
  #geom_ribbon(data = preds, aes(x = date, ymax = close_upper, ymin = close_lower), alpha = 0.5, fill = "skyblue") +
  geom_line(data = sets$validation, aes(x = date, y = close), color = 'green') +
  geom_line(data = preds, aes(x = date, y = close), color = 'red') +
  geom_line(data = preds2, aes(x = date, y = close), color = 'black')
  #geom_line(data = preds, aes(x = as.Date(ds), y = yhat), color = 'red') +
  #geom_line(data = preds, aes(x = as.Date(ds), y = trend), color = 'black') +
  #geom_ribbon(data = preds, aes(x = as.Date(ds), ymax = yhat_upper, ymin = yhat_lower), alpha = 0.5, fill = "skyblue")

y = predict(m$model, sets$training %>% setnames(old = c('date'), new = c('ds')))

#-----------------------------------


timesteps <- 60

#tx <- t(sapply(1 : (nrow(sets$training) - timesteps + 1),
#              function(i) sets$training$close[i : (i +  timesteps - 1)]))

tx <- t(sapply((timesteps + 1): nrow(sets$training),
               function(i) sets$training$close[(i - timesteps) : (i - 1)]))
dim(tx) <- c(dim(tx)[1], dim(tx)[2], 1)
tx <- tx / 400.0

ty <- sets$training$close[(timesteps + 1) : nrow(sets$training)]
ty <- ty / 400.0

m <- keras_model_sequential() %>%
  layer_lstm(units = 50, return_sequences = TRUE, input_shape = c(timesteps, 1)) %>%
  layer_lstm(units = 50) %>%
  layer_dense(1) %>%
  compile(loss='mean_squared_error', optimizer='adam')

mh <- m %>%  fit(x = tx, y = ty, epochs = 1, batch_size = 1, verbose = 2)

y <- predict(m, tx)

ggplot() +
  geom_line(data = sets$training, aes(x = date, y = close), color = 'blue') +
  geom_line(aes(x = tail(sets$training$date, -60), y = y*400.0), color = 'red')


p <- function(from_date, to_date = NULL, base_dataset = NULL)  {
  # If final date is null, then using the initial date, i.e predicting for 1 day
  if (is.null(to_date)) {
    to_date <- from_date
  }
  # Checking that date range is valid
  if (from_date > to_date) {
    stop('Invalid date range')
  }
  # Checking that prediction range is after training
  if (from_date <= m$training_end) {
    stop('Prediction range should be after training')
  }

  if (is.null(base_dataset)) {
    base_dataset <- m$training_set
  } else {
    base_dataset <- filter(base_dataset, symbol == m$ticker_symbol)
  }

  # Getting a dataframe containing the trading days to make predictions for,
  # including the days that might be missing after the end of training
  # and the begining of the predicting range
  trading_days <-
    get_trading_days_in_range(m$training_end + ddays(1), to_date)$date

  
  inputs <- tail(filter(base_dataset, date <= m$training_end)$close,
                 n = m$timesteps)
  inputs <- inputs / m$scale_factor
  preds <- c()

  for (i in 1:length(trading_days)) {
    x <- inputs[i : (i + m$timesteps - 1)]
    dim(x) <- c(1, m$timesteps, 1)
    y <- predict(m$model, x)
    preds[i] <- y * m$scale_factor

    existing_rec <- filter(base_dataset, date == trading_days[i])
    if (nrow(existing_rec) > 0) {
      inputs[i + m$timesteps] <- existing_rec$close[1] / m$scale_factor
    } else {
      inputs[i + m$timesteps] <- y
    }
  }

  data.frame(date = trading_days, close = preds) %>%
    filter(date >= from_date)
}

fp <- function(from_date, to_date = NULL, base_dataset = NULL)  {
  # If final date is null, then using the initial date, i.e predicting for 1 day
  if (is.null(to_date)) {
    to_date <- from_date
  }
  # Checking that date range is valid
  if (from_date > to_date) {
    stop('Invalid date range')
  }

  # If not base dataset to support predictions is provided, using the training set
  if (is.null(base_dataset)) {
    base_dataset <- m$training_set
  } else {
    base_dataset <- filter(base_dataset, symbol == m$ticker_symbol)
  }

  # Only the records on or before the end date of the prediction range are needed
  base_dataset <- filter(base_dataset, date <= to_date)

  idx <- which(base_dataset$date >= from_date)
  if (length(idx) > 0) {
    idx <- min(idx)

    if (idx <= m$timesteps) {
      stop('Not enough records to perform predictions')
    }

    base_dataset <- base_dataset[(idx - m$timesteps) : nrow(base_dataset),]
  } else {
    base_dataset <- tail(base_dataset, n = m$timesteps)

    if (nrow(base_dataset) <= m$timesteps) {
      stop('Not enough records to perform predictions')
    }
  }

  # Getting the missing days (which don't have historical records) 
  # needed to fulfill the predictions in the given range 
  missing_start <- max(base_dataset$date) + ddays(1)
  if (missing_start <= to_date) {
    missing_days <- get_trading_days_in_range(missing_start, to_date)
  } else {
    missing_days <- NULL
  }

  inputs <- base_dataset$close / m$scale_factor

  trading_days <- c()
  preds <- c()

  count <- 1

  i <- m$timesteps + 1
  while (i <= nrow(base_dataset)) {
    trading_days[count] <- base_dataset[i, 'date']

    x <- inputs[(i - m$timesteps) : (i - 1)]
    dim(x) <- c(1, m$timesteps, 1)
    y <- predict(m$model, x)

    preds[count] <- y * m$scale_factor

    i <- i + 1
    count <- count + 1
  }

  j <- 1
  while (!is.null(missing_days) && j <= nrow(missing_days)) {
    trading_days[count] <- missing_days[j, 'date']

    x <- inputs[(length(inputs) - m$timesteps + 1) : length(inputs)]
    dim(x) <- c(1, m$timesteps, 1)
    y <- predict(m$model, x)

    inputs[length(inputs) + 1] <- y

    preds[count] <- y * m$scale_factor

    j <- j + 1
    count <- count + 1
  }

  data.frame(date = as_date(trading_days), close = preds) %>%
    filter(date >= from_date)
}

preds <- p(as.Date('2018-07-01', '%Y-%m-%d'), as.Date('2018-10-31', '%Y-%m-%d'), dow_jones_historical_records)
preds <- fp(as.Date('2018-09-01', '%Y-%m-%d'), as.Date('2018-12-31', '%Y-%m-%d'), dow_jones_historical_records)
preds <- fp(as.Date('2018-09-01', '%Y-%m-%d'), as.Date('2018-12-31', '%Y-%m-%d'), sets$training)

ggplot() +
  geom_line(data = sets$training, aes(x = date, y = close), color = 'blue') +
  geom_line(data = sets$validation, aes(x = date, y = close), color = 'green') +
  geom_line(data = preds, aes(x = date, y = close), color = 'red')


