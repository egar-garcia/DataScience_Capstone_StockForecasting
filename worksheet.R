#-----------------------------------
sets <- get_train_and_validation_sets(dow_jones_historical_records,
                                      'BA',
                                      as.Date('2015-07-01', '%Y-%m-%d'),
                                      as.Date('2018-06-30', '%Y-%m-%d'),
                                      c(120))

#-----------------------------------

#m <- LinearRegressionStockForecaster(
#m <- GeneralizedAdditiveModelStockForecaster(
m <- SupportVectorMachineStockForecaster(
#m <- ArimaStockForecaster(
#m <- ProphetStockForecaster(
#m <- LongShortTermMemoryStockForecaster(
  dow_jones_historical_records,
  'BA', min(sets$training$date), max(sets$training$date))

#-----------------------------------

training_preds <- m$predict(min(sets$training$date), max(sets$training$date))
preds <- m$predict(min(sets$validation$date), max(sets$validation$date))

ggplot() +
  geom_line(data = sets$training, aes(x = date, y = close), color = 'blue') +
  geom_line(data = training_preds, aes(x = date, y = close), color = 'cyan') +
  geom_line(data = sets$validation, aes(x = date, y = close), color = 'green') +
  geom_line(data = preds, aes(x = date, y = close), color = 'red')

#-----------------------------------

preds <- m$predict(min(sets$validation$date), max(sets$validation$date))

ggplot() +
  geom_line(data = sets$training, aes(x = date, y = close), color = 'blue') +
  geom_line(data = sets$validation, aes(x = date, y = close), color = 'green') +
  geom_line(data = preds, aes(x = date, y = close), color = 'red')
  

#-----------------------------------

preds1 <- m$predict(min(sets$validation$date), max(sets$validation$date))
preds2 <- m$predict(min(sets$validation$date), max(sets$validation$date), base_dataset = dow_jones_historical_records)

ggplot() +
  geom_line(data = sets$training, aes(x = date, y = close), color = 'blue') +
  geom_line(data = sets$validation, aes(x = date, y = close), color = 'green') +
  geom_line(data = preds1, aes(x = date, y = close), color = 'red') +
  geom_line(data = preds2, aes(x = date, y = close), color = 'orange')

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
  
  # If not base dataset to support predictions is provided,
  # then using the training set
  if (is.null(base_dataset)) {
    base_dataset <- m$training_set
  } else {
    base_dataset <- filter(base_dataset, symbol == m$ticker_symbol)
  }
  
  # Only the historical records on or before the end prediction date are needed
  base_dataset <- filter(base_dataset, date <= to_date)

  # Reducing the base dataset just to contain the historical records
  # necessary to perform the prediction
  idx <- which(base_dataset$date >= from_date)
  if (length(idx) > 0) {
    # If there are records that already exist in the given date range ...
    idx <- min(idx)
    
    if (idx <= m$timesteps) {
      stop('Not enough records to perform predictions')
    }

    # ... keeping the previous 'timesteps' records before the begining of 
    # of the prediction date range
    base_dataset <- base_dataset[(idx - m$timesteps) : nrow(base_dataset),]
  } else {
    # If there are not records already existing in the given date range,
    # just taking the last 'timesteps' records
    base_dataset <- tail(base_dataset, n = m$timesteps)

    if (nrow(base_dataset) <= m$timesteps) {
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
  inputs <- base_dataset$close / m$scale_factor
  
  trading_days <- c() # The trading days in the date range to predict
  preds <- c() # The predicted closing prices in the date range
  
  count <- 1
  
  # First using the historical records that already exist to 
  # suuport the prediction in the given date range
  i <- m$timesteps + 1
  while (i <= nrow(base_dataset)) {
    trading_days[count] <- base_dataset[i, 'date']

    # Feeding the LSTM network with the previous subseries of size 'timesteps'
    x <- inputs[(i - m$timesteps) : (i - 1)]
    dim(x) <- c(1, m$timesteps, 1)
    y <- predict(m$model, x)

    # Scaling back the output to be the prediction
    preds[count] <- y * m$scale_factor
    
    i <- i + 1
    count <- count + 1
  }

  # Secondly, fulfiling the missing days by completing the timeseries
  # with the prediction for the previous day
  j <- 1
  while (!is.null(missing_days) && j <= nrow(missing_days)) {
    trading_days[count] <- missing_days[j, 'date']
  
    # Feeding the LSTM network with the previous subseries of size 'timesteps'
    x <- inputs[(length(inputs) - m$timesteps + 1) : length(inputs)]
    dim(x) <- c(1, m$timesteps, 1)
    y <- predict(m$model, x)

    # Completing the timeseries with the predicted value for the current day
    inputs[length(inputs) + 1] <- y

    # Scaling back the output to be the prediction
    preds[count] <- y * m$scale_factor
    
    j <- j + 1
    count <- count + 1
  }

  # Returning the results just for the given prediction data range
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


