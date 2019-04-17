#-----------------------------------
sets <- get_train_and_validation_sets(dow_jones_historical_records,
                                      'BA',
                                      as.Date('2015-07-01', '%Y-%m-%d'),
                                      as.Date('2018-06-30', '%Y-%m-%d'),
                                      c(120))

#-----------------------------------

#m <- LinearRegressionStockForecaster(
#m <- GeneralizedAdditiveModelStockForecaster(
#m <- SupportVectorMachineStockForecaster(
m <- MovingAverageStockForecaster(
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
  #geom_smooth(data = sets$training, aes(x = date, y = close)) +
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

preds <- sma(sets$training$close,)

ggplot() +
  geom_line(data = sets$training, aes(x = date, y = close), color = 'blue') +
  geom_line(aes(x = sets$training$date, y = preds$), color = 'cyan') 

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


#--------------------


dow_jones_historical_records %>%
  filter(symbol != 'DIA') %>%
  left_join(
    dow_jones_historical_records %>%
      filter(symbol == 'DIA') %>% 
      select(date, close) %>%
      setnames(old = c('close'), new = c('close_index')),
    by = 'date') %>%
  mutate(rate = (close - close_index) / close_index) %>%
  ggplot(aes(x = date, y = rate, group = symbol, color = symbol)) +
  geom_line(size = 0.3) +
  labs(colour = '') +
  theme(legend.position = 'none') +
  geom_dl(aes(label = symbol), method = 'angled.boxes') +
  geom_abline(aes(intercept = 0, slope = 0), linetype = 'dashed')

dow_jones_historical_records %>%
  filter(symbol != 'DIA') %>%
  left_join(
    dow_jones_historical_records %>%
      filter(symbol == 'DIA') %>% 
      select(date, close) %>%
      setnames(old = c('close'), new = c('close_index')),
    by = 'date') %>%
  group_by(symbol) %>%
  mutate(mean_proportion = mean(close / close_index)) %>%
  ungroup() %>%
  mutate(rate = (close / close_index) - mean_proportion) %>%
  ggplot(aes(x = date, y = rate, group = symbol, color = symbol)) +
  geom_line(size = 0.3) +
  labs(colour = '') +
  theme(legend.position = 'none') +
  geom_dl(aes(label = symbol), method = 'angled.boxes') +
  geom_abline(aes(intercept = 0, slope = 0), linetype = 'dashed')


dow_jones_historical_records %>%
  filter(symbol == 'CAT' | symbol == 'WBA' | symbol == 'MMM') %>%
  left_join(
    dow_jones_historical_records %>%
      filter(symbol == 'DIA') %>% 
      select(date, close) %>%
      setnames(old = c('close'), new = c('close_index')),
    by = 'date') %>%
  group_by(symbol) %>%
  mutate(mean_proportion = mean(close / close_index)) %>%
  ungroup() %>%
  mutate(rate = (close / close_index) - mean_proportion) %>%
  ggplot(aes(x = date, y = rate, group = symbol, color = symbol)) +
  geom_line(size = 0.3) +
  labs(colour = '') +
  theme(legend.position = 'none') +
  geom_dl(aes(label = symbol), method = 'angled.boxes') +
  geom_abline(aes(intercept = 0, slope = 0), linetype = 'dashed')



dow_jones_historical_records %>%
  filter(symbol != djia_symbol) %>%
  left_join(
    dow_jones_historical_records %>%
      filter(symbol == 'DIA') %>% 
      select(date, close) %>%
      setnames(old = c('close'), new = c('djia_close')),
    by = 'date') %>%
  group_by(symbol) %>%
  mutate(mean_djia_proportion = mean(close / djia_close)) %>%
  ungroup() %>%
  mutate(djia_ratio_change = (close / djia_close) - mean_djia_proportion) %>%
  ggplot(aes(x = date, y = djia_ratio_change,
             group = symbol, color = symbol)) +
  geom_line(size = 0.3) +
  labs(y = 'Change in ratio against DJIA', colour = '') +
  theme(legend.position = 'none') +
  geom_dl(aes(label = symbol), method = 'angled.boxes') +
  geom_hline(aes(yintercept = 0), linetype = 'dashed')


dow_jones_historical_records %>%
  filter(symbol != djia_symbol) %>%
  left_join(
    dow_jones_historical_records %>%
      filter(symbol == 'DIA') %>% 
      select(date, close) %>%
      setnames(old = c('close'), new = c('djia_close')),
    by = 'date') %>%
  group_by(symbol) %>%
  mutate(mean_djia_proportion = mean(close / djia_close)) %>%
  ungroup() %>%
  mutate(djia_ratio_change = (close / djia_close) - mean_djia_proportion) %>%
  ggplot(aes(x = date, y = djia_ratio_change,
             group = symbol, color = symbol)) +
  geom_line(size = 0.3) +
  labs(y = 'Change in ratio against DJIA', colour = '') +
  theme(legend.position = 'none') +
  geom_dl(aes(label = symbol), method = 'angled.boxes') +
  geom_hline(aes(yintercept = 0), linetype = 'dashed')


dow_jones_historical_records %>%
  filter(symbol == djia_symbol | symbol == 'CAT' | symbol == 'WBA' | symbol == 'UNH') %>%
  mutate(index = ifelse(symbol == djia_symbol, 'DJIA', 'Dow Jones stock'),
         # Hack alert: Prefixing the DJIA with 'zzz_' in order be plotted at the end
         symbol = ifelse(symbol == djia_symbol, 'DJIA', symbol)) %>%
  ggplot(aes(x = date, y = close, group = symbol, color = symbol, size = index)) +
  geom_line() +
  scale_color_manual(values = c(DJIA = 'black', CAT = 'red', WBA = 'blue', UNH = 'gray')) +
  scale_size_manual(values = c(0.5, 0.25)) +
  labs(colour = '') +
  theme(legend.position = 'none') +
  geom_dl(aes(label = symbol, color = symbol), method = 'last.polygons')


dow_jones_historical_records %>%
  filter(symbol != djia_symbol) %>%
  left_join(
    dow_jones_historical_records %>%
      filter(symbol == 'DIA') %>% 
      select(date, close) %>%
      setnames(old = c('close'), new = c('djia_close')),
    by = 'date') %>%
  ggplot(aes(x = date, y = close,
             group = symbol, color = symbol)) +
  geom_line(size = 0.3) +
  labs(y = 'Change in ratio against DJIA', colour = '') +
  theme(legend.position = 'none') +
  geom_dl(aes(label = symbol), method = 'angled.boxes') +
  geom_hline(aes(yintercept = 0), linetype = 'dashed')


# ------------------------

random_ticker_symbol <- sample(filter(dow_jones_stocks, symbol != 'DOW')$symbol , 1)
random_dates <- filter(dow_jones_historical_records, symbol == random_ticker_symbol)
idx_random_end_training <- sample(750:(nrow(random_dates) - 120), 1)
random_end_training <- random_dates[idx_random_end_training, 'date']
random_start_training <- random_dates[(idx_random_end_training - 750 + 1), 'date']

sets <- get_train_and_test_sets(dow_jones_historical_records,
                                random_ticker_symbol,
                                random_start_training,
                                random_end_training,
                                120)

rm(random_dates, random_end_training, random_start_training)

#----------------

lr_forecaster <- LinearRegressionStockForecaster(sets$training, sets$symbol)

training_preds <- lr_forecaster$predict(min(sets$training$date), max(sets$training$date))
preds <- lr_forecaster$predict(min(sets$test$date), max(sets$test$date))

#' Plots the predictions in comparison with the training and validation sets,
#' in order to provide a visualization of a model's prediction performance.
#'
#' @param sets A 
plot_predictions <- function(sets, predictions, training_predictions = NULL) {
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
    theme(legend.position = 'top')

  plot
}

plot_predictions(sets, preds, training_preds)
plot_predictions(sets, preds)

ggplot() +
  geom_line(data = training_preds, aes(x = date, y = close, color = 'Training Prediction')) +
  geom_line(data = sets$training, aes(x = date, y = close, color = 'Training Set')) +
  geom_line(data = sets$test, aes(x = date, y = close, color = 'Test Set')) +
  geom_line(data = preds, aes(x = date, y = close, color = 'Test Prediction')) +
  scale_color_manual(values = c('Training Set' = 'blue',
                                'Training Prediction' = 'cyan',
                                'Test Set' = 'green',
                                'Test Prediction' = 'red')) +
  labs(color = '') +
  theme(legend.position = 'bottom')


create_results_dataframe <- function() {
  data.frame('Method' = character(),
             'Number of Trading Days' = integer(), 
             'RMSE' = numeric(),
             stringsAsFactors = FALSE, check.names = FALSE)
}

results <- create_results_dataframe()

get_evaluation_results <- function(method, predictions) {
  eval_results <- create_results_dataframe()

  for (i in c(1, 5, 10, 20, 40, 60, 120)) {
    n <- nrow(eval_results) + 1
    eval_results[n, 'Method'] <- method
    eval_results[n, 'Number of Trading Days'] <- i
    eval_results[n, 'RMSE'] <- RMSE(predictions$close[1:i], sets$test$close[1:i])
    #c(method, i, RMSE(predictions$close[1:i], sets$test$close[1:i]))
  }

  results <<- rbind(results, eval_results)

  eval_results %>% select(-'Method')
}

get_evaluation_results('Linear Regression', preds)


for (i in c(1, 5, 10, 20, 40, 60, 120)) {
  results[(nrow(results) + 1), ] <-
    c('Linear Regression', i, RMSE(preds$close[1:i], sets$test$close[1:i]))
  #print(RMSE(preds$close[1:i], sets$test$close[1:i]))
}

# --------------------

ggplot() +
  geom_line(data = eval_sets$training, aes(x = date, y = close, color = 'Training Set')) +
  geom_line(data = eval_sets$test, aes(x = date, y = close, color = 'Test Set')) +
  scale_color_manual(values = c('Training Set' = 'blue', 'Test Set' = 'green')) +
  labs(color = '') +
  theme(legend.position = 'top')

# --------------------
results %>% spread('Number of trading days ahead', 'RMSE')

# --------------------

lr_forecaster <- LinearRegressionStockForecaster(eval_sets$training, eval_ticker_symbol)
lr_predictions <- lr_forecaster$predict(eval_test_start, eval_test_end)

plot_predictions(eval_sets, lr_predictions,
                 lr_forecaster$predict(eval_training_start, eval_training_end))

get_evaluation_results('Linear Regression', lr_predictions)


gam_forecaster <- GeneralizedAdditiveModelStockForecaster(eval_sets$training, eval_ticker_symbol)
gam_predictions <- gam_forecaster$predict(eval_test_start, eval_test_end)

plot_predictions(eval_sets, gam_predictions,
                 gam_forecaster$predict(eval_training_start, eval_training_end))

get_evaluation_results('GAM', gam_predictions)




