if(!require(httr)) install.packages('httr')
if(!require(jsonlite)) install.packages('jsonlite')
if(!require(ggcorrplot)) install.packages('ggcorrplot')

library(tidyr)
library(tidyverse)
library(dplyr)
library(data.table)
library(lubridate)


djia_tickers = c(
  'BA',   'PFE', 'MCD', 'WMT', 'KO',   'MRK',  'HD',   'V',   'JNJ',  'VZ',
  'CSCO', 'AXP', 'TRV', 'DIS', 'MSFT', 'UNH',  'DWDP', 'CAT', 'AAPL', 'UTX',
  'MMM',  'JPM', 'IBM', 'GS',  'XOM',  'INTC', 'NKE',  'CVX', 'PG',   'WBA')

iex_api_url_template <- 'https://api.iextrading.com/1.0/stock/%s/chart/%s'



get_historical_records <- function(ticker_symbol, period = '5y') {
  call_url <- sprintf(iex_api_url_template, ticker_symbol, period)
  resp <- GET(call_url)

  historical_prices <- as.data.frame(fromJSON(content(resp, 'text'), flatten = TRUE))
  historical_prices['symbol'] = ticker_symbol
  historical_prices$date <- as.Date(historical_prices$date, format='%Y-%m-%d')
  select(historical_prices, 'symbol', 'date', close)
}


get_djia_dataframe <- function(period = '5y') {

  historical_prices <- get_historical_records('DIA', period = period)

  for (tickers_symbol in djia_tickers) {
    historical_prices <- rbind(historical_prices,
                               get_historical_records(tickers_symbol, period = period))
  }

  historical_prices
}


update_djia_dataframe <- function(df) {
  last_recorded_day = max(df$date)
  today = Sys.Date()
  days_to_update = difftime(today, last_recorded_day, units="days")

  hist_period = '5y'
  if (days_to_update < 1) {
    return(df)
  } else if (days_to_update < 28) {
    hist_period = '1m'
  } else if (days_to_update < 365) {
    hist_period = '1y'
  }

  last_df = get_djia_dataframe(period = hist_period)

  df <- rbind(df, last_df)

  df[!duplicated(df[c('symbol', 'date')]),]
}


djia_historical_records <- get_djia_dataframe()

save(djia_historical_records, 'djia_dataframe.Rda')

djia_historical_records <- update_djia_dataframe(djia_historical_records)

save(djia_historical_records, file='djia_dataframe.Rda')

#--------------------------------

library(ggplot2)

djia_historical_records %>%
  filter(symbol != 'DIA') %>%
  ggplot(aes(x=date, y=close, group=symbol, color=symbol)) +
  geom_line()


djia_historical_records %>%
  ggplot(aes(x=date, y=close,
             colour=ifelse(symbol != 'DIA', 'Stock in Dow Jones', 'DJIA'),
             group=symbol)) +
  geom_line() +
  scale_color_manual(values = c('black', 'gray')) +
  labs(colour='') +
  theme(legend.position="top")


djia_historical_records %>%
  mutate(index = ifelse(symbol == 'DIA', 'DJIA', 'Stock in Dow Jones'),
         symbol = ifelse(symbol == 'DIA', 'z_DIA', symbol)) %>%
  ggplot(aes(x = date, y = close, group = symbol)) +
  geom_line(aes(color = index, size = index)) +
  scale_color_manual(values = c('black', 'gray')) +
  scale_size_manual(values = c(0.5, 0.25)) +
  labs(colour = '') +
  theme(legend.position = "top") +
  guides(size = FALSE)

djia_historical_records %>%
  filter(symbol == 'AAPL') %>%
  ggplot(aes(x=date, y=close)) +
  geom_line()

#--------------------------------
library(ggcorrplot)

djia_historical_records %>%
  mutate(symbol = ifelse(symbol != 'DIA', symbol, ' DIA')) %>%
  spread(symbol, close) %>% 
  select(-date) %>%
  cor(method = "pearson", use = "complete.obs") %>%
  ggcorrplot()

#--------------------------------

aapl_df <- djia_historical_records %>%
           filter(symbol == 'AAPL') %>%
           select(date, close)

m <- lm(close~date, aapl_df)

aapl_df %>%
  ggplot(aes(x = date, y = close)) +
  geom_line() +
  geom_abline(slope = coef(m)['date'], intercept = coef(m)['(Intercept)'])

#--------------------------------

if(!require(forecast)) install.packages('forecast')
library(forecast)

fit <- auto.arima(aapl_df %>% column_to_rownames(var = 'date') %>% .$close)

summary(fit)
checkresiduals(fit)
plot(forecast(fit, 60))
forecast(fit, 10)

#--------------------------------

if(!require(prophet)) install.packages('prophet')
library(prophet)

prophet_model <- aapl_df %>%
  setnames(old = c('date', 'close'), new = c('ds', 'y')) %>%
  prophet(daily.seasonality = TRUE)

future <- make_future_dataframe(prophet_model, periods = 60)
forecast <- predict(prophet_model, future)
plot(prophet_model, forecast)


#--------------------------------

market_holidays <- read.csv('market_holidays.csv') %>%
  mutate(date = as.Date(date, format='%Y-%m-%d'))

get_trading_days_in_range <- function(start_date, end_date) {
  data.frame('date' = seq(start_date, end_date, 'days')) %>%
    filter(!(wday(date) %in% c(1, 7))) %>%
    anti_join(market_holidays, by = 'date')
}

get_trading_days_after <- function(after_date, no_trading_days) {
  trading_days <- data.frame(date = as.Date(character()))
  counter <- 0
  current_date <- after_date
  while (counter < no_trading_days) {
    current_date <- current_date + ddays(1)
    #print(current_date)
    if (!(wday(current_date) %in% c(1, 7) | current_date %in% market_holidays$date)) {
      #print(current_date)
      counter <- counter + 1
      trading_days[counter,] <- c(current_date)
    }
  }

  trading_days
}

#x <- get_trading_days_in_range(as.Date('2019-04-01', '%Y-%m-%d'), as.Date('2019-05-30', '%Y-%m-%d'))

#--------------------------------

stock_symbol <- 'AAPL'
start_training <- as.Date('2015-07-01', '%Y-%m-%d')
end_training <- as.Date('2018-06-30', '%Y-%m-%d')
end_validation <- as.Date('2018-09-30', '%Y-%m-%d')

training_df <- djia_historical_records %>%
  filter(symbol == stock_symbol & date >= start_training & date <= end_training) %>%
  select(date, close)

validation_df <- djia_historical_records %>%
  filter(symbol == stock_symbol & date > end_training & date <= end_validation) %>%
  select(date, close)


ggplot() +
  geom_line(data = training_df, aes(x = date, y = close), color = 'blue') +
  geom_line(data = validation_df, aes(x = date, y = close), color = 'red')

#--------------------------------
#--------------------------------

load(file = 'djia_dataframe.Rda')

file_name <- sprintf('djia_stock_records_%s_%s.Rda',
                     min(djia_historical_records$date), max(djia_historical_records$date))

save(djia_historical_records, file=file_name)

load(file=file_name)

load(file="djia_stock_records_2014-04-04_2019-04-03.Rda")
load(file="djia_stock_records_2014-04-07_2019-04-04.Rda")

djia_historical_records <- update_djia_dataframe(djia_historical_records)

x <- read.csv('dow_jones_tickers.csv')