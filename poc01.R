if(!require(httr)) install.packages('httr')
if(!require(jsonlite)) install.packages('jsonlite')
if(!require(ggcorrplot)) install.packages('ggcorrplot')

library(tidyr)
library(tidyverse)
library(dplyr)
library(data.table)


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

prophet_model <- setnames(aapl_df, old = c('date', 'close'), new = c('ds', 'y')) %>%
  prophet(daily.seasonality = TRUE)


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
