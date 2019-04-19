rm(dow_jones_stocks, djia_symbol, iex_api_url_template,
   get_historical_records, get_dow_jones_dataframe,
   update_dow_jones_dataframe, dow_jones_historical_records)

rm(market_holidays, get_trading_days_in_range, get_trading_days_after)

rm(get_train_and_test_sets,
   eval_ticker_symbol, eval_sets,
   eval_training_start, eval_training_end,
   eval_test_start, eval_test_end)

rm(LinearRegressionStockForecaster,
   lr_forecaster, lr_predictions, lr_train_predictions)

rm(GeneralizedAdditiveModelStockForecaster,
   gam_forecaster, gam_predictions, gam_train_predictions)

rm(SupportVectorMachineStockForecaster,
   svm_forecaster, svm_predictions, svm_train_predictions)

rm(ArimaStockForecaster,
   arima_forecaster, arima_predictions)

rm(ProphetStockForecaster,
   prophet_forecaster, prophet_predictions, prophet_train_predictions)

rm(LongShortTermMemoryStockForecaster,
   lstm_forecaster, lstm_train_predictions, lstm_predictions, lstm_daily_predictions)
