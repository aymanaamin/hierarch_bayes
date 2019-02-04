
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

library(MCMCpack)
library(hts)
library(Matrix)

source("lib/functions_model_test.R")

load("dat/tradegts_reduced2.Rdata")
load("dat/tradegts_reduced1.Rdata")
load("dat/tradehts_reduced.Rdata")


# 1. GET DATA --------------------------------------------------------------
xgts <- hts(y = aggts(infantgts, levels = 1)) # smallest hts
# xgts <- hts(y = aggts(tradegts_reduced1, levels = 1)/1e+6)  # small hts
# xgts <- tradehts_reduced$cat;xgts$bts <- xgts$bts/1e+6 # deep hts
# xgts <- tradehts_reduced$reg;xgts$bts <- xgts$bts/1e+6 # wide hts
# xgts <- tradegts_reduced1;xgts$bts <- xgts$bts/1e+6 # large gts
# xgts <- infantgts # small gts

# define training sample
h <-  2 # forecast horizon
test_date = 2002
xgts_training <- window(xgts, end = test_date)
xgts_test <- window(xgts, start = test_date+1/frequency(xgts$bts), end = test_date+h/frequency(xgts$bts))


# 2. RECONCILIATION --------------------------------------------------------

results <- RunBSR_test(object = xgts_training, 
                       fmethod = "arima",
                       h = h,
                       shrinkage = "none",
                       series_to_be_shrunk = c())


# 3. RESULTS ---------------------------------------------------------------

# Compare different reconciliation methods
start.time = Sys.time()
xgts_forecast_mint <- forecast(xgts_training, h = h, method = "comb", weights = "mint", fmethod = "arima")
print(Sys.time() - start.time); start.time = Sys.time() 
xgts_forecast_wls <- forecast(xgts_training, h = h, method = "comb", weights = "wls", fmethod = "arima")
print(Sys.time() - start.time); start.time = Sys.time() 
xgts_forecast_ols <- forecast(xgts_training, h = h, method = "comb", weights = "ols", fmethod = "arima")
print(Sys.time() - start.time)

# acc_bsr <- t(accuracy(results$forecast, xgts_test))
# acc_mint <- t(accuracy(xgts_forecast_mint, xgts_test))
# acc_wls <- t(accuracy(xgts_forecast_wls, xgts_test))
# acc_ols <- t(accuracy(xgts_forecast_ols, xgts_test))
# 
# comparison <- round(cbind(acc_bsr[,"MASE"],acc_mint[,"MASE"],acc_wls[,"MASE"],acc_ols[,"MASE"]),2)
# colnames(comparison) <- c("BSR","MinT","WLS","OLS")
# comparison

# Compare base forecast and reconciled forecasts
aux <- data.frame("Selective Shrinkage" = 1:results$pars$m %in% results$pars$series_to_be_shrunk,
                  "BSR" = round(rowMeans(do.call(rbind, lapply(as.list(aggts(results$forecast)), function(fx) fx[h]))),1),
                  "MinT" = round(rowMeans(do.call(rbind, lapply(as.list(aggts(xgts_forecast_mint)), function(fx) fx[h]))),1),
                  "WLS" = round(rowMeans(do.call(rbind, lapply(as.list(aggts(xgts_forecast_wls)), function(fx) fx[h]))),1),
                  "OLS" = round(rowMeans(do.call(rbind, lapply(as.list(aggts(xgts_forecast_ols)), function(fx) fx[h]))),1),
                  "Base Forecast Mean" = round(do.call(rbind, lapply(results$forecasts.list, function(fx) mean(fx[,h]))),1),
                  "Base Forecast SD" = round(do.call(rbind, lapply(results$forecasts.list, function(fx) sd(fx[,h]))),1))
aux

# cbind(round(results$results.list$alpha,1),aux$Base.Forecast.Mean-aux$BSR)
# round(cbind(results$results.list[[1]]$alpha,results$results.list[[1]]$M %*% results$results.list[[1]]$alpha),3)

