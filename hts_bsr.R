
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

library(MCMCpack)
library(hts)
library(Matrix)

source("lib/functions_model.R")



# 1. GET DATA --------------------------------------------------------------

load("dat/tradegts_reduced.Rdata")
xgts <- hts(y = aggts(tradegts_reduced, levels = 1))  # Reduced to regions lvl 1


# define training sample
h <-  1 # forecast horizon
test_date = 2002-1/12
xgts_training <- window(xgts, end = test_date)
xgts_test <- window(xgts, start = test_date+1/12, end = test_date+h/12)



# 2. RECONCILIATION --------------------------------------------------------

results <- RunBSR(object = xgts_training, 
                  fmethod = "arima",
                  h = h,
                  series_to_be_shrunk = c(1,5))



# 3. RESULTS ---------------------------------------------------------------

# Compare different reconciliation methods
xgts_forecast_bsr <- results$forecast
xgts_forecast_mint <- forecast(xgts_training, h = h, method = "comb", weights = "mint", fmethod = "arima")
xgts_forecast_wls <- forecast(xgts_training, h = h, method = "comb", weights = "wls", fmethod = "arima")
xgts_forecast_ols <- forecast(xgts_training, h = h, method = "comb", weights = "ols", fmethod = "arima")

acc_bsr <- t(accuracy(xgts_forecast_bsr, xgts_test))
acc_mint <- t(accuracy(xgts_forecast_mint, xgts_test))
acc_wls <- t(accuracy(xgts_forecast_wls, xgts_test))
acc_ols <- t(accuracy(xgts_forecast_ols, xgts_test))

comparison <- round(cbind(acc_bsr[,"MASE"],acc_mint[,"MASE"],acc_wls[,"MASE"],acc_ols[,"MASE"]),2)
colnames(comparison) <- c("BSR","MinT","WLS","OLS")
comparison


# Compare base forecast and reconciled forecasts
hor = 1
data.frame("Shrinkage" = 1*(1:results$pars$m %in% results$pars$series_to_be_shrunk),
           "BSR" = round(rowMeans(do.call(rbind, lapply(as.list(aggts(results$forecast)), function(fx) fx[hor])))/1e+6,1),
           "MinT" = round(rowMeans(do.call(rbind, lapply(as.list(aggts(xgts_forecast_mint)), function(fx) fx[hor])))/1e+6,1),
           "WLS" = round(rowMeans(do.call(rbind, lapply(as.list(aggts(xgts_forecast_wls)), function(fx) fx[hor])))/1e+6,1),
           "OLS" = round(rowMeans(do.call(rbind, lapply(as.list(aggts(xgts_forecast_ols)), function(fx) fx[hor])))/1e+6,1),
           "Base Forecast Mean" = round(do.call(rbind, lapply(results$base.forecasts, function(fx) mean(fx[,hor])))/1e+6,1),
           "Base Forecast SD" = round(do.call(rbind, lapply(results$base.forecasts, function(fx) sd(fx[,hor])))/1e+6,1))

