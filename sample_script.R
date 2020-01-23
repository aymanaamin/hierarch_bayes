
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

library(hts)
library(bsr)



# 1. GET DATA --------------------------------------------------------------

# load("dat/tradegts_reduced1.Rdata")
# xgts <- tradegts_reduced1;xgts$bts <- xgts$bts/1e+6 # large gts
xgts <- infantgts # small gts

# define training sample
h <-  10 # forecast horizon
test_date = 1990
xgts_training <- window(xgts, end = test_date)
xgts_test <- window(xgts, start = test_date+1/frequency(xgts$bts), end = test_date+h/frequency(xgts$bts))




# 2. BSR -------------------------------------------------------------------

start.time = Sys.time() 
results <- bsr(object = xgts_training, 
               fmethod = "arima",
               h = h,
               shrinkage = NULL) # try shrinkage = "bu" or "td"
print(Sys.time() - start.time)


# 3. OTHERS ----------------------------------------------------------------

# Compare different reconciliation methods
start.time = Sys.time() 
xgts_forecast_mint <- forecast(xgts_training, h = h, method = "comb", weights = "mint", fmethod = "arima")
print(Sys.time() - start.time)
xgts_forecast_wls <- forecast(xgts_training, h = h, method = "comb", weights = "wls", fmethod = "arima")
xgts_forecast_ols <- forecast(xgts_training, h = h, method = "comb", weights = "ols", fmethod = "arima")



# 4. EVALUATION ------------------------------------------------------------

acc_bsr <- t(accuracy(results, xgts_test))
acc_mint <- t(accuracy(xgts_forecast_mint, xgts_test))
acc_wls <- t(accuracy(xgts_forecast_wls, xgts_test))
acc_ols <- t(accuracy(xgts_forecast_ols, xgts_test))

comparison <- round(cbind(acc_bsr[,"MASE"],acc_mint[,"MASE"],acc_wls[,"MASE"],acc_ols[,"MASE"]),2)
colnames(comparison) <- c("BSR","MinT","WLS","OLS")
comparison

# Compare base forecast and reconciled forecasts
data.frame("BSR" = round(rowMeans(do.call(rbind, lapply(as.list(aggts(results)), function(fx) fx[h]))),1),
           "MinT" = round(rowMeans(do.call(rbind, lapply(as.list(aggts(xgts_forecast_mint)), function(fx) fx[h]))),1),
           "WLS" = round(rowMeans(do.call(rbind, lapply(as.list(aggts(xgts_forecast_wls)), function(fx) fx[h]))),1),
           "OLS" = round(rowMeans(do.call(rbind, lapply(as.list(aggts(xgts_forecast_ols)), function(fx) fx[h]))),1),
           "base_mn" = round(do.call(rbind, lapply(results$base_forecasts, function(fx) mean(fx[,h]))),1),
           "base_sd" = round(do.call(rbind, lapply(results$base_forecasts, function(fx) sd(fx[,h])))))

