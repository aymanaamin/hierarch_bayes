
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

library(MCMCpack)
library(hts)

source("lib/functions.R")



# 1. GET DATA --------------------------------------------------------------

load("dat/swisstrade.Rdata")

swisstrade <- hts(y = aggts(swisstrade, levels = 1))  # Reduced to regions lvl 1

n <- 1000 # draws
h <-  12 # forecast horizon

# define training sample
test_date = 2000-1/12
swisstrade_training <- window(swisstrade, end = test_date)
swisstrade_test <- window(swisstrade, start = test_date+1/12, end = test_date+h/12)





# 3. RECONCILIATION --------------------------------------------------------

series_to_be_shrunk = c(1)

results <- run_recon(object = swisstrade_training, fmethod = "arima", h = h,
                     series_to_be_shrunk, nser_shr = 1, xser_shr = 1e+5)




# 5. RESULTS ---------------------------------------------------------------



# Compare different reconciliation methods
swisstrade_forecast_mint <- forecast(swisstrade_training, h = h, method = "comb", weights = "mint", fmethod = "arima")
swisstrade_forecast_wls <- forecast(swisstrade_training, h = h, method = "comb", weights = "wls", fmethod = "arima")
swisstrade_forecast_ols <- forecast(swisstrade_training, h = h, method = "comb", weights = "ols", fmethod = "arima")

acc_bsr <- t(accuracy(f = results$forecast, test = swisstrade_test))
acc_mint <- t(accuracy(swisstrade_forecast_mint, swisstrade_test))
acc_wls <- t(accuracy(swisstrade_forecast_wls, swisstrade_test))
acc_ols <- t(accuracy(swisstrade_forecast_ols, swisstrade_test))

comparison <- round(cbind(acc_bsr[,"MASE"],acc_mint[,"MASE"],acc_wls[,"MASE"],acc_ols[,"MASE"]),2)
colnames(comparison) <- c("BSR","MinT","WLS","OLS")
comparison


# Compare base forecast and reconciled forecasts
hor = 1
data.frame("Shrinkage" = 1*(1:results$pars$m %in% series_to_be_shrunk),
           "BSR" = round(rowMeans(do.call(rbind, lapply(as.list(aggts(results$forecast)), function(fx) fx[hor])))/1e+6,1),
           "MinT" = round(rowMeans(do.call(rbind, lapply(as.list(aggts(swisstrade_forecast_mint)), function(fx) fx[hor])))/1e+6,1),
           "WLS" = round(rowMeans(do.call(rbind, lapply(as.list(aggts(swisstrade_forecast_wls)), function(fx) fx[hor])))/1e+6,1),
           "OLS" = round(rowMeans(do.call(rbind, lapply(as.list(aggts(swisstrade_forecast_ols)), function(fx) fx[hor])))/1e+6,1),
           "Base Forecast Mean" = round(do.call(rbind, lapply(results$base, function(fx) mean(fx[,hor])))/1e+6,1),
           "Base Forecast SD" = round(do.call(rbind, lapply(results$base, function(fx) sd(fx[,hor])))/1e+6,1))

