
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

library(MCMCpack)
library(hts)

source("lib/functions.R")

# Options
options(scipen=10)



# 1. GET DATA --------------------------------------------------------------

load("dat/swisstrade.Rdata")

n <- 1000 # draws
h <-  12 # forecast horizon

# define training sample
test_date = 2000
swisstrade_training <- window(swisstrade, end = test_date)
swisstrade_test <- window(swisstrade, start = test_date+1, end = test_date+h)

# get parameters
S <- smatrix(infantgts_training)
m <- nrow(S) # total series
q <- ncol(S) # bottom level series




# 2. CREATE FORECASTS ------------------------------------------------------

# Fit a  model to the data and simulate n different outcomes to create predictive densities
fc_list <- lapply(as.list(aggts(infantgts_training)), function(x) create_predictions(x, h, n))





# 3. RECONCILIATION --------------------------------------------------------

# A0 reflects our certainty about the reconciliation error being closer to a0

# define priors and hyperparameters
series_to_be_shrunk <- c(4)
lambda <- define_lambda(x = series_to_be_shrunk, nser_shr = 0, xser_shr = 1e+5)

# get in-sample reconciliation errors from the last 10 periods
a0_all <- get_prior_mean(x = infantgts_training, h, fmethod = "arima")
# a0_all[series_to_be_shrunk,] <- 0
a0_all[,] <- 0
# a0_all <- lambda %*% a0_all
# If we shrink some series towards their base 
# forecast, we better set a0 to zero for those series

# loop reconciliation over forecast horizon
results <- run_recon(x = fc_list, h = h)

# results$`details for h =  1`$beta




# 5. RESULTS ---------------------------------------------------------------

# get forecast gts, this is a bit hacky so far.. :)
bottom_forecasts <- t(do.call(cbind, lapply(results,function(x) x$beta)))
colnames(bottom_forecasts) <- colnames(infantgts_training$bts)
infantgts_forecast <- gts(ts(bottom_forecasts, start = test_date+1), groups = infantgts_training$groups)
infantgts_forecast$histy <- infantgts_training$bts
acc_bsr <- t(accuracy.gts(f = infantgts_forecast, test = infantgts_test))

# Compare different reconciliation methods
infantgts_forecast_mint <- forecast(infantgts_training, h = h, method = "comb", weights = "mint", fmethod = "arima")
infantgts_forecast_wls <- forecast(infantgts_training, h = h, method = "comb", weights = "wls", fmethod = "arima")
infantgts_forecast_ols <- forecast(infantgts_training, h = h, method = "comb", weights = "ols", fmethod = "arima")

acc_mint <- t(accuracy(infantgts_forecast_mint, infantgts_test))
acc_wls <- t(accuracy(infantgts_forecast_wls, infantgts_test))
acc_ols <- t(accuracy(infantgts_forecast_ols, infantgts_test))

comparison <- round(cbind(acc_bsr[,"MASE"],acc_mint[,"MASE"],acc_wls[,"MASE"],acc_ols[,"MASE"]),2)
colnames(comparison) <- c("BSR","MinT","WLS","OLS")
comparison


# Compare base forecast and reconciled forecasts
hor = 1
data.frame("Shrinkage" = 1*(1:m %in% series_to_be_shrunk),
           "BSR" = round(rowMeans(do.call(rbind, lapply(as.list(aggts(infantgts_forecast)), function(fx) fx[hor]))),1),
           "MinT" = round(rowMeans(do.call(rbind, lapply(as.list(aggts(infantgts_forecast_mint)), function(fx) fx[hor]))),1),
           "WLS" = round(rowMeans(do.call(rbind, lapply(as.list(aggts(infantgts_forecast_wls)), function(fx) fx[hor]))),1),
           "OLS" = round(rowMeans(do.call(rbind, lapply(as.list(aggts(infantgts_forecast_ols)), function(fx) fx[hor]))),1),
           "Base Forecast Mean" = round(do.call(rbind, lapply(fc_list, function(fx) mean(fx[,hor]))),1),
           "Base Forecast Variance" = round(do.call(rbind, lapply(fc_list, function(fx) var(fx[,hor]))),1))

