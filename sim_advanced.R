
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

library(MCMCpack)
library(hts)

source("lib/functions.R")

# Options
options(scipen=10)



# 1. GET DATA --------------------------------------------------------------

n <- 1000 # draws
h <-  5 # forecast horizon

# define training sample
test_date = 1994
infantgts_training <- window(infantgts, end = test_date)
infantgts_test <- window(infantgts, start = test_date+1, end = test_date+h)

# get parameters
S <- smatrix(infantgts_training)
m <- nrow(S) # total series
q <- ncol(S) # bottom level series




# 2. CREATE FORECASTS ------------------------------------------------------

# Fit an ETS model to the data and simulate n different outcomes to create predictive densities
fc_list <- lapply(as.list(aggts(infantgts_training)), function(x) create_predictions(x, h, n))





# 3. RECONCILIATION --------------------------------------------------------

# A0 reflects our certainty about the reconciliation error being closer to a0

# define priors and hyperparameters
series_to_be_shrunk <- c(1)
lambda <- define_lambda(x = series_to_be_shrunk, 
                        nser_shr = 1, xser_shr = 1e+2)

# get in-sample reconciliation errors from the last 10 periods
a0_all <- get_prior_mean(x = infantgts_training, h, fmethod = "ets")
a0_all[series_to_be_shrunk,] <- 0 # If we shrink some series towards their base 
# forecast, we better set a0 to zero for those series

# loop reconciliation over forecast horizon
results <- run_recon(x = fc_list, h = h)

# results$`details for h =  1`$beta




# 5. RESULTS ---------------------------------------------------------------

# get forecast gts, this is a bit hacky so far.. :)
base_forecasts <- t(do.call(cbind, lapply(results,function(x) x$beta)))
colnames(base_forecasts) <- colnames(infantgts_training$bts)
infantgts_forecast <- gts(ts(base_forecasts, start = test_date+1), groups = infantgts_training$groups)
infantgts_forecast$histy <- infantgts_training$bts
acc_bsr <- t(accuracy.gts(f = infantgts_forecast, test = infantgts_test))

# Compare
infantgts_forecast_other <- forecast(infantgts_training, h = h, method = "comb", weights = "mint", fmethod = "ets")
acc_other <- t(accuracy(infantgts_forecast_other, infantgts_test))

round(cbind(acc_bsr,acc_other),2)
