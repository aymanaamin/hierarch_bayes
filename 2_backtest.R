
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

# Packages
library(hts)
library(forecast)
library(data.table)
library(doParallel)
library(foreach)

# Functions
# source("lib/functions_model.R")

# Data
load("dat/countries.rda")
load("dat/tradehts_reduced.Rdata")
load("dat/tradegts_reduced1.Rdata")
load("dat/tradegts_reduced2.Rdata")
agg_gts <- as.list(aggts(tradegts_reduced2))


# 1. FORECAST BACKTESTS ----------------------------------------------------

# Define aggregation and forecasting methods over which to loop
method = c("mo" = "mo","tdgsa" = "tdgsa","tdgsf" = "tdgsf", "tdfp" = "tdfp")
fmethods <- c("ets" = "ets", "arima" = "arima", "rw" = "rw")
hierarchy = c("cat" = "cat", "reg" = "reg")
fdate <- 1998:2017; names(fdate) = fdate
horizons <- seq(1,3); names(horizons) = horizons # h = 1 means we have data until just before fdate





# Create Cluster for parallel processing
cl <- makeCluster(length(fdate))
registerDoParallel(cl)

results <- foreach(n = 1:length(fdate), .packages = c("hts","forecast")) %dopar% {
  
  dx <- fdate[n]
  
  # 3.1 Run tests for basic methods (except bottom up)
  results_basic <-  lapply(hierarchy, function(rx){
    lapply(method, function(mx){
      lapply(fmethods, function(fx){
        
        lapply(horizons, function(hx){
          
          # Run forecasting and aggregation methods
          fcast <- forecast(window(tradehts_reduced[[rx]], end = dx-hx+11/12),
                            h = tail(horizons,1)*12,
                            method = mx,
                            # parallel = T,
                            # num.cores = ncores,
                            level = 2,
                            fmethod = fx)
          
          # Analyze forecast accuracy at different horizons and return summary
          test_wndw <- window(tradehts_reduced[[rx]], start = dx, end = dx+11/12)
          fcast_wndw = window(fcast, start = dx, end = dx+11/12)
          accuracy.gts(fcast_wndw, test_wndw)
          
        })
      })
    })
  })
  
  
  
  # 3.2 Run tests for bottom up method
  results_bu <- lapply(fmethods, function(fx){
    
    lapply(horizons, function(hx){
      
      # Run forecasting and aggregation methods
      fcast <- forecast(window(tradegts_reduced2, end = dx-hx+11/12),
                        h = tail(horizons,1)*12,
                        # parallel = T,
                        # num.cores = ncores,
                        method = "bu",
                        fmethod = fx)
      
      # Analyze forecast accuracy at different horizons and return summary
      test_wndw <- window(tradegts_reduced2, start = dx, end = dx+11/12)
      fcast_wndw = window(fcast, start = dx, end = dx+11/12)
      accuracy.gts(fcast_wndw, test_wndw)
      
    })
  })
  
  
  
  # 3.3 Run tests for combination methods
  results_ols <- lapply(fmethods, function(fx){
    
    # Print status message
    message(sprintf("Processing OLS optimal combination with %s forecasting method...", toupper(fx)))
    
    lapply(horizons, function(hx){
      
      # Run forecasting and aggregation methods
      fcast <- forecast(window(tradegts_reduced2, end = dx-hx+11/12),
                        h = tail(horizons,1)*12,
                        # parallel = T,
                        # num.cores = ncores,
                        method = "comb",
                        weights = "ols",
                        fmethod = fx)
      
      # Analyze forecast accuracy at different horizons and return summary
      test_wndw <- window(tradegts_reduced2, start = dx, end = dx+11/12)
      fcast_wndw = window(fcast, start = dx, end = dx+11/12)
      accuracy.gts(fcast_wndw, test_wndw)
      
    })
  })
  
  
  results_wls <- lapply(fmethods, function(fx){
    
    lapply(horizons, function(hx){
      
      # Run forecasting and aggregation methods
      fcast <- forecast(window(tradegts_reduced1, end = dx-hx+11/12),
                        h = tail(horizons,1)*12,
                        # parallel = T,
                        # num.cores = ncores,
                        method = "comb",
                        weights = "wls",
                        fmethod = fx)
      
      # Analyze forecast accuracy at different horizons and return summary
      test_wndw <- window(tradegts_reduced1, start = dx, end = dx+11/12)
      fcast_wndw = window(fcast, start = dx, end = dx+11/12)
      accuracy.gts(fcast_wndw, test_wndw)
      
    })
  })
  
  
  results_mint <- lapply(fmethods, function(fx){
    
    # Print status message
    message(sprintf("Processing MinT optimal combination with %s forecasting method...", toupper(fx)))
    
    lapply(horizons, function(hx){
      
      # Run forecasting and aggregation methods
      fcast <- forecast(window(tradegts_reduced1, end = dx-hx+11/12),
                        h = tail(horizons,1)*12, 
                        # parallel = T,
                        # num.cores = ncores,
                        method = "comb",
                        weights = "mint",
                        fmethod = fx)
      
      # Analyze forecast accuracy at different horizons and return summary
      test_wndw <- window(tradegts_reduced1, start = dx, end = dx+11/12)
      fcast_wndw = window(fcast, start = dx, end = dx+11/12)
      accuracy.gts(fcast_wndw, test_wndw)
      
    })
  })
  
  
  results_nseries <- lapply(fmethods, function(fx){
    
    lapply(horizons, function(hx){
      
      # Run forecasting and aggregation methods
      fcast <- forecast(window(tradegts_reduced2, end = dx-hx+11/12),
                        h = tail(horizons,1)*12, 
                        # parallel = T,
                        # num.cores = ncores,
                        method = "comb",
                        weights = "nseries",
                        fmethod = fx)
      
      # Analyze forecast accuracy at different horizons and return summary
      test_wndw <- window(tradegts_reduced2, start = dx, end = dx+11/12)
      fcast_wndw = window(fcast, start = dx, end = dx+11/12)
      accuracy.gts(fcast_wndw, test_wndw)
      
    })
  })
  
  
  
  # 3.3 Run tests for no reconciliation
  results_unreconciled <- lapply(fmethods, function(fx){
    
    lapply(horizons, function(hx){
      
      # Run forecasts methods
      fcasts_unrecon <- do.call(cbind, lapply(agg_gts, function(xs){
        
        library(forecast)
        
        if(fx == "rw"){
          
          rwf(window(xs, end = dx-hx+11/12), h = tail(horizons,1)*12)$mean
          
        } else if(fx == "ets"){
          
          forecast(ets(window(xs, end = dx-hx+11/12)), h = tail(horizons,1)*12, PI = FALSE)$mean
          
        } else if(fx == "arima"){
          
          forecast(auto.arima(window(xs, end = dx-hx+11/12)),h = tail(horizons,1)*12)$mean
          
        }
      }))
      
      # Analyze forecast accuracy at different horizons and return summary
      histy <- window(do.call(cbind,agg_gts), end = dx-hx+11/12)
      x <- window(do.call(cbind,agg_gts), start = dx, end = dx+11/12)
      fcast <- window(fcasts_unrecon, start = dx, end = dx+11/12)
      res <- x-fcast
      
      pe <- res/x * 100
      me <- colMeans(res, na.rm = TRUE)
      rmse <- sqrt(colMeans(res^2, na.rm = TRUE))
      mae <- colMeans(abs(res), na.rm = TRUE)
      mape <- colMeans(abs(pe), na.rm = TRUE)
      mpe <- colMeans(pe, na.rm = TRUE)
      scale <- colMeans(abs(diff(histy, lag = max(1, stats::frequency(histy)))), 
                        na.rm = TRUE)
      q <- sweep(res, 2, scale, "/")
      mase <- colMeans(abs(q), na.rm = TRUE)
      out <- rbind(me, rmse, mae, mape, mpe, mase)
      rownames(out) <- c("ME", "RMSE", "MAE", "MAPE", "MPE", "MASE")
      colnames(out) <- colnames(fcast)
      
      return(out)
      
    })
  })
  
  # Collect and return
  out <- list("bu" = results_bu,
              "mo_cat" = results_basic$cat$mo,
              "mo_reg" = results_basic$reg$mo,
              "tdgsa_cat" = results_basic$cat$tdgsa,
              "tdgsa_reg" = results_basic$reg$tdgsa,
              "tdgsf_cat" = results_basic$cat$tdgsf,
              "tdgsf_reg" = results_basic$reg$tdgsf,
              "tdfp_cat" = results_basic$cat$tdfp,
              "tdfp_reg" = results_basic$reg$tdfp,
              "ols" = results_ols,
              "wls" = results_wls,
              "mint" = results_mint,
              "nseries" = results_nseries,
              "unrecon" = results_unreconciled)
  
  return(out)
  
}

names(results) <- fdate



# 5. OUTPUT ---------------------------------------------------------------

save(results, file = "out/results_backtest.Rdata")

# Create wide format table more suitable for plotting backtest results
fdates <- names(results)
rmethod <- names(results$`1998`)
fmethod <- names(results$`1998`$bu)
horizons <- names(results$`1998`$bu$ets)
levels <- unname(colnames(results$`1998`$bu$ets$`1`))
levels[grepl("/",levels)] <- sapply(strsplit(levels[grepl("/",levels)],"/"),`[`,2)
grid <- CJ(fdates,rmethod,fmethod,horizons)

# MASE
tab_mase = foreach(n = 1:nrow(grid), .combine = rbind) %do% {

  mase = results[[grid[[n,1]]]][[grid[[n,2]]]][[grid[[n,3]]]][[grid[[n,4]]]]["MASE",]
  names(mase)[grepl("/",names(mase))] <- sapply(strsplit(names(mase)[grepl("/",names(mase))],"/"),`[`,2)
  vapply(1:length(levels), function(cx) mase[levels[cx]], 1)

}

tab_mase <- cbind(grid,tab_mase)
colnames(tab_mase) <- c("date","recon","fmethod","horizon",levels)

save(tab_mase, file = "out/tab_mase.Rdata")



# RMSE
tab_rmse = foreach(n = 1:nrow(grid), .combine = rbind) %do% {
  
  rmse = results[[grid[[n,1]]]][[grid[[n,2]]]][[grid[[n,3]]]][[grid[[n,4]]]]["RMSE",]
  names(rmse)[grepl("/",names(rmse))] <- sapply(strsplit(names(rmse)[grepl("/",names(rmse))],"/"),`[`,2)
  vapply(1:length(levels), function(cx) rmse[levels[cx]], 1)
  
}

tab_rmse <- cbind(grid,tab_rmse)
colnames(tab_rmse) <- c("date","recon","fmethod","horizon",levels)

save(tab_rmse, file = "out/tab_rmse.Rdata")


stopCluster(cl)

