
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

# Packages
library(hts)
library(LaF)
library(forecast)
library(data.table)
library(parallel)
library(ggplot2)

# Functions
source("lib/functions.R")

# Metadata
load("dat/countries.rda")

# Options
options(scipen=10)


# 1. IMPORT DATA -----------------------------------------------------------

total <- import_data()

total_exp <- total[substr(tsKey,1,1) == "E",]
total_exp_reg <- total_exp[, .(value = sum(value)), by = list(period, tsKey = substr(tsKey,2,5))]
total_exp_cat <- total_exp[, .(value = sum(value)), by = list(period, tsKey = substr(tsKey,6,14))]

dat_exp <- dt2ts(total_exp)
colnames(dat_exp) <-  substr(colnames(dat_exp),2,14)
dat_exp_reg <- dt2ts(total_exp_reg)
dat_exp_cat <- dt2ts(total_exp_cat)





# 2. GENERATE HIERARCHY ----------------------------------------------------

# define grouped hierarchy
tradegts <- gts(y = dat_exp,
                gnames = c("Regions Total", "Countries Total" ,"Goods Total Lvl 1","Goods Total Lvl 2",
                           "Goods Total Lvl 3","Goods Total Lvl 4","Goods Total Lvl 5","Goods Lvl 1 per Region",
                           "Goods Lvl 2 per Region","Goods Lvl 3 per Region","Goods Lvl 4 per Region",
                           "Goods Lvl 5 per Region","Goods Lvl 1 per Country","Goods Lvl 2 per Country",
                           "Goods Lvl 3 per Country","Goods Lvl 4 per Country"),
                characters = list(c(2,2), c(2,1,1,2,2)))

# define categories and countries hierarchies
tradehts <- list(cat = hts(y = dat_exp_cat, characters = c(2,1,1,2,2)),
                 reg = hts(y = dat_exp_reg, characters = c(2,2)))

# define reduced hierarchy for mint and wls estimations
agggts_red <- aggts(tradegts, levels = 10) # Reduced to regions lvl 2
colnames(agggts_red) <- substr(colnames(agggts_red), 24,40)
tradegts_reduced <- gts(y = agggts_red,
                        gnames = c("Regions Total", "Goods Total Lvl 1",
                                   "Goods Total Lvl 2","Goods Total Lvl 3",
                                   "Goods Lvl 1 per Region","Goods Lvl 2 per Region"),
                        characters = list(2, c(2,1,1)))

rm(dat_exp,dat_exp_reg,dat_exp_cat,total_exp,total_exp_reg,
   total_exp_cat,agggts_red,total)


# test <- aggts(tradegts, levels = 8) # Reduced to regions lvl 2
# colnames(test) <- substr(colnames(test), 24,40)
# swisstrade <- gts(y = test,
#                   gnames = c("Region", "Category"),
#                   characters = list(2, 2))


# 3. INSPECT DATA ----------------------------------------------------------

# aggregate
agg_gts <- as.list(aggts(tradegts))
agg_gts_red <- as.list(aggts(tradegts_reduced))
agg_hts_reg <- as.list(aggts(tradehts$reg))
agg_hts_cat <- as.list(aggts(tradehts$cat))

# check out some series
plot(agg_gts$Total/1e+9)
plot(agg_gts$'Goods Total Lvl 1/06') # Total Pharmaceutical products, very inelastic
plot(agg_gts$'Goods Total Lvl 1/09') # Total Machinery & Equipment, depends strongly on European business cycle and CHF exchange rate
plot(agg_gts$'Goods Lvl 1 per Country/AOAU11') # Watches to Australia
plot(agg_gts$'Goods Lvl 1 per Region/AF10') # Vehicles to Region 'Africa and Middle East'

# crosscheck to ensure the different hierarchies are aggregated correctly
all.equal(agg_hts_reg$Total,agg_gts$Total)
all.equal(agg_gts$Total,agg_gts_red$Total)
all.equal(agg_hts_reg$AOAU,agg_gts$`Countries Total/AOAU`)
all.equal(agg_hts_cat$`061`,agg_gts_red$`Goods Total Lvl 2/061`)
all.equal(agg_hts_reg$EU,agg_gts_red$`Regions Total/EU`)
all.equal(agg_hts_cat$`061101`,agg_gts$`Goods Total Lvl 4/061101`)

rm(agg_hts_reg,agg_hts_cat,agg_gts_red)





# 4. FORECAST BACKTESTS ----------------------------------------------------

# Define aggregation and forecasting methods over which to loop
method = c("mo" = "mo","tdgsa" = "tdgsa","tdgsf" = "tdgsf", "tdfp" = "tdfp")
fmethods <- c("ets" = "ets", "arima" = "arima", "rw" = "rw")
hierarchy = c("cat" = "cat", "reg" = "reg")
fdate <- 1998:2017; names(fdate) = fdate
horizons <- seq(1,3); names(horizons) = horizons # h = 1 means we have data until just before

# Create Cluster for parallel processing on windows
ncores = 30
cl <- makeCluster(ncores)
clusterExport(cl, varlist=c("tradegts","tradehts","tradegts_reduced",
                            "horizons","fdate","agg_gts"))

# 3.1 Run tests for basic methods (except bottom up)
results_basic <-  lapply(hierarchy, function(rx){
  lapply(method, function(mx){
    lapply(fmethods, function(fx){
      
      # Print status message
      message(sprintf("Processing %s reconciliation on %s hierarchy with %s 
                      forecasting method...", toupper(mx), rx, toupper(fx)))
      
      parLapply(cl, fdate, function(dx){
        
        library(forecast)
        
        lapply(horizons, function(hx){
          
          # Run forecasting and aggregation methods
          fcast <- forecast(window(tradehts[[rx]], end = dx-hx+11/12),
                            h = tail(horizons,1)*12,
                            method = mx,
                            level = 2,
                            fmethod = fx)
          
          # Analyze forecast accuracy at different horizons and return summary
          test_wndw <- window(tradehts[[rx]], start = dx, end = dx+11/12)
          fcast_wndw = window(fcast, start = dx, end = dx+11/12)
          accuracy.gts(fcast_wndw, test_wndw)
          
        })
      })
    })
  })
  })



# 3.2 Run tests for bottom up method
results_bu <- lapply(fmethods, function(fx){
  
  # Print status message
  message(sprintf("Processing bottom up aggregation with %s forecasting method...", toupper(fx)))
  
  parLapply(cl, fdate, function(dx){
    
    library(forecast)
    
    lapply(horizons, function(hx){
      
      # Run forecasting and aggregation methods
      fcast <- forecast(window(tradegts, end = dx-hx+11/12),
                        h = tail(horizons,1)*12,
                        method = "bu",
                        fmethod = fx)
      
      # Analyze forecast accuracy at different horizons and return summary
      test_wndw <- window(tradegts, start = dx, end = dx+11/12)
      fcast_wndw = window(fcast, start = dx, end = dx+11/12)
      accuracy.gts(fcast_wndw, test_wndw)
      
    })
  })
})



# 3.3 Run tests for combination methods
results_ols <- lapply(fmethods, function(fx){
  
  # Print status message
  message(sprintf("Processing OLS optimal combination with %s forecasting method...", toupper(fx)))
  
  parLapply(cl, fdate, function(dx){
    
    library(forecast)
    
    lapply(horizons, function(hx){
      
      # Run forecasting and aggregation methods
      fcast <- forecast(window(tradegts, end = dx-hx+11/12),
                        h = tail(horizons,1)*12, 
                        method = "comb",
                        weights = "ols",
                        fmethod = fx)
      
      # Analyze forecast accuracy at different horizons and return summary
      test_wndw <- window(tradegts, start = dx, end = dx+11/12)
      fcast_wndw = window(fcast, start = dx, end = dx+11/12)
      accuracy.gts(fcast_wndw, test_wndw)
      
    })
  })
})


results_wls <- lapply(fmethods, function(fx){
  
  # Print status message
  message(sprintf("Processing WLS optimal combination with %s forecasting method...", toupper(fx)))
  
  parLapply(cl, fdate, function(dx){
    
    library(forecast)
    
    lapply(horizons, function(hx){
      
      # Run forecasting and aggregation methods
      fcast <- forecast(window(tradegts_reduced, end = dx-hx+11/12),
                        h = tail(horizons,1)*12, 
                        method = "comb",
                        weights = "wls",
                        fmethod = fx)
      
      # Analyze forecast accuracy at different horizons and return summary
      test_wndw <- window(tradegts_reduced, start = dx, end = dx+11/12)
      fcast_wndw = window(fcast, start = dx, end = dx+11/12)
      accuracy.gts(fcast_wndw, test_wndw)
      
    })
  })
})


results_mint <- lapply(fmethods, function(fx){
  
  # Print status message
  message(sprintf("Processing MinT optimal combination with %s forecasting method...", toupper(fx)))
  
  parLapply(cl, fdate, function(dx){
    
    library(forecast)
    
    lapply(horizons, function(hx){
      
      # Run forecasting and aggregation methods
      fcast <- forecast(window(tradegts_reduced, end = dx-hx+11/12),
                        h = tail(horizons,1)*12, 
                        method = "comb",
                        weights = "mint",
                        fmethod = fx)
      
      # Analyze forecast accuracy at different horizons and return summary
      test_wndw <- window(tradegts_reduced, start = dx, end = dx+11/12)
      fcast_wndw = window(fcast, start = dx, end = dx+11/12)
      accuracy.gts(fcast_wndw, test_wndw)
      
    })
  })
})


results_nseries <- lapply(fmethods, function(fx){
  
  # Print status message
  message(sprintf("Processing nseries optimal combination with %s forecasting method...", toupper(fx)))
  
  parLapply(cl, fdate, function(dx){
    
    library(forecast)
    
    lapply(horizons, function(hx){
      
      # Run forecasting and aggregation methods
      fcast <- forecast(window(tradegts, end = dx-hx+11/12),
                        h = tail(horizons,1)*12, 
                        method = "comb",
                        weights = "nseries",
                        fmethod = fx)
      
      # Analyze forecast accuracy at different horizons and return summary
      test_wndw <- window(tradegts, start = dx, end = dx+11/12)
      fcast_wndw = window(fcast, start = dx, end = dx+11/12)
      accuracy.gts(fcast_wndw, test_wndw)
      
    })
  })
})



# 3.3 Run tests for no reconciliation
results_unreconciled <- lapply(fmethods, function(fx){
  
  # Print status message
  message(sprintf("Unreconciled estimation with %s forecasting method...", toupper(fx)))
  
  parLapply(cl, fdate, function(dx){
    
    library(forecast)
    
    lapply(horizons, function(hx){
      
      # Run forecasts methods
      fcasts_unrecon <- do.call(cbind, parLapply(cl, agg_gts, function(xs){
        
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
})


stopCluster(cl)



# 5. OUTPUT ---------------------------------------------------------------

results <- list("bu" = results_bu,
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

# save(results, file = "results_backtest.Rdata")


