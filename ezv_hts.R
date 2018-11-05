
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

# Packages
library(hts)
library(LaF)
library(forecast)
library(data.table)

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





# 3. INSPECT DATA ----------------------------------------------------------

# aggregate
agg_gts <- as.list(aggts(tradegts))
agg_hts_reg <- as.list(aggts(tradehts$reg))
agg_hts_cat <- as.list(aggts(tradehts$cat))

# check out some series
plot(agg_gts$Total/1e+9)
plot(agg_gts$'Goods Total Lvl 1/06') # Total Pharmaceutical products, very inelastic
plot(agg_gts$'Goods Total Lvl 1/09') # Total Machinery & Equipment, depends strongly on European business cycle and CHF exchange rate
plot(agg_gts$'Goods Lvl 1 per Country/AOAU11') # Watches to Australia
plot(agg_gts$'Goods Lvl 1 per Region/AF10') # Vehicles to Region 'Africa and Middle East'

# crosscheck
all.equal(agg_hts_reg$Total,agg_gts$Total)
all.equal(agg_hts_reg$AOAU,agg_gts$`Regions Total/AOAU`)
all.equal(agg_hts_cat$`061101`,agg_gts$`Goods Total Lvl 4/061101`)




# 4. FORECAST BACKTESTS ----------------------------------------------------

# Create Cluster for parallel processing on windows
ncores = 30
cl <- makeCluster(ncores)
clusterExport(cl, varlist=c("tradegts")) 

# Define aggregation and forecasting methods over which to loop
method = c("mo" = "mo","tdgsa" = "tdgsa","tdgsf" = "tdgsf", "tdfp" = "tdfp")
fmethods <- c("ets" = "ets", "arima" = "arima", "rw" = "rw")
hierarchy = c("cat" = "cat", "reg" = "reg")
weights <-  c("wls" = "wls", "ols" = "ols",
              "mint" = "mint", "nseries" = "nseries")
fdate <- seq(2000,2015,2); names(fdate) = fdate
horizons <- seq(1,3); names(horizons) = horizons

# 3.1 Run tests for basic methods (except bottom up)
results_basic <- lapply(method, function(mx){
  lapply(fmethods, function(fx){
    
    # Print status message
    message(sprintf("Processing %s aggregation with %s forecasting method...", toupper(mx), toupper(fx)))
    
    lapply(hierarchy, function(rx){
      
      lapply(fdate, function(dx){
        
        # Run forecasting and aggregation methods
        fcast <- forecast(window(tradehts[[rx]], end = dx-1/12),
                          h = tail(horizons,1)*12, 
                          parallel = T,
                          num.cores = 30,
                          method = mx,
                          level = 2,
                          fmethod = fx)
        
        lapply(horizons, function(hx){
          
          # Analyze forecast accuracy at different horizons and return summary
          test_wndw <- window(tradehts[[rx]], start = dx+hx-1, end = dx+hx-1/12)
          fcast_wndw = window(fcast, start = dx+hx-1, end = dx+hx-1/12)
          accuracy.gts(fcast_wndw, test_wndw)
          
        })        
      })
    })
  })
})



# 3.2 Run tests for bottom up method 
result_bu <- lapply(fmethods, function(fx){
  
  # Print status message
  message(sprintf("Processing bottom up aggregation with %s forecasting method...", toupper(fx)))
  
  lapply(fdate, function(dx){
    
    # Run forecasting and aggregation methods
    fcast <- forecast(window(tradegts, end = dx-1/12),
                      h = tail(horizons,1)*12, 
                      parallel = T,
                      num.cores = 30,
                      method = "bu",
                      fmethod = fx)
    
    lapply(horizons, function(hx){
      
      # Analyze forecast accuracy at different horizons and return summary
      test_wndw <- window(tradegts, start = dx+hx-1, end = dx+hx-1/12)
      fcast_wndw = window(fcast, start = dx+hx-1, end = dx+hx-1/12)
      accuracy.gts(fcast_wndw, test_wndw)
      
    })
  })
})




# 3.3 Run tests for combination methods
results_comb <- lapply(weights, function(wx){
  lapply(fmethods, function(fx){
    
    # Print status message
    message(sprintf("Processing optimal combination with %s weights and %s forecasting method...", toupper(wx), toupper(fx)))
    
    lapply(fdate, function(dx){
      
      # Run forecasting and aggregation methods
      fcast <- forecast(window(tradegts, end = dx-1/12),
                        h = tail(horizons,1)*12, 
                        parallel = T,
                        num.cores = 30,
                        method = "comb",
                        weights = wx,
                        fmethod = fx)
      
      lapply(horizons, function(hx){
        
        # Analyze forecast accuracy at different horizons and return summary
        test_wndw <- window(tradegts, start = dx+hx-1, end = dx+hx-1/12)
        fcast_wndw = window(fcast, start = dx+hx-1, end = dx+hx-1/12)
        accuracy.gts(fcast_wndw, test_wndw)
        
      })
    })
  })
})

stopCluster(cl)

