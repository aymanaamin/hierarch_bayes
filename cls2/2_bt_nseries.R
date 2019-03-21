
rm(list = ls())

source("cls/1_pars.R")

# create cluster for parallel processing
cl <- makeCluster(length(fdate))
registerDoParallel(cl)

nseries <- foreach(n = 1:length(fdate), .packages = c("hts","forecast")) %dopar% {
  
  dx <- fdate[n]
  
  lapply(fmethods, function(fx){
    
    # Run forecasting methods
    fcast <- forecast(window(tradegts_reduced2, end = dx-1/12),
                      h = tail(horizons,1)*12,
                      method = "comb",
                      weights = "nseries",
                      fmethod = fx)
    
  })
}

stopCluster(cl)
names(nseries) <- fdate
save(nseries, file = "out/forecasts/forecasts_nseries.Rdata")



