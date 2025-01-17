
rm(list = ls())

source("cls2/1_pars.R")

# create cluster for parallel processing
cl <- makeCluster(cores_l)
registerDoParallel(cl)

nseries <- foreach(n = 1:length(fdate), .packages = c("hts","forecast")) %dopar% {
  
  dx <- fdate[n]
  
  lapply(fmethods, function(fx){
    
    if(fx == "arima"){
      
      # Run forecasting methods
      fcast <- forecast(window(tradegts_reduced2, end = dx-1/12),
                        h = horizon,
                        method = "comb",
                        weights = "nseries",
                        fmethod = fx)
      
    } else {
      
      NULL
      
    }
    
  })
}

stopCluster(cl)
names(nseries) <- fdate
save(nseries, file = "out/forecasts/forecasts_nseries.Rdata")



