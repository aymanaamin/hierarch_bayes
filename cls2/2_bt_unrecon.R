
rm(list = ls())

source("cls2/1_pars.R")

agg_gts <- as.list(aggts(tradegts_reduced2))

# create cluster for parallel processing
cl <- makeCluster(cores_l)
registerDoParallel(cl)

unrecon <- foreach(n = 1:length(fdate), .packages = c("hts","forecast")) %dopar% {
  
  dx <- fdate[n]
  
  lapply(fmethods, function(fx){
    
    # Run forecasting methods
    do.call(cbind, lapply(agg_gts, function(xs){
      
      library(forecast)
      
      if(fx == "rw"){
        
        rwf(window(xs, end = dx-1/12), h = horizon)$mean
        
      } else if(fx == "ets"){
        
        forecast(ets(window(xs, end = dx-1/12)), h = horizon, PI = FALSE)$mean
        
      } else if(fx == "arima"){
        
        forecast(auto.arima(window(xs, end = dx-1/12)),h = horizon)$mean
        
      }
      
    }))
    
  })
}


stopCluster(cl)
names(unrecon) <- fdate
save(unrecon, file = "out/forecasts/forecasts_unrecon.Rdata")



