
rm(list = ls())

source("cls/1_pars.R")

agg_gts <- as.list(aggts(tradegts_reduced2))

# create cluster for parallel processing
cl <- makeCluster(length(fdate))
registerDoParallel(cl)

unrecon <- foreach(n = 1:length(fdate), .packages = c("hts","forecast")) %dopar% {
  
  dx <- fdate[n]
  
  lapply(fmethods, function(fx){
    
    do.call(cbind, lapply(agg_gts, function(xs){
      
      library(forecast)
      
      if(fx == "rw"){
        
        rwf(window(xs, end = dx-1/12), h = tail(horizons,1)*12)$mean
        
      } else if(fx == "ets"){
        
        forecast(ets(window(xs, end = dx-1/12)), h = tail(horizons,1)*12, PI = FALSE)$mean
        
      } else if(fx == "arima"){
        
        forecast(auto.arima(window(xs, end = dx-1/12)),h = tail(horizons,1)*12)$mean
        
      }
      
    }))
    
  })
}


stopCluster(cl)
names(unrecon) <- fdate
save(unrecon, file = "out/forecasts_unrecon.Rdata")



