
rm(list = ls())

source("cls2/1_pars.R")

# create cluster for parallel processing
cl <- makeCluster(cores_l)
registerDoParallel(cl)

wls <- foreach(n = 1:length(fdate), .packages = c("hts","forecast")) %dopar% {
  
  dx <- fdate[n]
  
  lapply(fmethods, function(fx){
    
    if(fx == "arima"){
      
      # Run forecasting methods
      forecast(window(tradegts_reduced2, end = dx-1/12),
               h = horizon,
               method = "comb",
               weights = "wls",
               fmethod = fx)
    } else {
      
      NULL
      
    }
    
  })
}

stopCluster(cl)
names(wls) <- fdate
save(wls, file = "out/forecasts/forecasts_wls.Rdata")



