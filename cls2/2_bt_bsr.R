
rm(list = ls())

source("cls/1_pars.R")

# create cluster for parallel processing
cl <- makeCluster(12)
registerDoParallel(cl)

bsr <- foreach(n = 1:length(fdate), .packages = c("hts","forecast","Matrix")) %dopar% {
  
  dx <- fdate[n]
  
  lapply(fmethods, function(fx){
    
    if(fx == "arima"){
      
      # Run forecasting methods
      RunBSR(object = window(tradegts_reduced2, end = dx-1/12), 
             fmethod = fx,
             h = tail(horizons,1)*12,
             shrinkage = "none",
             series_to_be_shrunk = c())
      
      
    } else {
      
      NULL
      
    }
  })
}


stopCluster(cl)

names(bsr) <- fdate
save(bsr, file = "out/forecasts/forecasts_bsr.Rdata")

