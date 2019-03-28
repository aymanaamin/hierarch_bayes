
rm(list = ls())

source("cls2/1_pars.R")

# create cluster for parallel processing
cl <- makeCluster(cores_s)
registerDoParallel(cl)

bsr <- foreach(n = 1:length(fdate), .packages = c("hts","forecast","Matrix")) %dopar% {
  
  dx <- fdate[n]
  
  lapply(fmethods, function(fx){
    
    if(fx == "arima"){
      
      # Run forecasting methods
      RunBSR(object = window(tradegts_reduced2, end = dx-1/12), 
             fmethod = fx,
             h = horizon,
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

