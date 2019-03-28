
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

source("cls2/1_pars.R")

# create cluster for parallel processing
cl <- makeCluster(cores_l)
registerDoParallel(cl)


bu <- foreach(n = 1:length(fdate), .packages = c("hts","forecast")) %dopar% {
  
  dx <- fdate[n]
  
  lapply(fmethods, function(fx){
    
    # Run forecasting methods
    forecast(window(tradegts_reduced2, end = dx-1/12),
                      h = horizon,
                      method = "bu",
                      fmethod = fx)
    
  })
}

stopCluster(cl)
names(bu) <- fdate
save(bu, file = "out/forecasts/forecasts_bu.Rdata")



