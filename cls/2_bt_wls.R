
rm(list = ls())

source("cls/1_pars.R")

# create cluster for parallel processing
cl <- makeCluster(length(fdate))
registerDoParallel(cl)

wls <- foreach(n = 1:length(fdate), .packages = c("hts","forecast")) %dopar% {
  
  dx <- fdate[n]
  
  lapply(fmethods, function(fx){
    
    # Run forecasting methods
    fcast <- forecast(window(tradegts_reduced2, end = dx-1/12),
                      h = tail(horizons,1)*12,
                      method = "comb",
                      weights = "wls",
                      fmethod = fx)
    
    # Analyze forecast accuracy at different horizons
    out <- lapply(1:min(tail(fdate,1)-dx+1,tail(horizons,1)), function(hx){
      
      test_wndw <- window(tradegts_reduced2, start = dx+hx-1, end = dx+hx-1/12)
      fcast_wndw = window(fcast, start = dx+hx-1, end = dx+hx-1/12)
      accuracy.gts(fcast_wndw, test_wndw)
      
    })
    
    names(out) <- 1:min(tail(fdate,1)-dx+1,tail(horizons,1))
    return(out)
    
  })
}

stopCluster(cl)
names(wls) <- fdate
save(wls, file = "out/results_wls.Rdata")



