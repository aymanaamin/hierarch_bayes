
rm(list = ls())

source("cls/1_pars.R")
rm(tradegts_reduced1,tradehts_reduced)

# create cluster for parallel processing
cl <- makeCluster(length(fdate))
registerDoParallel(cl)

bsr <- foreach(n = 1:length(fdate), .packages = c("hts","forecast","Matrix")) %dopar% {
  
  dx <- fdate[n]
  
  lapply(fmethods, function(fx){
    
    tradegts_reduced2$bts <- tradegts_reduced2$bts/1e+6 
    
    # Run forecasting methods
    fcast <- RunBSR(object = window(tradegts_reduced2, end = dx-1/12), 
                    fmethod = fx,
                    h = tail(horizons,1)*12,
                    shrinkage = "none",
                    series_to_be_shrunk = c())
    
    tradegts_reduced2$bts <- tradegts_reduced2$bts*1e+6 
    fcast$bts <- fcast$bts*1e+6
    fcast$histy <- fcast$histy*1e+6
    
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
names(bsr) <- fdate
save(bsr, file = "out/results_bsr.Rdata")



