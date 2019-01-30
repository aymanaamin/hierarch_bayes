
rm(list = ls())

source("cls/1_pars.R")

# create cluster for parallel processing
cl <- makeCluster(length(fdate))
registerDoParallel(cl)


mo_cat <- foreach(n = 1:length(fdate), .packages = c("hts","forecast")) %dopar% {
  
  dx <- fdate[n]
  
  lapply(fmethods, function(fx){
    
    # Run forecasting methods
    fcast <- forecast(window(tradehts_reduced$cat, end = dx-1/12),
                      h = tail(horizons,1)*12,
                      method = "mo",
                      level = 1,
                      fmethod = fx)
    
    # Analyze forecast accuracy at different horizons
    out <- lapply(1:min(tail(fdate,1)-dx+1,tail(horizons,1)), function(hx){
      
      test_wndw <- window(tradehts_reduced$cat, start = dx+hx-1, end = dx+hx-1/12)
      fcast_wndw = window(fcast, start = dx+hx-1, end = dx+hx-1/12)
      accuracy.gts(fcast_wndw, test_wndw)
      
    })
    
    names(out) <- 1:min(tail(fdate,1)-dx+1,tail(horizons,1))
    return(out)
    
  })
}



mo_reg <- foreach(n = 1:length(fdate), .packages = c("hts","forecast")) %dopar% {
  
  dx <- fdate[n]
  
  lapply(fmethods, function(fx){
    
    # Run forecasting methods
    fcast <- forecast(window(tradehts_reduced$reg, end = dx-1/12),
                      h = tail(horizons,1)*12,
                      method = "mo",
                      level = 1,
                      fmethod = fx)
    
    # Analyze forecast accuracy at different horizons
    out <- lapply(1:min(tail(fdate,1)-dx+1,tail(horizons,1)), function(hx){
      
      test_wndw <- window(tradehts_reduced$reg, start = dx+hx-1, end = dx+hx-1/12)
      fcast_wndw = window(fcast, start = dx+hx-1, end = dx+hx-1/12)
      accuracy.gts(fcast_wndw, test_wndw)
      
    })
    
    names(out) <- 1:min(tail(fdate,1)-dx+1,tail(horizons,1))
    return(out)
    
  })
}


stopCluster(cl)
names(mo_cat) <- fdate
names(mo_reg) <- fdate
save(mo_cat, file = "out/results_mo_cat.Rdata")
save(mo_reg, file = "out/results_mo_reg.Rdata")




