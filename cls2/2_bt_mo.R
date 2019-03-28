
rm(list = ls())

source("cls2/1_pars.R")

# create cluster for parallel processing
cl <- makeCluster(cores_l)
registerDoParallel(cl)


mo_cat <- foreach(n = 1:length(fdate), .packages = c("hts","forecast")) %dopar% {
  
  dx <- fdate[n]
  
  lapply(fmethods, function(fx){
    
    # Run forecasting methods
    forecast(window(tradehts_reduced$cat, end = dx-1/12),
             h = horizon,
             method = "mo",
             level = 1,
             fmethod = fx)
    
  })
}



mo_reg <- foreach(n = 1:length(fdate), .packages = c("hts","forecast")) %dopar% {
  
  dx <- fdate[n]
  
  lapply(fmethods, function(fx){
    
    # Run forecasting methods
    forecast(window(tradehts_reduced$reg, end = dx-1/12),
             h = horizon,
             method = "mo",
             level = 1,
             fmethod = fx)
    
    
  })
}


stopCluster(cl)
names(mo_cat) <- fdate
names(mo_reg) <- fdate
save(mo_cat, file = "out/forecasts/forecasts_mo_cat.Rdata")
save(mo_reg, file = "out/forecasts/forecasts_mo_reg.Rdata")




