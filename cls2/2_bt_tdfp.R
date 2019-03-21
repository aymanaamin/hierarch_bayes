
rm(list = ls())

source("cls/1_pars.R")

# create cluster for parallel processing
cl <- makeCluster(length(fdate))
registerDoParallel(cl)

tdfp_cat <- foreach(n = 1:length(fdate), .packages = c("hts","forecast")) %dopar% {
  
  dx <- fdate[n]
  
  lapply(fmethods, function(fx){
    
    # Run forecasting methods
    forecast(window(tradehts_reduced$cat, end = dx-1/12),
             h = tail(horizons,1)*12,
             method = "tdfp",
             fmethod = fx)
    
  })
}


tdfp_reg <- foreach(n = 1:length(fdate), .packages = c("hts","forecast")) %dopar% {
  
  dx <- fdate[n]
  
  lapply(fmethods, function(fx){
    
    # Run forecasting methods
    forecast(window(tradehts_reduced$reg, end = dx-1/12),
             h = tail(horizons,1)*12,
             method = "tdfp",
             fmethod = fx)
    
  })
}


stopCluster(cl)
names(tdfp_cat) <- fdate
names(tdfp_reg) <- fdate
save(tdfp_cat, file = "out/forecasts/forecasts_tdfp_cat.Rdata")
save(tdfp_reg, file = "out/forecasts/forecasts_tdfp_reg.Rdata")




