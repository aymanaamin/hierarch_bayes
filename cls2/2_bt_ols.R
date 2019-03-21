
rm(list = ls())

source("cls/1_pars.R")

# create cluster for parallel processing
cl <- makeCluster(length(fdate))
registerDoParallel(cl)

ols <- foreach(n = 1:length(fdate), .packages = c("hts","forecast")) %dopar% {
  
  dx <- fdate[n]
  
  lapply(fmethods, function(fx){
    
    # Run forecasting methods
    forecast(window(tradegts_reduced2, end = dx-1/12),
                      h = tail(horizons,1)*12,
                      method = "comb",
                      weights = "ols",
                      fmethod = fx)
    
  })
}

stopCluster(cl)
names(ols) <- fdate
save(ols, file = "out/forecasts/forecasts_ols.Rdata")



