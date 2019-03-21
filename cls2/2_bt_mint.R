
rm(list = ls())

source("cls/1_pars.R")
# rm(tradegts_reduced1,tradehts_reduced)

tradegts_reduced2$bts[which(tradegts_reduced2$bts == 0)] <- rlnorm(length(which(tradegts_reduced2$bts == 0)),0,1)*10

# create cluster for parallel processing
cl <- makeCluster(length(fdate))
registerDoParallel(cl)

mint <- foreach(n = 1:length(fdate), .packages = c("hts","forecast")) %dopar% {
  
  dx <- fdate[n]
  
  lapply(fmethods, function(fx){
    
    if(fx == "arima"){
      
      # Run forecasting methods
      forecast(window(tradegts_reduced2, end = dx-1/12),
               h = tail(horizons,1)*12,
               method = "comb",
               weights = "mint",
               fmethod = fx)
      
    } else {
      
      NULL
      
    }
  })
}

stopCluster(cl)
names(mint) <- fdate
save(mint, file = "out/forecasts/forecasts_mint.Rdata")



