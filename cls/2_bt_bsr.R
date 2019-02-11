
rm(list = ls())

source("cls/1_pars.R")

# create cluster for parallel processing
cl <- makeCluster(12)
registerDoParallel(cl)

bsr1 <- foreach(n = 1:12, .packages = c("hts","forecast","Matrix")) %dopar% {
  
  dx <- fdate[n]
  
  lapply(fmethods, function(fx){
    
    if(fx == "arima"){
      
      # Run forecasting methods
      fcast <- RunBSR(object = window(tradegts_reduced2, end = dx-1/12), 
                      fmethod = fx,
                      h = 1*12,
                      shrinkage = "none",
                      series_to_be_shrunk = c())
      
      # Analyze forecast accuracy at different horizons
      test_wndw <- window(tradegts_reduced2, start = dx, end = dx+11/12)
      fcast_wndw = window(fcast, start = dx, end = dx+11/12)
      res <- accuracy.gts(fcast_wndw, test_wndw)
      
      asdf <- matrix(NA,6,13118)
      rownames(asdf) <- c("ME","RMSE","MAE","MAPE","MPE","MASE")
      colnames(asdf) <- colnames(aggts(tradegts_reduced2))
      out <- list(res,asdf,asdf)
      
    } else {
      
      asdf <- matrix(NA,6,13118)
      rownames(asdf) <- c("ME","RMSE","MAE","MAPE","MPE","MASE")
      colnames(asdf) <- colnames(aggts(tradegts_reduced2))
      out <- list(asdf,asdf,asdf)
      
    }
    
    names(out) <- 1:3
    return(out) 
    
  })
}


bsr2 <- foreach(n = 13:length(fdate), .packages = c("hts","forecast","Matrix")) %dopar% {
  
  dx <- fdate[n]
  
  lapply(fmethods, function(fx){
    
    if(fx == "arima"){
      
      # Run forecasting methods
      fcast <- RunBSR(object = window(tradegts_reduced2, end = dx-1/12), 
                      fmethod = fx,
                      h = 1*12,
                      shrinkage = "none",
                      series_to_be_shrunk = c())
      
      # Analyze forecast accuracy at different horizons
      test_wndw <- window(tradegts_reduced2, start = dx, end = dx+11/12)
      fcast_wndw = window(fcast, start = dx, end = dx+11/12)
      res <- accuracy.gts(fcast_wndw, test_wndw)
      
      asdf <- matrix(NA,6,13118)
      rownames(asdf) <- c("ME","RMSE","MAE","MAPE","MPE","MASE")
      colnames(asdf) <- colnames(aggts(tradegts_reduced2))
      out <- list(res,asdf,asdf)
      
    } else {
      
      asdf <- matrix(NA,6,13118)
      rownames(asdf) <- c("ME","RMSE","MAE","MAPE","MPE","MASE")
      colnames(asdf) <- colnames(aggts(tradegts_reduced2))
      out <- list(asdf,asdf,asdf)
      
    }
    
    names(out) <- 1:3
    return(out) 
    
  })
}

stopCluster(cl)

bsr <- c(bsr1,bsr2)
names(bsr) <- fdate
save(bsr, file = "out/results_bsr.Rdata")

