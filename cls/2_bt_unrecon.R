
rm(list = ls())

source("cls/1_pars.R")

agg_gts <- as.list(aggts(tradegts_reduced2))

# create cluster for parallel processing
cl <- makeCluster(length(fdate))
registerDoParallel(cl)

unrecon <- foreach(n = 1:length(fdate), .packages = c("hts","forecast")) %dopar% {
  
  dx <- fdate[n]
  
  lapply(fmethods, function(fx){
    
    # Run forecasting methods
    fcasts_unrecon <- do.call(cbind, lapply(agg_gts, function(xs){
      
      library(forecast)
      
      if(fx == "rw"){
        
        rwf(window(xs, end = dx-1/12), h = tail(horizons,1)*12)$mean
        
      } else if(fx == "ets"){
        
        forecast(ets(window(xs, end = dx-1/12)), h = tail(horizons,1)*12, PI = FALSE)$mean
        
      } else if(fx == "arima"){
        
        forecast(auto.arima(window(xs, end = dx-1/12)),h = tail(horizons,1)*12)$mean
        
      }
    }))
    
    # Analyze forecast accuracy at different horizons
    out <- lapply(1:min(tail(fdate,1)-dx+1,tail(horizons,1)), function(hx){
      
      histy <- window(do.call(cbind,agg_gts), end = dx-1/12)
      x <- window(do.call(cbind,agg_gts), start = dx+hx-1, end = dx+hx-1/12)
      fcast <- window(fcasts_unrecon, start = dx+hx-1, end = dx+hx-1/12)
      res <- x-fcast
      
      pe <- res/x * 100
      me <- colMeans(res, na.rm = TRUE)
      rmse <- sqrt(colMeans(res^2, na.rm = TRUE))
      mae <- colMeans(abs(res), na.rm = TRUE)
      mape <- colMeans(abs(pe), na.rm = TRUE)
      mpe <- colMeans(pe, na.rm = TRUE)
      scale <- colMeans(abs(diff(histy, lag = max(1, stats::frequency(histy)))), 
                        na.rm = TRUE)
      q <- sweep(res, 2, scale, "/")
      mase <- colMeans(abs(q), na.rm = TRUE)
      out <- rbind(me, rmse, mae, mape, mpe, mase)
      rownames(out) <- c("ME", "RMSE", "MAE", "MAPE", "MPE", "MASE")
      colnames(out) <- colnames(fcast)
      
      return(out)
      
    })
    
    names(out) <- 1:min(tail(fdate,1)-dx+1,tail(horizons,1))
    return(out)
    
  })
}


stopCluster(cl)
names(unrecon) <- fdate
save(unrecon, file = "out/results_unrecon.Rdata")



