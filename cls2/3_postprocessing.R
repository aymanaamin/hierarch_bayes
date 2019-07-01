
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

# Packages
library(hts)
library(forecast)
library(data.table)
library(doParallel)
library(foreach)
library(multDM)
library(dplyr)
library(tidyr)

load("dat/tradegts_reduced2.Rdata")
load("dat/tradehts_reduced.Rdata")
agg_gts <- as.list(aggts(tradegts_reduced2))


# 1. GET FORECASTS --------------------------------------------------------

recon <- c("bsr","bu","mo_cat","mo_reg","tdfp_cat","tdfp_reg","ols","wls","mint","nseries","unrecon")
for(ix in recon) load(sprintf("L:/Groups/Economic Forecasting/Users/FlorianEckert/bsr/forecasts_%s.Rdata",ix))
fcasts <- lapply(recon, function(ix) eval(parse(text=ix)))
fmethods <- c("ets" = "ets", "arima" = "arima", "rw" = "rw")
fdates <- seq(1995,2015+11/12,1/12); names(fdates) = fdates
names(fcasts) <- recon
horizon <- 36
rm(list = recon,ix)

hx = 3

results <- lapply(recon, function(rx){
  
  lapply(fdates, function(dx){
    
    dx <- as.numeric(dx)
    
    lapply(fmethods, function(fx){
      
      if(rx == "unrecon"){
        
        histy <- window(do.call(cbind,agg_gts), end = dx-1/12)
        x <- window(do.call(cbind,agg_gts), start = dx, end = dx+hx-1/12)
        fcast <- window(fcasts[[rx]][[as.character(dx)]][[fx]], start = dx, end = dx+hx-1/12)
        res <- x - fcast
        
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
        
        
      } else if(rx %in% c("mo_cat","tdfp_cat")){
        
        fcast_wndw = window(fcasts[[rx]][[as.character(dx)]][[fx]], start = dx, end = dx+hx-1/12)
        test_wndw <- window(tradehts_reduced$cat, start = dx, end = dx+hx-1/12)
        accuracy.gts(fcast_wndw, test_wndw)
        
      } else if(rx %in% c("mo_reg","tdfp_reg")){
        
        fcast_wndw = window(fcasts[[rx]][[as.character(dx)]][[fx]], start = dx, end = dx+hx-1/12)
        test_wndw <- window(tradehts_reduced$reg, start = dx, end = dx+hx-1/12)
        accuracy.gts(fcast_wndw, test_wndw)
        
      } else {
        
        if(fx == "arima"){
          
          fcast_wndw = window(fcasts[[rx]][[as.character(dx)]][[fx]], start = dx, end = dx+hx-1/12)
          test_wndw <- window(tradegts_reduced2, start = dx, end = dx+hx-1/12)
          accuracy.gts(fcast_wndw, test_wndw)  
          
        } else { 
          NULL 
        }
      }
    })
  })
})

names(results) <- recon

save(results, file = "out/results.Rdata")



# 3. CREATE TABLES --------------------------------------------------------

# Cluster
# cl <- makeCluster(4)
# registerDoParallel(cl)

# Create wide format table more suitable for plotting backtest results
levels <- unname(colnames(results$ols$`1998`$arima))
levels[grepl("/",levels)] <- sapply(strsplit(levels[grepl("/",levels)],"/"),`[`,2)
grid <- CJ(recon,names(fdates),fmethods)

# Create table for each forecast accuracy measure
acc_measures <- c("RMSE","MAPE","MASE")
tabs <- lapply(acc_measures, function(tx){
  
  tab_all <- foreach(n = 1:nrow(grid), .combine = rbind) %do% {
    
    tab = results[[grid[[n,1]]]][[grid[[n,2]]]][[grid[[n,3]]]][tx,]
    if(is.null(tab)){
      
      rep(NA,length(levels))
      
    } else {
      
      names(tab)[grepl("/",names(tab))] <- sapply(strsplit(names(tab)[grepl("/",names(tab))],"/"),`[`,2)
      vapply(1:length(levels), function(cx) tab[levels[cx]], 1)
      
    }
  }
  
  tab_named <- cbind(grid,tab_all)
  colnames(tab_named) <- c("recon","date","fmethod",levels)
  tab <- as_tibble(tab_named)
  tab <- tab %>% filter(date > 1997)
  
  return(tab)
  
})

# stopCluster(cl)

names(tabs) <- acc_measures
save(tabs, file = "out/tabs.Rdata")

# ME     Mean Error
# RMSE   Root Mean Square Error
# MAE    Mean Absolute Error
# MAPE   Mean Absolute Percentage Error
# MPE    Mean Percentage Error
# MASE   Mean Absolute Scaled Error





# DIEBOLD MARIANO STATS ---------------------------------------------------


dm <- lapply(recon[-(which(recon == "unrecon"))], function(rx){
  
  horz <- sapply(1:36, function(hx){
    
    f_unrecon <- sapply(names(fdates), function(dx) t(fcasts[["unrecon"]][[dx]][["arima"]])[1,hx])
    f_recon <- sapply(names(fdates), function(dx) t(aggts(fcasts[[rx]][[dx]][["arima"]]))[1,hx])
    realiz <- sapply(names(fdates), function(dx) as.numeric(window(agg_gts[[1]], start = as.numeric(dx))[hx]))
    
    # For alternative="greater", the alternative hypothesis is that method 2 is more accurate than method 1.
    out <- dm.test(e1 = f_unrecon - realiz,
                   e2 = f_recon - realiz,
                   alternative = "greater",
                   h = hx,
                   power = 2)
    
    return(out$p.value)
    
  })
  
  names(horz) <- 1:36
  return(horz)
  
})

names(dm) <- recon[-(which(recon == "unrecon"))]

save(dm, file = "out/dm.Rdata")


