
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

# Packages
library(hts)
library(forecast)
library(data.table)
library(doParallel)
library(foreach)
library(tidyverse)
library(multDM)
library(sp)

load("dat/tradegts_reduced2.Rdata")
load("dat/tradehts_reduced.Rdata")
agg_gts <- as.list(aggts(tradegts_reduced2))


# 1. GET FORECASTS --------------------------------------------------------

recon <- c("bsr","bu","mo_cat","mo_reg","tdfp_cat","tdfp_reg","ols","wls","mint","nseries","unrecon")
for(ix in recon) load(sprintf("out/forecasts/forecasts_%s.Rdata",ix))
fcasts <- lapply(recon, function(ix) eval(parse(text=ix)))
names(fcasts) <- recon
rm(list = recon,recon,ix)




# 2. COMPUTE ACCURACY MEASURES --------------------------------------------

rmethod <- names(fcasts); names(rmethod) <- names(fcasts)
fdates <- names(fcasts$bu); names(fdates) <-  names(fcasts$bu)
fmethods <- names(fcasts$bu$`1998`); names(fmethods) <- names(fcasts$bu$`1998`)
horizons <- 1:3; names(horizons) <- 1:3

results <- lapply(rmethod, function(rx){
  
  lapply(fdates, function(dx){
    
    dx <- as.numeric(dx)
    
    lapply(fmethods, function(fx){
      
      # Analyze forecast accuracy at different horizons
      out <- lapply(1:min(as.numeric(tail(fdates,1))-dx+1,tail(horizons,1)), function(hx){
        
        if(rx == "unrecon"){
          
          histy <- window(do.call(cbind,agg_gts), end = dx-1/12)
          x <- window(do.call(cbind,agg_gts), start = dx+hx-1, end = dx+hx-1/12)
          fcast <- window(fcasts[[rx]][[as.character(dx)]][[fx]], start = dx+hx-1, end = dx+hx-1/12)
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
          
          
        } else if(rx %in% c("mo_cat","tdfp_cat")){
          
          fcast_wndw = window(fcasts[[rx]][[as.character(dx)]][[fx]], start = dx+hx-1, end = dx+hx-1/12)
          test_wndw <- window(tradehts_reduced$cat, start = dx+hx-1, end = dx+hx-1/12)
          accuracy.gts(fcast_wndw, test_wndw)
          
        } else if(rx %in% c("mo_reg","tdfp_reg")){
          
          fcast_wndw = window(fcasts[[rx]][[as.character(dx)]][[fx]], start = dx+hx-1, end = dx+hx-1/12)
          test_wndw <- window(tradehts_reduced$reg, start = dx+hx-1, end = dx+hx-1/12)
          accuracy.gts(fcast_wndw, test_wndw)
          
        } else {
          
          fcast_wndw = window(fcasts[[rx]][[as.character(dx)]][[fx]], start = dx+hx-1, end = dx+hx-1/12)
          test_wndw <- window(tradegts_reduced2, start = dx+hx-1, end = dx+hx-1/12)
          accuracy.gts(fcast_wndw, test_wndw)  
          
        }
      })
      
      names(out) <- 1:min(as.numeric(tail(fdates,1))-dx+1,tail(horizons,1))
      return(out)
      
    })
  })
})








# 3. CREATE TABLES --------------------------------------------------------

# Cluster
# cl <- makeCluster(4)
# registerDoParallel(cl)

# Create wide format table more suitable for plotting backtest results
levels <- unname(colnames(results$ols$`1998`$ets$`1`))
levels[grepl("/",levels)] <- sapply(strsplit(levels[grepl("/",levels)],"/"),`[`,2)
grid <- CJ(rmethod,fdates,fmethod,horizons)
grid <- grid[!(V2 == 2018 & V4 %in% c(3,2)),]
grid <- grid[!(V2 == 2017 & V4 %in% c(3)),]

# Create table for each forecast accuracy measure
acc_measures <- c("RMSE","MAPE","MASE")
tabs <- lapply(acc_measures, function(tx){
  
  tab_all <- foreach(n = 1:nrow(grid), .combine = rbind) %do% {
    
    tab = results[[grid[[n,1]]]][[grid[[n,2]]]][[grid[[n,3]]]][[grid[[n,4]]]][tx,]
    names(tab)[grepl("/",names(tab))] <- sapply(strsplit(names(tab)[grepl("/",names(tab))],"/"),`[`,2)
    vapply(1:length(levels), function(cx) tab[levels[cx]], 1)
    
  }
  
  tab_named <- cbind(grid,tab_all)
  colnames(tab_named) <- c("recon","date","fmethod","horizon",levels)
  tab <- as_tibble(tab_named)
  tab$date <- as.integer(tab$date) + as.integer(tab$horizon) - 1
  tab$horizon <- as.integer(tab$horizon)
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

fdates2 <- fdates[1:20]

dm <- lapply(rmethod[-(which(rmethod == "unrecon"))], function(rx){
  
  horz <- sapply(1:36, function(hx){
    
    f_unrecon <- sapply(fdates2, function(dx) t(fcasts[["unrecon"]][[dx]][["arima"]])[1,hx])
    f_recon <- sapply(fdates2, function(dx) t(aggts(fcasts[[rx]][[dx]][["arima"]]))[1,hx])
    realiz <- sapply(fdates2, function(dx) as.numeric(window(agg_gts[[1]], start = as.numeric(dx))[hx]))
    
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

tab1 <- as_tibble(do.call(cbind,dm)) %>% 
  add_column(h = 1:36) %>% 
  gather(method,pval, -h) %>% 
  mutate(Reconciliation = factor(method,
                                 levels = c("bu","mo_cat","mo_reg","tdfp_cat","tdfp_reg",
                                            "mint","wls","ols","nseries","bsr"),
                                 labels = c(" Bottom-Up "," Middle-Out (Categories) ", 
                                            " Middle-Out (Regions) ", " Top-Down (Categories)  ",
                                            " Top-Down (Regions) ", " MinT    "," WLS    ",
                                            " OLS    "," nseries"," BSR    "),
                                 ordered = T),
         Grouping = recode(method,
                           "bu" = "Basic",
                           "mo_cat" = "Basic",
                           "mo_reg" = "Basic",
                           "tdgsa_cat" = "Basic",
                           "tdgsa_reg" = "Basic",
                           "tdgsf_reg" = "Basic",
                           "tdgsf_cat" = "Basic",
                           "tdfp_reg" = "Basic",
                           "tdfp_cat" = "Basic",
                           "ols" = "Optimal",
                           "wls" = "Optimal",
                           "nseries" = "Optimal",
                           "mint" = "Optimal",
                           "bsr" = "Optimal"))


ggplot(tab1, aes(x = h, y = pval, group = Reconciliation, color = Reconciliation)) +
  geom_jitter() +
  geom_smooth(se = F) +
  facet_grid(. ~ Grouping) +
  theme_bw() +
  coord_cartesian(ylim = c(0,1), expand = F) +
  scale_x_continuous(breaks = c(12,24,36)) + 
  scale_y_continuous(breaks = seq(0,1,0.2)) + 
  scale_color_manual(values = c(bpy.colors(11)[-11])) +
  ylab("P-values of Diebold-Mariano Tests") +
  xlab("Forecast Horizon (in Months)") +
  theme(legend.position="bottom", legend.title = element_blank())  +
  guides(fill=guide_legend(nrow=2))


  
  
  