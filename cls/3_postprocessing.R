
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

# Packages
library(hts)
library(forecast)
library(data.table)
library(doParallel)
library(foreach)
library(tidyverse)

# Data
recon <- c("bu","mo_cat","mo_reg","tdfp_cat","tdfp_reg","ols","wls","mint","nseries","unrecon")
for(ix in recon) load(sprintf("out/results_%s.Rdata",ix))
results <- lapply(recon, function(ix) eval(parse(text=ix)))
names(results) <- recon
rm(list = recon,recon,ix)

# # Cluster
# cl <- makeCluster(20)
# registerDoParallel(cl)



# 1. CREATE TABLES --------------------------------------------------------

# Create wide format table more suitable for plotting backtest results
rmethod <- names(results)
fdates <- names(results$ols)
fmethod <- names(results$ols$`1998`)
horizons <- names(results$ols$`1998`$ets)
levels <- unname(colnames(results$ols$`1998`$ets$`1`))
levels[grepl("/",levels)] <- sapply(strsplit(levels[grepl("/",levels)],"/"),`[`,2)
grid <- CJ(rmethod,fdates,fmethod,horizons)
grid <- grid[!(V2 == 2017 & V4 %in% c(3,2)),]
grid <- grid[!(V2 == 2016 & V4 %in% c(3)),]

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

names(tabs) <- acc_measures
save(tabs, file = "out/tabs.Rdata")

# ME     Mean Error
# RMSE   Root Mean Square Error
# MAE    Mean Absolute Error
# MAPE   Mean Absolute Percentage Error
# MPE    Mean Percentage Error
# MASE   Mean Absolute Scaled Error
