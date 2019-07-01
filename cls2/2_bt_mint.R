
rm(list = ls())

source("cls2/1_pars.R")

tradegts_reduced2$bts[which(tradegts_reduced2$bts == 0)] <- 1e+4 + rlnorm(length(which(tradegts_reduced2$bts == 0)),0,1)*1e+4

# create cluster for parallel processing
cl <- makeCluster(6)
registerDoParallel(cl)

chunks <- split(1:length(fdate), factor(sort(rank(1:length(fdate)) %% 12)))

for(ix in 1:length(chunks)){  
  
  mint <- foreach(n = chunks[[ix]], .packages = c("hts","forecast")) %dopar% {
    
    dx <- fdate[n]
    
    lapply(fmethods, function(fx){
      
      if(fx == "arima"){
        
        tryCatch({
          
          # Run forecasting methods
          forecast(window(tradegts_reduced2, end = dx-1/12),
                   h = horizon,
                   method = "comb",
                   weights = "mint",
                   fmethod = fx)
          
        }, warning = function(w) {
          
          NULL
          
        }, error = function(e) {
          
          NULL
          
        })
        
      } else {
        
        NULL
        
      }
    })
  }
  
  names(mint) <- fdate[chunks[[ix]]]
  save(mint, file = paste0("out/forecasts/forecasts_mint_",ix,".Rdata"))
  
}


mint_l <- {}
for(ix in 1:12) {
  load(paste0("out/forecasts/forecasts_mint_",ix,".Rdata"))
  mint_l <- c(mint_l,mint)
  }




dx <- names(mint_l)[which(sapply(mint_l, function(x) is.null(x$arima)))]

mint_v2 <- forecast(window(tradegts_reduced2, end = as.numeric(dx)-1/12),
                              h = 36,
                              method = "comb",
                              weights = "mint",
                              fmethod = "arima")

mint_l[[dx]][["arima"]] <- mint_v2


mint <- mint_l
save(mint, file = "out/forecasts/forecasts_mint.Rdata")

