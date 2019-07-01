
rm(list = ls())

source("cls2/1_pars.R")

# create cluster for parallel processing
cl <- makeCluster(cores_s)
registerDoParallel(cl)

chunks <- split(1:length(fdate), factor(sort(rank(1:length(fdate)) %% 12)))

for(ix in 1:length(chunks)){

  bsr <- foreach(n = chunks[[ix]], .packages = c("hts","forecast","Matrix")) %dopar% {

    dx <- fdate[n]

    lapply(fmethods, function(fx){

      if(fx == "arima"){

        # Run forecasting methods
        RunBSR(object = window(tradegts_reduced2, end = dx-1/12),
               fmethod = fx,
               h = horizon,
               shrinkage = "none",
               series_to_be_shrunk = c())


      } else {

        NULL

      }
    })
  }

  names(bsr) <- fdate[chunks[[ix]]]
  save(bsr, file = paste0("out/forecasts/forecasts_bsr_",ix,".Rdata"))

}



bsr_l <- {}
for(ix in 1:12) {
  load(paste0("out/forecasts/forecasts_bsr_",ix,".Rdata"))
  bsr_l <- c(bsr_l,bsr)
}

bsr <- bsr_l
save(bsr, file = "out/forecasts/forecasts_bsr.Rdata")






