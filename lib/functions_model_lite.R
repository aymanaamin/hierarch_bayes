
RunBSR <- function(object, h, fmethod = "arima", 
                   series_to_be_shrunk = NULL,
                   shrinkage = "none", sparse = TRUE){
  
  start.time = Sys.time() # Tic
  
  # Step 1: Define Summation Matrix & Parameters
  S <- hts:::SmatrixM(object$groups)
  pars <- list("sparse" = T,
               "length_sample" = 1000,
               "length_max" = 1e+5,
               "fmethod" = fmethod,
               "h" = h,
               "n" = 1000,
               "m" = nrow(S),
               "q" = ncol(S),
               "shrinkage" = shrinkage,
               "xser_shr" = 1e+5,
               "series_to_be_shrunk" = series_to_be_shrunk)
  
  if(!is.null(series_to_be_shrunk)) if(max(series_to_be_shrunk) > pars$m){
    stop("Series to be shrunk doesn't exist.", 
         call. = FALSE)
  }
  
  
  # Step 2: Run Forecasting Model
  forecasts.list <- CreatePredictions(object, fmethod, pars)
  
  # Step 3: Create Weighting Matrix
  pars$lambda <- DefineWeights(S,pars)
  
  # Step 4: Run Reconciliation
  results.list <- RunReconciliation(S, forecasts.list, pars)
  
  # Step 5: Collect Output and Parameters
  out <- CollectOutput(object, forecasts.list, results.list, pars)
  
  print(Sys.time() - start.time) # Toc
  
  return(out)
  
}



CreatePredictions <- function(object, fmethod, pars){
  
  print("Running Base Forecast Models..")
  
  lapply(as.list(aggts(object)), function(x){
    
    if(fmethod == "rw"){
      fit <- Arima(x, order = c(0, 1, 0))
    } else if(fmethod == "ets"){
      fit <- ets(x)
    } else if(fmethod == "arima"){
      fit <- auto.arima(x)
    }
    
    t(matrix(replicate(pars$n, simulate(fit, future=TRUE, nsim=pars$h)),ncol=pars$n))
    
  })
}



DefineWeights <- function(S, pars){
  
  # define global shrinkages
  if(pars$shrinkage == "nseries"){
    
    lvec <- (1/rowSums(S))
    lambda <- lvec/prod(lvec)^(1/pars$m)
    
  } else if(pars$shrinkage == "td"){
    
    lvec <- rep(1,pars$m)
    lvec[1] <- lvec[1]/pars$xser_shr
    lambda <- lvec/prod(lvec)^(1/pars$m)
    
  } else if(pars$shrinkage == "mo"){
    
    lvec <- rep(1,pars$m)
    lvec[-c(1,seq(pars$m-pars$q+1,pars$m))] <- lvec[-c(1,seq(pars$m-pars$q+1,pars$m))]/(pars$xser_shr*(1+pars$q))
    lambda <- lvec/prod(lvec)^(1/pars$m)
    
  } else if(pars$shrinkage == "bu"){
    
    lvec <- rep(1,pars$m)
    lvec[seq(pars$m-pars$q+1,pars$m)] <- lvec[seq(pars$m-pars$q+1,pars$m)]/(pars$xser_shr*pars$q)
    lambda <- lvec/prod(lvec)^(1/pars$m)
    
  } else {
    
    lambda <- rep(1,pars$m)
    
  }
  
  # Define local shrinkage
  x <- pars$series_to_be_shrunk
  
  if(!is.null(x)){
    lambda[x] <- lambda[x]/(pars$xser_shr*(1/length(x)))
    lambda <- lambda/prod(lambda)^(1/pars$m)
  }
  
  return(Diagonal(x = lambda))
  
}


RunReconciliation <- function(S, forecasts.list, pars){
  
  print("Running Reconciliation..")
  
  # Loop over each forecasting horizon and run reconciliation
  lapply(1:pars$h, function(hx){
    
    # Get matrix with forecasts and prior mean
    Y <- Matrix(do.call(rbind, lapply(forecasts.list, function(fx) fx[,hx])))
    Y_mean <- rowMeans(Y)
    Sigma = Diagonal(x = apply(Y, 1, var) + 1e-16)
    W <- pars$lambda %*% Sigma %*% pars$lambda
    M <- Diagonal(n = pars$m) - (S %*% solve(t(S) %*% solve(W) %*% S) %*% t(S)%*% solve(W))
    alpha <- M %*% Y_mean
    
    # 2. Compute Beta
    B1  <- solve(pars$n*(t(S) %*% solve(Sigma) %*% S))
    beta <- B1 %*% (pars$n*(t(S) %*% solve(Sigma) %*% (Y_mean - alpha)))

    # Compute discrete statistics of posterior distribution
    results <- list("beta" = beta)
      
  })
}



CollectOutput <- function(object, forecasts.list, results.list, pars){
  
  bfcasts <- as.matrix(t(do.call(cbind, lapply(results.list,function(x) x$beta))))
  bfcasts <- ts(bfcasts, 
                start = as.numeric(tail(time(object$bts),1)) + 1/frequency(object$bts),
                frequency = frequency(object$bts))
  colnames(bfcasts) <- colnames(object$bts)
  class(bfcasts) <- class(object$bts)
  attr(bfcasts, "msts") <- attr(object$bts, "msts")
  out <- list(bts = bfcasts, histy = object$bts, labels = object$labels, fmethod = pars$fmethod)
  if (is.hts(object)) {
    out$nodes <- object$nodes
  } else {
    out$groups <- object$groups
  }

  return(structure(out, class = class(object)))
  
}
