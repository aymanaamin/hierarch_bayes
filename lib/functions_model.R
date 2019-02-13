
#' Run Hierarchichal Reconciliation with Bayesian Shrinkage
#'
#' BSR description
#' @param object gts or hts object.
#' @param h integer, forecasting horizon.
#' @param fmethod character, forecasting method to use ("arima", "ets" or "rw").
#' @param series_to_be_shrunk vector of integers, indicating which reconciled
#' to shrink towards its base forecast.
#' @param nser_shr numerical, shrinks top level towards base forecasts.
#' @param xser_shr numerical, shrinks selected forecasts towards base forecasts.
#' @return A list containing parameters for each horizon and gts output
#' @export
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
    a0 <- Matrix(0,pars$m,1)
    
    # Preallocation
    chain <- matrix(NA, nrow = pars$length_max, ncol = 3)
    
    draw_save <- list("beta" = matrix(NA,pars$length_sample,pars$q),
                      "Sigma" = matrix(NA,pars$length_sample,pars$m))
    
    # Starting values
    Sigma = Diagonal(x = apply(Y, 1, var) + 1e-16)
    alpha = Matrix(0,pars$m,1)
    beta = solve(t(S) %*% solve(Sigma) %*% S) %*% (t(S) %*% solve(Sigma) %*% Y_mean)
    
    checks <- list("convergence" = F,
                   "sampling" = F,
                   "jx" = 0,
                   "ix" = 0)
    
    while(checks$jx < pars$length_max){
      
      checks$jx <- checks$jx+1
      
      W <- pars$lambda %*% Sigma %*% pars$lambda
      
      # 1. Compute Alpha
      M <- Diagonal(n = pars$m) - (S %*% solve(t(S) %*% solve(W) %*% S) %*% t(S)%*% solve(W))
      A0 <- Diagonal(n = pars$m, x = 1e-9)
      A1 <- forceSymmetric(M %*% (Sigma/pars$n) %*% t(M)) + A0
      a1 <- M %*% Y_mean
      alpha <- a1 + t(rnorm(pars$m,0,1) %*% chol(A1))
      
      # 2. Compute Beta
      B1  <- solve(pars$n*(t(S) %*% solve(W) %*% S))
      b1 <- B1 %*% (pars$n*(t(S) %*% solve(W) %*% (Y_mean - alpha)))
      beta <- b1 + t(rnorm(pars$q,0,1) %*% chol(B1))
      
      # 3. Compute Sigma
      E <- Y - kronecker(alpha, Matrix(1,1,pars$n)) - kronecker(S %*% beta, Matrix(1,1,pars$n))
      
      if(pars$sparse==T){
        Sigma <- Diagonal(x = sapply(1:pars$m, function(sx){
          1/rgamma(n = 1, shape = pars$n + 1e-16, 
                   rate = t(E[sx,]) %*% E[sx,]) + 1e-16}))
      } else {
        Sigma <- Matrix(riwish(v = pars$n + 1e-16, 
                               S = E %*% t(E) + 1e-16))
      }
      
      # X1. convergence check
      if(checks$jx == 1){
        chain[checks$jx,] <- c(mean(alpha),mean(beta),mean(diag(Sigma)))
      } else {
        chain[checks$jx,] <- (chain[checks$jx-1,]*(checks$jx-1) + 
                                c(mean(alpha),mean(beta),mean(diag(Sigma))))/checks$jx
        if(checks$jx > 200) if(all(abs(chain[checks$jx,2]/chain[checks$jx-100,2]-1) < 1e-6)) checks$convergence <- T
      }
      
      if(checks$jx %% 100 == 0) plot(mcmc(chain[c(1:checks$jx),]))
      
      # X2. start sampling upon convergence
      if(checks$convergence & !checks$sampling){
        
        checks$ix <- checks$ix + 1
        draw_save$beta[checks$ix,] = as.matrix(beta)
        draw_save$Sigma[checks$ix,] = diag(Sigma)
        
        if(checks$ix == pars$length_sample) checks$sampling <- T
        
      }
      
      if(checks$convergence & checks$sampling){
        
        print(sprintf('h = %d: convergence achieved and sampling completed after %d draws.',hx,checks$jx+1))
        
        # Compute discrete statistics of posterior distribution
        results <- list("beta" = colMeans(draw_save$beta),
                        "Sigma" = colMeans(draw_save$Sigma))
        
        break
        
      }
    }
    
    return(results)
    
  })
}



CollectOutput <- function(object, forecasts.list, results.list, pars){
  
  bfcasts <- t(do.call(cbind, lapply(results.list,function(x) x$beta)))
  bfcasts <- ts(bfcasts, 
                start = as.numeric(tail(time(object$bts),1)) + 1/frequency(object$bts),
                frequency = frequency(object$bts))
  bvar <- ts(t(do.call(cbind, lapply(results.list,function(x) x$Sigma))), 
             start = as.numeric(tail(time(object$bts),1)) + 1/frequency(object$bts),
             frequency = frequency(object$bts))
  colnames(bvar) <- colnames(aggts(object))
  colnames(bfcasts) <- colnames(object$bts)
  class(bfcasts) <- class(object$bts)
  attr(bfcasts, "msts") <- attr(object$bts, "msts")
  out <- list(bts = bfcasts, histy = object$bts, labels = object$labels, fmethod = pars$fmethod)
  if (is.hts(object)) {
    out$nodes <- object$nodes
  } else {
    out$groups <- object$groups
  }
  
  out$var <- bvar
  
  return(structure(out, class = class(object)))
  
}
