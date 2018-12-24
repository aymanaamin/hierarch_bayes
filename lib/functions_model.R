
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
                   nser_shr = 0){
  
  start.time = Sys.time() # Tic
  
  if(!is.null(series_to_be_shrunk)) if(max(series_to_be_shrunk) > length(unlist(object$labels))){
    stop("Series to be shrunk doesn't exist.", 
         call. = FALSE)
  }
  
  
  # Step 1: Define Summation Matrix & Parameters
  S <- Matrix(smatrix(object))
  pars <- list("sparse" = T,
               "length_sample" = 1000,
               "length_max" = 1e+5,
               "h" = h,
               "n" = 1000,
               "m" = nrow(S),
               "q" = ncol(S),
               "nser_shr" = nser_shr,
               "xser_shr" = nrow(S)*1000, # n*m
               "series_to_be_shrunk" = series_to_be_shrunk) 
  
  # Step 2: Run Forecasting Model
  forecasts.list <- CreatePredictions(object,fmethod,pars)
  
  # Step 3: Set Priors
  priors.list <- DefinePriors(S, forecasts.list, pars)
  
  # Step 4: Run Reconciliation
  results.list <- RunReconciliation(S, forecasts.list,priors.list,pars)
  
  # Step 5: Collect Output and Parameters
  out.list <- CollectOutput(object, forecasts.list, priors.list, results.list, pars)
  
  print(Sys.time() - start.time) # Toc
  
  return(out.list)
  
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



DefinePriors <- function(S, forecasts.list, pars){
  
  print("Defining Priors..")
  
  # Define Prior Variance
  nser <- (1/rowSums(S))^pars$nser_shr
  mat <- nser/prod(nser)^(1/pars$m)
  
  x <- pars$series_to_be_shrunk
  
  if(!is.null(x)){
    
    mat[x] <- mat[x]/pars$xser_shr^(1/length(x))
    mat[-x] <-  mat[-x] * pars$xser_shr^(1/(length(mat)-length(x)))
    
  }
  
  lambda <- mat
  
  out <- lapply(1:pars$h, function(hx){
    
    Omega <- apply(do.call(rbind, lapply(forecasts.list, function(fx) fx[,hx])), 1, var) + 1e-16
    
    Diagonal(x = (lambda * (pars$m*Omega/pars$n) * lambda))
    
  })
  
  return(list("A0" = out,
              "lambda" = lambda))
}



RunReconciliation <- function(S, forecasts.list, priors.list, pars){
  
  print("Running Reconciliation..")
  
  lapply(1:pars$h, function(hx){
    
    # Get matrix with forecasts and prior mean
    Y <- Matrix(do.call(rbind, lapply(forecasts.list, function(fx) fx[,hx])))
    Y_mean <- rowMeans(Y)
    a0 <- Matrix(0,pars$m,1)
    A0 <- priors.list$A0[[hx]]
    b0 = Matrix(0,pars$q,1)
    B0 = Diagonal(n = pars$q, x = 1e+16)
    
    # Preallocation
    chain <- matrix(NA, nrow = pars$length_max, ncol = 3)
    
    draw_save <- list("alpha" = matrix(NA,pars$length_sample,pars$m),
                      "beta" = matrix(NA,pars$length_sample,pars$q),
                      "Sigma" = array(NA,c(pars$m,pars$m,pars$length_sample)),
                      "A1" = array(NA,c(pars$m,pars$m,pars$length_sample)),
                      "A0" = array(NA,c(pars$m,pars$m,pars$length_sample)))
    
    # Starting values
    Sigma = Diagonal(x = apply(Y, 1, var) + 1e-16)
    alpha = Matrix(0,pars$m,1)
    beta = solve(t(S) %*% solve(Sigma) %*% S) %*% (t(S) %*% solve(Sigma) %*% rowMeans(Y))
    
    checks <- list("convergence" = F,
                   "sampling" = F,
                   "jx" = 0,
                   "ix" = 0)
    
    while(checks$jx < pars$length_max){
      
      checks$jx <- checks$jx+1
      
      # 1. Compute Alpha
      A1 <- solve(pars$n*solve(Sigma) + solve(A0))
      a1 <- A1 %*% (rowSums(solve(Sigma) %*% (Y - kronecker(S %*% beta, Matrix(1,1,pars$n)))) + solve(A0) %*% a0)
      alpha <- a1 + t(rnorm(pars$m,0,1) %*% chol(A1))
      
      # 2. Compute Beta
      B1  <- solve(pars$n*(t(S) %*% solve(Sigma) %*% S))
      b1 <- B1 %*% (rowSums(t(S) %*% solve(Sigma) %*% (Y - kronecker(alpha, Matrix(1,1,pars$n)))))
      beta <- b1 + t(rnorm(pars$q,0,1) %*% chol(B1))
      
      
      # 3. Compute Sigma
      E <- Y - kronecker(alpha, Matrix(1,1,pars$n)) - kronecker(S %*% beta, Matrix(1,1,pars$n))
      
      if(pars$sparse==T){
        Sigma <- Diagonal(x = sapply(1:pars$m, function(sx){
          1/rgamma(n = 1, shape = pars$n+ 1e-16, 
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
        if(checks$jx > 100) if(all(abs(chain[checks$jx,]/chain[checks$jx-100,]-1) < 1e-4)) checks$convergence <- T
      }
      
      
      # recursive mean trace plot
      if(checks$jx %% 250 == 0) plot(mcmc(chain[c(1:checks$jx),]))
      
      # X2. start sampling upon convergence
      if(checks$convergence & !checks$sampling){
        
        checks$ix <- checks$ix + 1
        
        draw_save$alpha[checks$ix,] = as.matrix(alpha)
        draw_save$beta[checks$ix,] = as.matrix(beta)
        draw_save$A1[,,checks$ix] = as.matrix(A1)
        draw_save$A0[,,checks$ix] = as.matrix(A0)
        draw_save$Sigma[,,checks$ix] = as.matrix(Sigma)
        
        if(checks$ix == pars$length_sample) checks$sampling <- T
        
      }
      
      if(checks$convergence & checks$sampling){
        
        print(sprintf('h = %d: convergence achieved and sampling completed after %d draws.',hx,checks$jx+1))
        
        # Compute discrete statistics of posterior distribution
        results <- list(
          "beta" = colMeans(draw_save$beta),
          "beta_var" = var(draw_save$beta),
          "alpha" = colMeans(draw_save$alpha),
          "alpha_var" = var(draw_save$alpha),
          "Sigma_mean" = apply(draw_save$Sigma, 1:2, mean),
          "A1" = apply(draw_save$A1, 1:2, mean),
          "A0" = apply(draw_save$A0, 1:2, mean),
          "a0" = a0)
        
        break
        
      }
    }
    
    return(results)
    
  })
}



CollectOutput <- function(object, forecasts.list, priors.list, results.list, pars){
  
  # clean up results and return them in an appropriate manner
  out <- results.list
  names(out) <- paste("h = ",1:h)
  bottom_forecasts <- t(do.call(cbind, lapply(out,function(x) x$beta)))
  colnames(bottom_forecasts) <- colnames(object$bts)
  if("hts" %in% class(object)){
    
    out$forecast <- hts(y = ts(bottom_forecasts, 
                               start = test_date+1,
                               frequency = frequency(object$bts)),
                        nodes = object$nodes)
    
  } else {
    
    out$forecast <- gts(y = ts(bottom_forecasts, 
                               start = test_date+1,
                               frequency = frequency(object$bts)), 
                        groups = object$groups)
    
  }
  
  out$forecast$histy <- object$bts
  out$pars <- pars 
  out$base.forecasts <- forecasts.list
  out$priors <- priors.list
  
  return(out)
  
}




# get_prior_mean <- function(x, h, fmethod){
#   
#   # Choose the starting date such that the seasonality pattern of the forecast is the same
#   end_date <- as.numeric(tail(time(x$bts),1))
#   wndw <- ceiling(h/frequency(x$bts))*5
#   
#   Reduce('+',lapply(seq(from = end_date-wndw,
#                         to = end_date),
#                     function(dx){
#     
#     test_wndw <- window(x, end = dx)
#     
#     fcast_unrecon <- do.call(cbind, lapply(as.list(aggts(test_wndw)), 
#                                            function(xs){
#                                              if(fmethod == "rw"){
#                                                rwf(xs, h = h)$mean
#                                              } else if(fmethod == "ets"){
#                                                forecast(ets(xs), h = h, PI = FALSE)$mean
#                                              } else if(fmethod == "arima"){
#                                                forecast(auto.arima(xs),h = h)$mean
#                                              }
#                                            }))
#     
#     fcast_recon <- aggts(forecast(test_wndw, h = h, method = "comb", 
#                                   weights = "wls", fmethod = fmethod))
#     
#     
#     return(t(fcast_unrecon) - t(fcast_recon))
# 
#   }))/wndw
#   
# }
# 
