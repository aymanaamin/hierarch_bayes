
#' Create Predictions
#'
#' This function generates samples by drawing from fitted models
#' @param object gts object
#' @param fmethod character, forecasting method to use ("arima", "ets" or "rw")
#' @param pars list of parameters
#' @return diagonal sparse matrix
#' @export
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




#' Define Weighting Matrix
#'
#' This function generates a diagonal matrix of weights used
#' @param S S matrix
#' @param pars list of parameters
#' @return diagonal sparse matrix
#' @export
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





#' Reconcile Forecasts at each Horizon
#'
#' This function approximates the joint posterior distribution using Gibbs sampling
#' @param object gts or hts object.
#' @param h integer, forecasting horizon.
#' @param fmethod character, forecasting method to use ("arima", "ets" or "rw").
#' @param series_to_be_shrunk vector of integers, indicating which reconciled
#' to shrink towards its base forecast.
#' @param nser_shr numerical, shrinks top level towards base forecasts.
#' @param xser_shr numerical, shrinks selected forecasts towards base forecasts.
#' @return A list containing parameters for each horizon and gts output
#' @export
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
                      "Sigma" = matrix(NA,pars$length_sample,pars$m),
                      "fitted" = matrix(NA,pars$length_sample,pars$m))

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
      A0 <- Diagonal(n = pars$m, x = 1e-6)
      A1 <- forceSymmetric(M %*% (Sigma/pars$n) %*% t(M)) + A0
      a1 <- M %*% Y_mean
      alpha <- mvrnorm(n = 1, mu = a1, Sigma = A1)

      # 2. Compute Beta
      B1  <- forceSymmetric(solve(pars$n*(t(S) %*% solve(Sigma) %*% S)))
      b1 <- B1 %*% (pars$n*(t(S) %*% solve(Sigma) %*% (Y_mean - alpha)))
      beta <- b1 + t(rnorm(pars$q,0,1) %*% chol(B1))

      # 3. Compute Sigma
      E <- Y - kronecker(alpha, Matrix(1,1,pars$n)) - kronecker(S %*% beta, Matrix(1,1,pars$n))

      if(pars$sparse==T){
        Sigma <- Diagonal(x = sapply(1:pars$m, function(sx){
          1/rgamma(n = 1, shape = pars$n + 1e-16,
                   rate = t(E[sx,]) %*% E[sx,]) + 1e-16}))
      } else {
        Sigma <- forceSymmetric(Matrix(riwish(v = pars$n + 1e-16,
                               S = E %*% t(E) + 1e-16)))
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
        draw_save$fitted[checks$ix,] = as.matrix(S %*% beta + t(rnorm(pars$m,0,1) %*% chol(Sigma)))

        if(checks$ix == pars$length_sample) checks$sampling <- T

      }

      if(checks$convergence & checks$sampling){

        print(sprintf('h = %d: convergence achieved and sampling completed after %d draws.',hx,checks$jx+1))

        # Compute discrete statistics of posterior distribution
        results <- list("mean" = colMeans(draw_save$beta),
                        "variance" = apply(draw_save$fitted, 2, var))

        break

      }
    }

    return(results)

  })
}




#' Collect Output
#'
#' This function generates a gts object
#' @param object gts object
#' @param forecast.list list of forecasts
#' @param results.list list of results
#' @param pars list of parameters
#' @return gts objects
#' @export
CollectOutput <- function(object, forecasts.list, results.list, pars){

  bfcasts <- t(do.call(cbind, lapply(results.list,function(x) x$mean)))
  bfcasts <- ts(bfcasts,
                start = as.numeric(tail(time(object$bts),1)) + 1/frequency(object$bts),
                frequency = frequency(object$bts))
  bvar <- ts(t(do.call(cbind, lapply(results.list,function(x) x$variance))),
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

