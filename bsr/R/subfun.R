
#' Define Weighting Matrix
#'
#' This function generates a diagonal matrix of weights used
#' @param object gts object
#' @param fmethod forecasting method to be used
#' @param pars list of parameters
#' @return list of predictions
create_predictions <- function(object, fmethod, pars){

  lapply(as.list(aggts(object)), function(jx){


    if(fmethod == "rw"){
      fit <- Arima(jx, order = c(0, 1, 0))
    } else if(fmethod == "ets"){
      fit <- ets(jx)
    } else if(fmethod == "arima"){
      fit <- auto.arima(jx)
    }

    t(matrix(replicate(pars$n, simulate(fit, future=TRUE, nsim=pars$h)),ncol=pars$n))

  })

}




#' Define Weighting Matrix
#'
#' This function generates a diagonal matrix of weights used
#' @param pars list of parameters
#' @return vector of weights
define_weights <- function(pars){

  xser_shr = 1e+6

  # define global shrinkages
  if(is.null(pars$shrinkage)){

    lambda <- rep(1,pars$m)

  } else if(pars$shrinkage == "nseries"){

    lvec <- (1/rowSums(pars$S))
    lambda <- lvec/prod(lvec)^(1/pars$m)

  } else if(pars$shrinkage == "td"){

    lvec <- rep(1,pars$m)
    lvec[1] <- lvec[1]/xser_shr
    lambda <- lvec/prod(lvec)^(1/pars$m)

  } else if(pars$shrinkage == "mo"){

    lvec <- rep(1,pars$m)
    lvec[-c(1,seq(pars$m-pars$q+1,pars$m))] <- lvec[-c(1,seq(pars$m-pars$q+1,pars$m))]/(xser_shr*(1+pars$q))
    lambda <- lvec/prod(lvec)^(1/pars$m)

  } else if(pars$shrinkage == "bu"){

    lvec <- rep(1,pars$m)
    lvec[seq(pars$m-pars$q+1,pars$m)] <- lvec[seq(pars$m-pars$q+1,pars$m)]/(xser_shr*pars$q)
    lambda <- lvec/prod(lvec)^(1/pars$m)

  } else if(is.numeric(pars$shrinkage)){

    lambda <- rep(1,pars$m)
    lambda[pars$shrinkage] <- lambda[pars$shrinkage]/(xser_shr*(1/length(pars$shrinkage)))
    lambda <- lambda/prod(lambda)^(1/pars$m)

  } else {

    stop("Unknown shrinkage parameter, use 'nseries', 'td', 'mo', 'bu' or a
         vector of integers to indicate which reconciliation error to shrink.")

  }

  return(lambda)

}



#' Running Reconciliation
#'
#' This function estimates a Bayesian state space model to reconcile all
#' forecast horizons jointly
#' @param forecasts_list list of predictions
#' @param pars list of parameters
#' @return matrix containing betas
run_reconciliation <- function(forecasts_list, pars){

  # Get matrix with forecasts and prior mean
  Y <- matrix(t(do.call(cbind,lapply(forecasts_list, function(x) apply(x,2,mean)))))
  sigmas <- t(do.call(cbind,lapply(forecasts_list, function(x) apply(x,2,var))))
  Smat <- kronecker(Diagonal(pars$h), pars$S)

  # preallocation & starting values
  alpha_save <- vector(mode = "list", length = pars$length_sample)
  beta_save <- vector(mode = "list", length = pars$length_sample)
  alpha = Matrix(0,(pars$h+1)*pars$m,1)
  beta = solve(crossprod(Smat), crossprod(Smat,Y))
  omega = Diagonal(x = 1e+9,n = pars$m)
  sigmainv = Diagonal(x = 1,n = pars$m*pars$h)

  # initialize progress bar
  pb <- txtProgressBar(style = 3)

  # loop
  for(jx in 1:(pars$burn_in + pars$length_sample)){

    setTxtProgressBar(pb, jx/(pars$burn_in + pars$length_sample))

    # 1.1 alpha
    M <- Diagonal(n = pars$m*pars$h) - (Smat %*% solve(t(Smat) %*% sigmainv %*% Smat) %*% t(Smat) %*% sigmainv)
    H1 <- Diagonal(pars$m*(pars$h+1),1)
    H2 <- cbind(rbind(Matrix(0,pars$m,pars$m*(pars$h)),
                      Diagonal(pars$m*(pars$h),-1)),
                Matrix(0,pars$m*(pars$h+1),pars$m))
    H <- H1+H2
    SS <- kronecker(Diagonal(pars$h+1), omega)
    SS[1:pars$m,1:pars$m] <- Diagonal(pars$m, 1e-16)
    K <- t(H) %*% solve(SS) %*% H
    G <- cbind(Matrix(0,pars$m*pars$h,pars$m), Diagonal(pars$m*pars$h))
    P <-  K + t(G) %*% G
    C <- chol(forceSymmetric(P))
    alpha <- solve(C, solve(t(C), t(G) %*% M %*% Y, sparse = T), sparse = T) + solve(C, rnorm(nrow(C)))

    # 1.2 omega
    alpha_out <- alpha
    dim(alpha_out) <- c(pars$m,pars$h+1)
    alpha_out <- t(alpha_out)

    omega <- Diagonal(x = sapply(1:pars$m, function(mx){

      err = diff(alpha_out[,mx])
      1/rgamma(n = 1, shape = (pars$h + 3)/2, rate = (t(err) %*% err + 1e-2)/2)

    }))

    # 1.3 beta
    B1  <- t(Smat) %*% sigmainv %*% Smat
    C2 <- chol(B1)
    b1 <- solve(C2, solve(t(C2), t(Smat) %*% sigmainv %*% (Y - G %*% alpha), sparse = T),
                sparse = T)
    beta <- b1 + solve(C2, rnorm(nrow(C2)), sparse = T)


    # 1.4 Sigma
    sigmainv <- Diagonal(x = 1/matrix(sapply(1:pars$h, function(hx){

      sapply(1:pars$m, function(mx){

        a0 <- pars$n * 1e+5
        d0 <- sigmas[mx,hx] * pars$weights[mx] * pars$n * 1e+5 + 1e-9

        1/rgamma(n = 1,
               shape = (pars$n + a0)/2,
               rate = (sigmas[mx,hx] * pars$n + d0)/2) + 1e-16


      })
    })))

    # 2 store output
    if(jx > pars$burn_in){

      beta_out <- b1
      dim(beta_out) <- c(pars$q,pars$h)
      beta_out <- t(beta_out)

      alpha_save[[jx-pars$burn_in]] <- alpha_out
      beta_save[[jx-pars$burn_in]] <- beta_out

    }
  }

  close(pb)

  beta_out <- Reduce("+", beta_save)/pars$length_sample
  # alpha_out <- Reduce("+", alpha_save)/pars$length_sample
  # cbind(apply(forecasts_list[[1]],2,mean), (alpha_out[-1,] + beta_out %*% t(pars$S))[,1])

  return(beta_out)

}

