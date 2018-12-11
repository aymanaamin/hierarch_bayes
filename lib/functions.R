
import_data <- function(){
  
  # countries
  load("dat/countries.rda")
  countries <- countries[validTo == "12.2999", .(numCode, isoCode, regCode)]
  
  # define data files to import
  ls_files <- data.table(
    exports = list.files(file.path("dat/exports"), "*.txt", full.names = TRUE, recursive = TRUE),
    imports = list.files(file.path("dat/imports"), "*.txt", full.names = TRUE, recursive = TRUE)
  )
  
  ls_files <- data.table::melt(ls_files, measure.vars = c("exports", "imports"), variable.name = "dat", value.name = "fil")
  ls_files[, id := seq(.N)]
  
  # import raw data
  data_raw <- ls_files[,
                       .SD[, {
                         message(sprintf("Reading file %s...", fil));
                         laf_open_fwf(fil,
                                      column_types = c("character","character","integer",
                                                       "character","character","numeric",
                                                       "numeric","numeric"),
                                      column_widths = c(1,12,3,7,4,13,13,13),
                                      column_names = c("direction","group",
                                                       "numCode","period","volumecode",
                                                       "volume","weight","value")
                         )[,]
                       }
                       , by = fil]
                       , by = list(id, dat)]
  
  data_raw <- data_raw[!is.na(value)]
  data_raw <- data_raw[!(group %in% c("13.1","13.2","14.1","14.2")),]
  data_raw <- data_raw[, numCode:=as.integer(numCode)]
  
  message("Processing raw data...")
  
  # aggregate value of goods in each entry
  data_agg <- data_raw[, .(value = sum(value)),
                       by = list(dat, direction, numCode, group, period)]
  
  # merge iso country codes and data
  total <- merge(data_agg, countries, by = "numCode")
  
  # generate keys
  total[, group_clean := gsub("[.]","",group)]
  total[, group_nchar := nchar(group_clean)] 
  total[, group_8dig := sapply(1:nrow(total), function(jx) {
    paste0(total$group_clean[jx],
           paste(rep(0, 8 - total$group_nchar[jx]),collapse = ""))})]
  total[, tsKey := paste0(direction, regCode, isoCode, group_8dig)]
  
  return(total[, .(period,value,tsKey)])
  
}


dt2ts <- function(dtx){
  
  # retrieve dates
  toNumericDate <- function(x) {
    num <- as.numeric(x)
    mnth <- floor(num)
    10000*(num - mnth) + (mnth - 1)/12
  }
  
  dtx[, dateNumeric := toNumericDate(period)]
  
  # transform to time series
  value_dt <- dcast(dtx[, .(tsKey, dateNumeric, value)], "dateNumeric ~ tsKey", value.var = "value", fun.aggregate = sum)
  class(value_dt) <- "data.frame"
  tsl <- as.list(as.ts(zoo::zoo(x = value_dt[,-1], order.by = value_dt[, 1])))
  tsl <- lapply(tsl, function(x) {
    x[is.na(x)] <- 0
    x
  })
  
  return(do.call(cbind,tsl))
  
}




run_recon <- function(object, h, fmethod = "arima", 
                      series_to_be_shrunk = NULL,
                      nser_shr = 0, xser_shr = 1e+6,
                      in_sample_prior = F){

  start.time = Sys.time() # Tic
  
  # Get Parameters
  in_sample_prior = T
  sparse=T
  length_sample = 1000
  length_max = 1e+5
  S <- Matrix(smatrix(object))
  m <- nrow(S) # total series
  q <- ncol(S) # bottom level series
  
  
  
  # Step 1: Run Forecasting Model
  print("Running base forecast models...")
  fcl <- lapply(as.list(aggts(object)), function(x){
    
    if(fmethod == "rw"){
      fit <- Arima(x, order = c(0, 1, 0))
    } else if(fmethod == "ets"){
      fit <- ets(x)
    } else if(fmethod == "arima"){
      fit <- auto.arima(x)
    }
    
    t(matrix(replicate(n, simulate(fit, future=TRUE, nsim=h)),ncol=n))
    
  })
  
  
  
  
  # Step 2. Set Priors
  if(isTRUE(in_sample_prior)){
    
    print("Define prior from in-sample data...")
    a0_all <- get_prior_mean(x = object, h, fmethod = fmethod)
    a0_all[series_to_be_shrunk,] <- 0
    
  } else {
    
    print("Define mean zero prior...")
    a0_all <- matrix(0,m,h)
    
  }
  
  lambda <- define_lambda(x = series_to_be_shrunk, S, m,
                          nser_shr = nser_shr, xser_shr = xser_shr)
  
  
  # Step 3. Run Reconciliation (loop over forecasting periods)
  print("Running reconciliation...")
  out <- lapply(1:h, function(hx){
    
    # Get matrix with forecasts and prior mean
    Y <- Matrix(do.call(rbind, lapply(fcl, function(fx) fx[,hx])))
    # a0 <- Matrix(0,m,1)
    a0 <- Matrix(a0_all[,hx])
    b0 = Matrix(0,q,1)
    B0 = Diagonal(n = q, x = 1e+16)
    
    # Preallocation
    chain <- matrix(NA, nrow = length_max, ncol = 3)
    
    draw_save <- list("alpha" = matrix(NA,length_sample,m),
                      "beta" = matrix(NA,length_sample,q),
                      "Sigma" = array(NA,c(m,m,length_sample)),
                      "A1" = array(NA,c(m,m,length_sample)),
                      "A0" = array(NA,c(m,m,length_sample)))
    
    # Starting values
    Sigma = Diagonal(x = apply(Y, 1, var))
    alpha = Matrix(0,m,1)
    beta = solve(t(S) %*% S) %*% (t(S) %*% rowMeans(Y))
    
    checks <- list("convergence" = F,
                   "sampling" = F,
                   "jx" = 0,
                   "ix" = 0)
    
    while(checks$jx < length_max){
      
      checks$jx <- checks$jx+1
      
      if(sparse==T){
        
        A0 <- lambda %*%Diagonal(x = sapply(1:m, function(sx){
          
          1/rgamma(n = 1, shape = n, rate = Sigma[sx,sx])
          
        })) %*% lambda
        
      } else {
        
        A0 <- riwish(v = n, S = lambda %*%  Sigma %*% lambda)
        
      }
      
      
      # 1. Compute Alpha
      A1 <- solve(n*solve(Sigma) + solve(A0))
      a1 <- A1 %*% (rowSums(diag(m) %*% solve(Sigma) %*% (Y - matrix(rep(S %*% beta,n),ncol = n,nrow = m))) + solve(A0) %*% a0)
      alpha <- a1 + t(rnorm(m,0,1) %*% chol(A1))
      
      
      # 2. Compute Beta
      B1  <- solve(n*(t(S) %*% solve(Sigma) %*% S) + solve(B0))
      b1 <- B1 %*% (rowSums(t(S) %*% solve(Sigma) %*% (Y - matrix(rep(alpha,n),ncol = n,nrow = m))) + solve(B0) %*% b0)
      beta <- b1 + t(rnorm(q,0,1) %*% chol(B1))
      
      
      # 3. Compute Sigma
      E <- Y - matrix(rep(alpha,n),ncol = n,nrow = m) - matrix(1,1,n) %x% (S %*% beta)
      
      if(sparse==T){
        
        Sigma <- Diagonal(x = sapply(1:m, function(sx){
          
          # Draw from Inverse Gamma
          1/rgamma(n = 1, shape = n, rate = t(E[sx,]) %*% E[sx,])
          
        }))
        
      } else {
       
        Sigma <- Matrix(riwish(v = n, S = E %*% t(E)))
         
      }
      
      
      # X1. convergence check
      if(checks$jx == 1){
        chain[checks$jx,] <- c(mean(alpha),mean(beta),mean(diag(Sigma)))
      } else {
        chain[checks$jx,] <- (chain[checks$jx-1,]*(checks$jx-1) + 
                                c(mean(alpha),mean(beta),mean(diag(Sigma))))/checks$jx
        if(all(abs(chain[checks$jx,]/chain[checks$jx-1,]-1) < 1e-6)) checks$convergence <- T
  
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
        
        if(checks$ix == length_sample) checks$sampling <- T
        
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
  
  print(Sys.time() - start.time) # Toc
  
  # clean up results and return them in an appropriate manner
  names(out) <- paste("h = ",1:h)
  bottom_forecasts <- t(do.call(cbind, lapply(out,function(x) x$beta)))
  colnames(bottom_forecasts) <- colnames(object$bts)
  if("hts" %in% class(object)){
    
    out$forecast <- hts(ts(bottom_forecasts, 
                           start = test_date+1,
                           frequency = frequency(object$bts)))
    
  } else {
    
    out$forecast <- gts(ts(bottom_forecasts, 
                           start = test_date+1,
                           frequency = frequency(object$bts)), 
                        groups = object$groups)
    
  }

  out$forecast$histy <- object$bts
  out$pars <- list("m" = m,"q" = q, "h" = h,"n" = n, "S" = S, "lambda" = lambda) 
  out$base <- fcl
  
  return(out)
  
}


define_lambda <- function(x = NULL, S, m, nser_shr = 0, xser_shr = 1){
  
  nser <- (1/rowSums(S))^nser_shr
  mat <- nser/prod(nser)^(1/m)
  
  if(!is.null(x)){
    
    mat[x] <- mat[x]/xser_shr^(1/length(x))
    mat[-x] <-  mat[-x] * xser_shr^(1/(length(mat)-length(x)))
    
  }
  
  return(Diagonal(x = mat))
  
}





get_prior_mean <- function(x, h, fmethod){
  
  # Choose the starting date such that the seasonality pattern of the forecast is the same
  end_date <- as.numeric(tail(time(x$bts),1))
  wndw <- ceiling(h/frequency(x$bts))*5
  
  Reduce('+',lapply(seq(from = end_date-wndw,
                        to = end_date),
                    function(dx){
    
    test_wndw <- window(x, end = dx)
    
    fcast_unrecon <- do.call(cbind, lapply(as.list(aggts(test_wndw)), 
                                           function(xs){
                                             if(fmethod == "rw"){
                                               rwf(xs, h = h)$mean
                                             } else if(fmethod == "ets"){
                                               forecast(ets(xs), h = h, PI = FALSE)$mean
                                             } else if(fmethod == "arima"){
                                               forecast(auto.arima(xs),h = h)$mean
                                             }
                                           }))
    
    fcast_recon <- aggts(forecast(test_wndw, h = h, method = "comb", 
                                  weights = "wls", fmethod = fmethod))
    
    
    return(t(fcast_unrecon) - t(fcast_recon))

  }))/wndw
  
}


# Some example forecasts to play around with
sim_hierarch <- function(nodes){
  
  # Generate hierarchy
  q <- sum(tail(nodes,1)[[1]])
  beta_sim <- runif(q,80,120)
  dat_hts <- hts(ts(t(beta_sim)), nodes = nodes)
  
  # Add recon error
  S <- smatrix(dat_hts)
  alpha_sim = runif(nrow(S),-10,10)
  dat_agg <- aggts(dat_hts)
  
  fcasts <- cbind(t(dat_agg + alpha_sim), S %*% runif(q,0,10))
  colnames(fcasts) <- c("mean","var")
  
  return(list("fcasts" = fcasts,
              "beta" = beta_sim,
              "alpha" = alpha_sim,
              "S" = S))
  
}



get_values <- function(recon,fmethod,fdate,horizon, measure = "MASE"){
  t(results[[recon]][[fmethod]][[fdate]][[horizon]])[,measure]}



get_table <- function(recon,fmethod,fdate,horizon,measure = "MASE",levels = c("Total")){
  grid <- expand.grid(list("recon_key" = recon,
                           "fcast_key" = fmethod,
                           "Date" = as.character(fdate),
                           "Horizon" = as.character(horizon)),
                      stringsAsFactors = F)
  out <- as.matrix(t(sapply(1:nrow(grid), function(ix) t(results[[grid[ix,1]]][[grid[ix,2]]]
                                                         [[grid[ix,3]]][[grid[ix,4]]])[levels,measure])))
  if(length(levels) == 1) out <- t(out)
  colnames(out) <- levels
  out <- as_tibble(cbind(grid,out))
  out$Date <- as.character(as.numeric(out$Date)+as.numeric(out$Horizon)-1)
  out %>% 
    add_column(.before = 1,
               "Category" = recode(out$recon_key,
                                   "bu" = "Basic Methods",
                                   "mo_cat" = "Basic Methods",
                                   "mo_reg" = "Basic Methods",
                                   "tdgsa_cat" = "Basic Methods",
                                   "tdgsa_reg" = "Basic Methods",
                                   "tdgsf_reg" = "Basic Methods",
                                   "tdgsf_cat" = "Basic Methods",
                                   "tdfp_reg" = "Basic Methods",
                                   "tdfp_cat" = "Basic Methods",
                                   "ols" = "Optimal Methods",
                                   "wls_cat" = "Optimal Methods",
                                   "wls_reg" = "Optimal Methods",
                                   "nseries" = "Optimal Methods",
                                   "unrecon" = "Basic Methods")) %>% 
    add_column(.before = 1,
               "Reconciliation" = recode(out$recon_key,
                                         "bu" = "Bottom-Up",
                                         "mo_cat" = "Middle-Out (Categories)",
                                         "mo_reg" = "Middle-Out (Regions)",
                                         "tdgsa_cat" = "Top-Down A (Categories)",
                                         "tdgsa_reg" = "Top-Down A (Regions)",
                                         "tdgsf_cat" = "Top-Down F (Categories)",
                                         "tdgsf_reg" = "Top-Down F (Regions)",
                                         "tdfp_reg" = "Top-Down P (Regions)",
                                         "tdfp_cat" = "Top-Down P (Categories)",
                                         "ols" = "OLS",
                                         "wls_cat" = "WLS (Categories)",
                                         "wls_reg" = "WLS (Regions)",
                                         "nseries" = "nSeries",
                                         "unrecon" = "Unreconciled")) %>% 
    add_column(.before = 2,
               "Forecast" = recode(out$fcast_key,
                                   "arima" = "ARIMA",
                                   "ets" = "ETS",
                                   "rw" = "RW")) %>% 
    select(-one_of("recon_key","fcast_key"))
  
}


# 
# run_gibbs <- function(Y,length_max,length_min,length_sample){
# 
#   # Preallocation
#   chain = matrix(NA, nrow = length_max, ncol = q+2*m)
#   alpha_save = matrix(NA,length_max,m)
#   beta_save = matrix(NA,length_max,q)
#   Sigma_save = array(NA,c(m,m,length_max))
#   A1_save = array(NA,c(m,m,length_max))
#   A0_save = array(NA,c(m,m,length_max))
#   X_save = matrix(NA,length_max,m)
# 
#   # Starting values
#   Sigma = diag(100*n,m)
#   alpha = matrix(0,m)
#   beta = solve(t(S) %*% S) %*% t(S) %*% rowMeans(Y)
# 
#   for(jx in 1:length_max){
# 
#     if(jx %% 1000 == 0){print(sprintf('Gibbs Sampler at %d out of a maximum of %d Draws.',jx,length_max))}
# 
#     A0 <- riwish(v = n, S = lambda %*%  Sigma %*% lambda)
# 
#     # 1. Compute Alpha
#     A1 <- solve(n*solve(Sigma) + solve(A0))
#     a1 <- A1 %*% (rowSums(diag(m) %*% solve(Sigma) %*% (Y - matrix(rep(S %*% beta,n),ncol = n,nrow = m))) + solve(A0) %*% a0)
#     alpha <- a1 + t(rnorm(m,0,1) %*% chol(A1))
# 
# 
#     # 2. Compute Beta
#     B1  <- solve(n*(t(S) %*% solve(Sigma) %*% S) + solve(B0))
#     b1 <- B1 %*% (rowSums(t(S) %*% solve(Sigma) %*% (Y - matrix(rep(alpha,n),ncol = n,nrow = m))) + solve(B0) %*% b0)
#     beta <- b1 + t(rnorm(q,0,1) %*% chol(B1))
# 
# 
#     # 3. Compute Sigma
#     E <- (Y - matrix(rep(alpha,n),ncol = n,nrow = m)) - matrix(1,1,n) %x% (S %*% beta)
#     Sigma <- riwish(v = n, S = E %*% t(E))
# 
# 
#     # X.1 Save draws
#     alpha_save[jx,] = alpha
#     beta_save[jx,] = beta
#     Sigma_save[,,jx] = Sigma
#     A1_save[,,jx] = A1
#     A0_save[,,jx] = A0
#     X_save[jx,] = S %*% beta + t(rnorm(m,0,1) %*% chol(Sigma))
# 
#     # X.2 Convergence check
#     if(jx == 1){
#       chain[jx,] = c(alpha,beta,diag(Sigma))
#     } else {
#       chain[jx,] = (chain[jx-1,]*(jx-1) + c(alpha,beta,diag(Sigma)))/jx
#     }
# 
#     # Trace Plot
#     if(jx %% 1000 == 0) plot(mcmc(chain[c(1:jx),c(1,m+1,m+q+1)]))
# 
#     # Check for Convergence
#     if(jx == length_min){
# 
#       # Compute discrete statistics of posterior distribution
#       results <- list(
#         "beta" = colMeans(beta_save[c((jx-length_sample):(jx-1)),]),
#         "beta_var" = var(beta_save[c((jx-length_sample):(jx-1)),]),
#         "alpha" = colMeans(alpha_save[c((jx-length_sample):(jx-1)),]),
#         "alpha_var" = var(alpha_save[c((jx-length_sample):(jx-1)),]),
#         "Sigma_mean" = apply(Sigma_save[,,c((jx-length_sample):(jx-1))], 1:2, mean),
#         "A1" = apply(A1_save[,,c((jx-length_sample):(jx-1))], 1:2, mean),
#         "A0" = apply(A0_save[,,c((jx-length_sample):(jx-1))], 1:2, mean),
#         "X" = colMeans(X_save[c((jx-length_sample):(jx-1)),]),
#         "X_var" = var(X_save[c((jx-length_sample):(jx-1)),]))
# 
#       return(results)
# 
#     }
#   }
# }


# # This function returns a sparse matrix supported by Matrix pkg
# smatrix2 <- function(xgts) { 
#   # Sparse matrices stored in coordinate format
#   # gmatrix contains all the information to generate smatrix
#   gmat <- xgts$groups
#   num.bts <- ncol(gmat)
#   sparse.S <- apply(gmat, 1L, function(x) {
#     ia <- as.integer(x)
#     ra <- as.integer(rep(1L, num.bts))
#     ja <- as.integer(1L:num.bts)
#     s <- sparseMatrix(i = ia, j = ja, x = ra)
#   })
#   sparse <- do.call("rbind", sparse.S)
#   return(sparse)
# }



# create_predictions <- function(x,h){
#   
#   # Run the bsts model
#   ss = AddLocalLinearTrend(list(), x)
#   ss = AddSeasonal(ss, x, nseasons = 4)
#   bsts.model = bsts(x, state.specification = ss, niter = 500, ping = 0)
#   
#   # Get a suggested number of burn-ins
#   burn = SuggestBurn(0.1, bsts.model)
#   
#   # Predict
#   p = predict.bsts(bsts.model, horizon = h, burn = burn)
#   
#   out = cbind(p$mean,apply(p$distribution, 2, var))
#   colnames(out) = c("mean","var")
#   
#   return(out)
#   
# }