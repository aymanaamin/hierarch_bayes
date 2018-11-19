
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


# This function returns a sparse matrix supported by Matrix pkg
smatrix2 <- function(xgts) { 
  # Sparse matrices stored in coordinate format
  # gmatrix contains all the information to generate smatrix
  gmat <- xgts$groups
  num.bts <- ncol(gmat)
  sparse.S <- apply(gmat, 1L, function(x) {
    ia <- as.integer(x)
    ra <- as.integer(rep(1L, num.bts))
    ja <- as.integer(1L:num.bts)
    s <- sparseMatrix(i = ia, j = ja, x = ra)
  })
  sparse <- do.call("rbind", sparse.S)
  return(sparse)
}


run_gibbs <- function(length_max,length_min,length_sample){
  
  # Preallocation
  chain = matrix(NA, nrow = length_max, ncol = q+2*m)
  alpha_save = matrix(NA,length_max,m)
  beta_save = matrix(NA,length_max,q)
  Sigma_save = array(NA,c(m,m,length_max))
  A1_save = array(NA,c(m,m,length_max))
  A0_save = array(NA,c(m,m,length_max))
  X_save = matrix(NA,length_max,m)
  
  # Starting values
  Sigma = diag(m)
  alpha = matrix(0,m)
  beta = solve(t(S) %*% S) %*% t(S) %*% rowMeans(Y) 
  
  for(jx in 1:length_max){
    
    if(jx %% 1000 == 0){print(sprintf('Gibbs Sampler at %d out of a maximum of %d Draws.',jx,length_max))}
    
    A0 <- riwish(v = n, S = lambda^0.5 %*%  Sigma %*% lambda^0.5)
    
    # 1. Compute Alpha
    A1 <- solve(n*solve(Sigma) + solve(A0))
    a1 <- A1 %*% (rowSums(diag(m) %*% solve(Sigma) %*% (Y - matrix(rep(S %*% beta,n),ncol = n,nrow = m))) + solve(A0) %*% a0)
    alpha <- a1 + t(rnorm(m,0,1) %*% chol(A1))
    
    
    # 2. Compute Beta
    B1  <- solve(n*(t(S) %*% solve(Sigma) %*% S) + solve(B0))
    b1 <- B1 %*% (rowSums(t(S) %*% solve(Sigma) %*% (Y - matrix(rep(alpha,n),ncol = n,nrow = m))) + solve(B0) %*% b0)
    beta <- b1 + t(rnorm(q,0,1) %*% chol(B1))
    
    
    # 3. Compute Sigma
    E <- (Y - matrix(rep(alpha,n),ncol = n,nrow = m)) - matrix(1,1,n) %x% (S %*% beta)
    Sigma <- riwish(v = n, S = E %*% t(E))
    
    
    # X.1 Save draws
    alpha_save[jx,] = alpha
    beta_save[jx,] = beta
    Sigma_save[,,jx] = Sigma
    A1_save[,,jx] = A1
    A0_save[,,jx] = A0
    X_save[jx,] = S %*% beta + t(rnorm(m,0,1) %*% chol(Sigma)) 
    
    # X.2 Convergence check
    if(jx == 1){
      chain[jx,] = c(alpha,beta,diag(Sigma))
    } else {
      chain[jx,] = (chain[jx-1,]*(jx-1) + c(alpha,beta,diag(Sigma)))/jx
    }
    
    # Trace Plot
    if(jx %% 1000 == 0) plot(mcmc(chain[c(1:jx),c(1,m+1,m+q+1)]))
    
    # Check for Convergence
    if(jx == length_min){
      
      # Compute discrete statistics of posterior distribution
      results <- list(
        "beta" = colMeans(beta_save[c((jx-length_sample):(jx-1)),]),
        "beta_var" = var(beta_save[c((jx-length_sample):(jx-1)),]),
        "alpha" = colMeans(alpha_save[c((jx-length_sample):(jx-1)),]),
        "alpha_var" = var(alpha_save[c((jx-length_sample):(jx-1)),]),
        "Sigma_mean" = apply(Sigma_save[,,c((jx-length_sample):(jx-1))], 1:2, mean),
        "A1" = apply(A1_save[,,c((jx-length_sample):(jx-1))], 1:2, mean),
        "A0" = apply(A0_save[,,c((jx-length_sample):(jx-1))], 1:2, mean),
        "X" = colMeans(X_save[c((jx-length_sample):(jx-1)),]),
        "X_var" = var(X_save[c((jx-length_sample):(jx-1)),]))
      
      return(results)
      
    }
  }
}


define_lambda <- function(x, eta){
  
  mat = diag(m)
  
  if(!is.null(x)){
    
    mat[x,x] <- mat[x,x]/eta^(1/length(x))
    mat[-x,-x] <-  mat[-x,-x] * eta^(1/(ncol(mat)-length(x)))
    
  }
  
  return(mat)
  
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
  grid <- expand.grid(list("Reconciliation" = recon,
                           "Forecast" = fmethod,
                           "Date" = as.character(fdate),
                           "Horizon" = as.character(horizon)),
                      stringsAsFactors = F)
  out <- as.matrix(t(sapply(1:nrow(grid), function(ix) t(results[[grid[ix,1]]][[grid[ix,2]]]
                                               [[grid[ix,3]]][[grid[ix,4]]])[levels,measure])))
  if(length(levels) == 1) out <- t(out)
  colnames(out) <- levels
  cbind(grid,out)
}
