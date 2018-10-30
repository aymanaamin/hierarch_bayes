
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
  
  # # remove entries at the lowest level of aggregation
  # total = total[-which(nchar(total$group) == 12 | nchar(total$group) == 9), ]
  
  # generate keys
  total[, group_clean := gsub("[.]","",group)]
  total[, group_nchar := nchar(group_clean)] 
  total[, group_8dig := sapply(1:nrow(total), function(jx) {
    paste0(total$group_clean[jx],
           paste(rep(0, 8 - total$group_nchar[jx]),collapse = ""))})]
  total[, tsKey := paste0(direction, regCode, isoCode, group_8dig)]
  
  # retrieve dates
  toNumericDate <- function(x) {
    num <- as.numeric(x)
    mnth <- floor(num)
    10000*(num - mnth) + (mnth - 1)/12
  }
  total[, dateNumeric := toNumericDate(period)]
  
  # transform to time series
  value_dt <- dcast(total[, .(tsKey, dateNumeric, value)], "dateNumeric ~ tsKey", value.var = "value", fun.aggregate = sum)
  class(value_dt) <- "data.frame"
  tsl <- as.list(as.ts(zoo::zoo(x = value_dt[,-1], order.by = value_dt[, 1])))
  tsl <- lapply(tsl, function(x) {
    x[is.na(x)] <- 0
    x
  })
  
  return(tsl)
  
}

create_predictions <- function(x,h){
  
  # Run the bsts model
  ss = AddLocalLinearTrend(list(), x)
  ss = AddSeasonal(ss, x, nseasons = 4)
  bsts.model = bsts(x, state.specification = ss, niter = 500, ping = 0)
  
  # Get a suggested number of burn-ins
  burn = SuggestBurn(0.1, bsts.model)
  
  # Predict
  p = predict.bsts(bsts.model, horizon = h, burn = burn)
  
  out = cbind(p$mean,apply(p$distribution, 2, var))
  colnames(out) = c("mean","var")
  
  return(out)
  
}


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
  X_save = matrix(NA,length_max,m)
  
  # Starting values
  Sigma = diag(m)
  alpha = matrix(0,m)
  beta = matrix(0,q)
  
  for(jx in 1:length_max){
    
    if(jx %% 1000 == 0){print(sprintf('Gibbs Sampler at %d out of a maximum of %d Draws.',jx,length_max))}
    
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
    X_save[jx,] = S %*% beta + t(rnorm(m,0,1) %*% chol(Sigma)) 
    
    # X.2 Convergence check
    chain[jx,] = c(alpha,beta,diag(Sigma))
    
    # Trace Plot
    if(jx %% 1000 == 0) plot(mcmc(chain[c(1:jx),c(1,m+1,m+q+1)]))
    
    # Check for Convergence
    if(jx %% 1000 == 0 & jx > length_min){
      
      check_geweke = geweke.diag(mcmc(chain[c((jx-length_sample+1):jx),]))
      print(sprintf('Convergence achieved for %d%% of chains.',
                    round(100*sum(abs(check_geweke$z) < 1.96)/length(check_geweke$z)),jx))
      
      
      if(sum(abs(check_geweke$z) < 1.96)/length(check_geweke$z) > 0.9){
        
        alpha_out = alpha_save[c((jx-length_sample):(jx-1)),]
        beta_out = beta_save[c((jx-length_sample):(jx-1)),]
        Sigma_out = Sigma_save[,,c((jx-length_sample):(jx-1))]
        X_out = X_save[c((jx-length_sample):(jx-1)),]
        
        # Compute discrete statistics of posterior distribution
        result <- list(
          "beta" = colMeans(beta_out),
          "beta_var" = var(beta_out),
          "alpha" = colMeans(alpha_out),
          "alpha_var" = var(alpha_out),
          "Sigma_mean" = apply(Sigma_out, 1:2, mean),
          "X" = colMeans(X_out),
          "X_var" = var(X_out))
        
        return(result)
        
      }
    }
  }
}
