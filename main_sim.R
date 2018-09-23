
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

library(MCMCpack)
library(hts)
library(bsts)

source("lib/functions.R")




# GENERATE DATA -----------------------------------------------------------

# Model Parameters
t <-  100
n <-  1000 # Forecast draws

# Series with random starting point, normally distributed innovations and random drift
dat <- hts(y = do.call(cbind, lapply(1:5, function(x) runif(1,50,100) + cumsum(rnorm(t,runif(1),5)))),
           nodes = list(2, c(3, 2)))

plot(dat)



# 2. GENERATE HIERARCHY ----------------------------------------------------

# compute summation matrix
S <- smatrix(dat)
m <- nrow(S) # Total series
q <- ncol(S) # Series at the bottom level

# get aggregate time series
dat_agg <- aggts(dat)






# 3. FORECASTS -------------------------------------------------------------

# Forecast each series individually

start.time = Sys.time() # Tic
fcasts <- lapply(as.list(dat_agg), function(x) create_predictions(x, h = 24))
Sys.time() - start.time # Toc




# 4. RECONCILIATION --------------------------------------------------------

# 4.1 Define Important Prior
# Reflects our certainty about reconciliation error being closer to zero
A0 = diag(rep(100,m)); A0[1,1] = 0.01 

# 4.2 Define Irrelevant Priors
b0 = matrix(rep(0,q)) # diffuse prior for beta
B0 = diag(rep(1e+16,q))
a0 = matrix(rep(0,m)) # diffuse prior for beta
v0 = rep(0,m) # scale, beta
d0 = rep(0,m) # shape, alpha

# 4.3 Gibbs Sampler Preliminaries
length_max  = 100000
length_min = 15000
length_sample = 10000


# 4.4 Loop over each forecast horizon
# (Just one forecast horizon for now)
for(hx in 5:5){ 
  
  # Preallocation
  chain = matrix(NA, nrow = length_max, ncol = q+2*m)
  alpha_save = matrix(NA,length_max,m)
  beta_save = matrix(NA,length_max,q)
  Sigma_save = matrix(NA,length_max,m)
  X_save = matrix(NA,length_max,m)
  
  # Starting values
  Sigma = diag(m)
  alpha = matrix(0,m)
  beta = matrix(0,q)
  
  # Approximate forecast distributions
  Y_pars = unname(do.call(rbind, lapply(1:m, function(mx) cbind(fcasts[[mx]][hx,1], fcasts[[mx]][hx,2]))))
  Y = do.call(rbind, lapply(1:m, function(mx) rnorm(n = n,
                                                   mean = fcasts[[mx]][hx,1],
                                                   sd = fcasts[[mx]][hx,2]^0.5)))
  
  for(jx in 1:length_max){
    
    if(jx %% 1000 == 0){print(sprintf('Gibbs Sampler at %d out of a maximum of %d Draws.',jx,length_max))}
    
    # 1. Compute Alpha
    vb = solve(n*(solve(Sigma) + solve(A0)))
    mn = vb %*% (rowSums(diag(m) %*% solve(Sigma) %*% (Y - matrix(rep(S %*% beta,n),ncol = n,nrow = m))) + solve(A0) %*% a0)
    alpha = mn + t(rnorm(m,0,1) %*% chol(vb))
    
    
    # 2. Compute Beta
    vb = solve(n*(t(S) %*% solve(Sigma) %*% S + solve(B0)))
    mn = vb %*% (rowSums(t(S) %*% solve(Sigma) %*% (Y - matrix(rep(alpha,n),ncol = n,nrow = m))) + solve(B0) %*% b0)
    beta = mn + t(rnorm(q,0,1) %*% chol(vb))
    
    
    # 3. Compute Sigma
    E = (Y - matrix(rep(alpha,n),ncol = n,nrow = m)) - matrix(1,1,n) %x% (S %*% beta)
    
    Sigma <- diag(sapply(1:m, function(ix){
      
      v1 = v0[ix] + n
      d1 = d0[ix] + t(E[ix,]) %*% E[ix,]
      
      # Draw from Inverse Gamma
      1/rgamma(n = 1, shape = 0.5 * v1, rate = 0.5 * d1)
      
    }))
    
    
    # X.1 Save draws
    alpha_save[jx,] = alpha
    beta_save[jx,] = beta
    Sigma_save[jx,] = diag(Sigma)
    X_save[jx,] = S %*% beta + t(rnorm(m,0,1) %*% chol(Sigma)) 
    
    # X.2 Convergence check
    chain[jx,] = c(alpha,beta,diag(Sigma))
    
    # Trace Plot
    if(jx %% 250 == 0) plot(mcmc(chain[c(1:jx),c(1,m+1,m+q+1)]))
    
    # Check for Convergence
    if(jx %% 1000 == 0 & jx > length_min){
      
      check_geweke = geweke.diag(mcmc(chain[c((jx-length_sample+1):jx),]))
      print(sprintf('Convergence achieved for %d%% of chains at %d Draws.',
                    round(100*sum(abs(check_geweke$z) < 1.96)/length(check_geweke$z)),jx))
      
      
      if(sum(abs(check_geweke$z) < 1.96)/length(check_geweke$z) > 0.95){
        
        alpha_out = alpha_save[c((jx-length_sample):(jx-1)),]
        beta_out = beta_save[c((jx-length_sample):(jx-1)),]
        Sigma_out = Sigma_save[c((jx-length_sample):(jx-1)),]
        X_out = X_save[c((jx-length_sample):(jx-1)),]
        break
        
      }
    }
  }
  
  # Discrete statistics of posterior distribution
  beta_mean <- colMeans(beta_out)
  beta_var <- var(beta_out)
  alpha_mean <- colMeans(alpha_out)
  alpha_var <- var(alpha_out)
  Sigma_mean <- colMeans(Sigma_out)
  X_mean = colMeans(X_out)
  X_var = var(X_out)
  
  # Check results
  check <- round(cbind(alpha_mean + S %*% beta_mean, Y_pars[,1]),2)
  colnames(check) <- c("Estimate","Original")
  print(check)
  
  # Inconsistent
  res_inc <- Y_pars
  
  # Consistent
  res_con = cbind(X_mean, diag(X_var))
  
  cmprsn <- round(cbind(res_inc,res_con),2)
  colnames(cmprsn) <- c("Inc Mean","Inc Var","Cons Mean","Cons Var")
  print(cmprsn)
  print(rbind(cmprsn[1,],colSums(cmprsn[(m-q+1):m,])))
  
}



