
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

library(MCMCpack)
library(hts)
library(LaF)
library(data.table)
library(bsts)

source("lib/functions.R")



# 1. IMPORT DATA -----------------------------------------------------------

tsl <- import_data()





# 2. GENERATE HIERARCHY ----------------------------------------------------

# extract all exports from tsl
dat_exp <- do.call(cbind,tsl[which(substr(names(tsl),1,1) == "E")])
colnames(dat_exp) = substr(colnames(dat_exp),2,7)

# define hierarchy
dat_gts <- gts(y = dat_exp,
               gnames = c("Country Total","Goods Lvl 1","Goods Lvl 2","Goods Lvl 3",
                          "Goods Lvl 1 per Country","Goods Lvl 2 per Country"),
               characters = list(2, c(2,1,1)))

# compute summation matrix
S <- smatrix(dat_gts)

# get aggregate time series
dat_agg <- aggts(dat_gts)






# 3. FORECASTS -------------------------------------------------------------

# Forecast each series individually in order to keep model flexibility
start.time = Sys.time() # Tic
fcasts <- lapply(as.list(dat_agg), function(x) create_predictions(x, h = 24))
Sys.time() - start.time # Toc






# 4. RECONCILIATION --------------------------------------------------------

# Model Parameters
m = nrow(S) # Total number of series
q = ncol(S) # Series at the bottom level
n = 1000 # Number of draws to approximate distribution


# Tic
start.time = Sys.time()

# Loop over each period in the forecasting horizon
for(tx in 1:1){
  
  # Draw forecasts to approximate distribution
  Y = do.call(rbind, lapply(fcasts, function(x) rnorm(n = n,
                                                      mean = x[tx],
                                                      sd = x[tx]^0.5)))
  
  # Priors
  b0 = matrix(rep(0,q)) # Prior for beta chosen as diffuse as possible
  B0 = diag(rep(1e+16,q))
  v0 = rep(0,m); v0[1] = 10*n # Scale, is the weight given to the prior variance of the first error
  d0 = rep(0,m); d0[1] = 0 # Shape, variance of first error is shrunk towards zero in this case
  
  # Gibbs Sampling Preliminaries
  length_max  = 50000
  length_min = 1000
  length_sample = 5000
  
  # Preallocation
  chain = matrix(NA, nrow = length_max, ncol = q+m)
  b_save = array(NA, c(q,1,length_max))
  Sigma_save = array(NA, c(m,m,length_max))
  X_save = array(NA, c(m,1,length_max))
  
  # Starting values
  Sigma = diag(m)
  
  
  
  for(jx in 1:length_max){
    
    if(jx %% 1000 == 0){print(sprintf('Gibbs Sampler at %d out of a maximum of %d Draws.',jx,length_max))}
    
    # 1. Compute consistent hierarchical point estimate
    vb = solve(n*(t(S) %*% solve(Sigma) %*% S + solve(B0)))
    mn = vb %*% (rowSums(t(S) %*% solve(Sigma) %*% Y) + solve(B0) %*% b0)
    b = mn + t(rnorm(q,0,1) %*% chol(vb))
    
    # 2. Compute Sigma
    E = Y - matrix(1,1,n) %x% (S %*% b)
    
    Sigma <- diag(sapply(1:m, function(ix){
      
      v1 = v0[ix] + n
      d1 = d0[ix] + t(E[ix,]) %*% E[ix,]
      
      # Draw from Inverse Gamma
      1/rgamma(n = 1, shape = 0.5 * v1, rate = 0.5 * d1)
      
    }))
    
    # 3. Compute predictive Density of Y
    X = S %*% b + t(rnorm(m,0,1) %*% chol(Sigma))
    
    # X.1 Save draws
    b_save[,,jx] = b
    Sigma_save[,,jx] = Sigma
    X_save[,,jx] = X
    
    # X.2 Convergence check
    chain[jx,] = c(b,diag(Sigma))
    
    # Trace Plot
    if(jx %% 250 == 0) plot(mcmc(chain[c(1:jx),c(1,q,m)]))
    
    # Check for Convergence
    if(jx %% 1000 == 0 & jx > length_min+length_sample){
      
      check_geweke = geweke.diag(mcmc(chain[c((jx-length_sample+1):jx),]))
      print(sprintf('Convergence achieved for %d%% of chains at %d Draws.',
                    round(100*sum(abs(check_geweke$z) < 1.96)/length(check_geweke$z)),jx))
      
      
      if(sum(abs(check_geweke$z) < 1.96)/length(check_geweke$z) > 0.95){
        
        b_save = b_save[,,c((jx-length_sample+1):jx),drop=F]
        Sigma_save = Sigma_save[,,c((jx-length_sample+1):jx),drop=F]
        X_save = X_save[,,c((jx-length_sample+1):jx),drop=F]
        break
        
      }
    }
  }
  
  # Toc
  Sys.time() - start.time
  
  
  
  
  
  
  # REVISED FORECASTS -------------------------------------------------------
  
  b_mean <- apply(b_save, 1:2, mean)
  b_var <- apply(b_save, 1:2, var)
  Sigma_mean <- round(apply(Sigma_save, 1:2, mean),1)
  
  # Take directly from predictive distribution
  X_mean = apply(X_save, 1:2, mean)
  X_var = round(diag(c(apply(X_save, 1:2, var))),4)
  
  # Inconsistent
  res_inc <- fcasts
  
  # Consistent
  res_con = round(cbind(X_mean, diag(X_var)),1)
  
  # There might be minor errors between the different hierarchies 
  # if the sample from the posterior is too small, but it converges
  # to a consistent hierarchy by the law of large numbers.
  
  cmprsn <- cbind(res_inc,res_con)
  colnames(cmprsn) <- c("Inc Mean","Inc Var","Cons Mean","Cons Var")
  cmprsn
  rbind(cmprsn[1,],colSums(cmprsn[(m-q+1):m,]))
  
}








