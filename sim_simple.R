
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

library(MCMCpack)
library(hts)
library(bsts)

source("lib/functions.R")

# Options
options(scipen=10)
# set.seed(123)


# 1. GENERATE DATA ---------------------------------------------------------

# Model Parameters
t <-  100
n <-  1000 # Forecast draws
q <- 5 # Series at the bottom level

# Some example forecasts to play around with
# beta_sim <- runif(q,0,100)
beta_sim <- rep(20,q)
dat_hts <- hts(y = ts(t(beta_sim), start = 2015))

plot(dat_hts)



# 2. GENERATE HIERARCHY ----------------------------------------------------

# compute summation matrix
S <- smatrix(dat_hts)
m <- nrow(S)

# get aggregate time series
dat_agg <- aggts(dat_hts)





# 3. SIMULATE FORECAST ERRORS ---------------------------------------------

alpha_sim = c(20,rep(0,q))#
fcasts = cbind(t(dat_agg + alpha_sim), S %*% runif(q,0,100))
colnames(fcasts) = c("mean","var")






# 4. RECONCILIATION --------------------------------------------------------

# 4.1 Define Important Prior
# Reflects our certainty about reconciliation error being closer to zero
# A0 = diag(rep(100,m))#; A0[3,3] = 0.01 
A0 = diag(fcasts[,2])

# 4.2 Define Irrelevant Priors
b0 = matrix(rep(0,q)) # diffuse prior for beta
B0 = diag(rep(1e+16,q))
a0 = matrix(rep(0,m)) # diffuse prior for beta
v0 = rep(0,m) # scale, beta
d0 = rep(0,m) # shape, alpha

# 4.3 Gibbs Sampler Preliminaries
length_max = 10000
length_min = 5000
length_sample = 1000

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
Y = do.call(rbind, lapply(1:m, function(mx) rnorm(n = n,
                                                  mean = fcasts[mx,1],
                                                  sd = fcasts[mx,2]^0.5)))

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
  if(jx %% 1000 == 0) plot(mcmc(chain[c(1:jx),c(1,m+1,m+q+1)]))
  
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



# 5. RESULTS ---------------------------------------------------------------

# Compute discrete statistics of posterior distribution
beta_mean <- colMeans(beta_out)
beta_var <- var(beta_out)
alpha_mean <- colMeans(alpha_out)
alpha_var <- var(alpha_out)
Sigma_mean <- colMeans(Sigma_out)
X_mean = colMeans(X_out)
X_var = var(X_out)

# 5.1 Compare Fitted to Simulated Forecasts
check <- round(cbind(alpha_mean + S %*% beta_mean, fcasts[,1]),2)
colnames(check) <- c("Fitted","Original")
print(check)


# 5.2 Compare Fitted to Simulated Forecasts
cmprsn <- round(cbind(fcasts,cbind(X_mean, diag(X_var))),2)
colnames(cmprsn) <- c("Inc Mean","Inc Var","Cons Mean","Cons Var")
print(cmprsn)
cmprsn_sums <- rbind(cmprsn[1,],colSums(cmprsn[(m-q+1):m,]))
rownames(cmprsn_sums) <- c("Total","Sum of Bottom-LvL")
print(cmprsn_sums)

# 5.3 Compare OLS, WLS and B-SUR
regs <- round(cbind(solve(t(S) %*% S) %*% t(S) %*% fcasts[,1],
                    solve(t(S) %*% solve(diag(fcasts[,2])) %*% S) %*% t(S) %*% solve(diag(fcasts[,2])) %*% fcasts[,1],
                    beta_mean,
                    beta_sim),2)
colnames(regs) <- c("OLS","WLS","B-SUR","Simulated")
regs


# 5.4 Compare simulated reconciliation errors and estimated ones 
print(cbind(alpha_sim, alpha_mean))



