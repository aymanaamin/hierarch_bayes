
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

library(MCMCpack)
library(hts)
library(bsts)

source("lib/functions.R")

# Options
options(scipen=10)
set.seed(123)


# 1. GENERATE DATA ---------------------------------------------------------

# Model Parameters
n <-  100 # Forecast draws
q <- 10 # Series at the bottom level

# Some example forecasts to play around with
beta_sim <- rep(100,q)
alpha_sim = c(50,rep(0,q))
dat_hts <- hts(ts(t(beta_sim)))
S <- smatrix(dat_hts)
m <- nrow(S)
dat_agg <- aggts(dat_hts)
fcasts = cbind(t(dat_agg + alpha_sim), S %*% runif(q,0,50))
colnames(fcasts) = c("mean","var")
fcasts

# Approximate forecast distributions
Y = do.call(rbind, lapply(1:m, function(mx) rnorm(n = n,
                                                  mean = fcasts[mx,1],
                                                  sd = fcasts[mx,2]^0.5)))
# Y = do.call(rbind, lapply(1:m, function(mx) sort(rnorm(n = n,
#                                                        mean = fcasts[mx,1],
#                                                        sd = fcasts[mx,2]^0.5))))



# 4. RECONCILIATION --------------------------------------------------------

# 4.1 Define Important Prior
# Reflects our certainty about reconciliation error being closer to zero
A0 = diag(fcasts[,2])
A0[1,1] = 0.1
# A0 = diag(10,m)

# 4.2 Define Irrelevant Priors
b0 = matrix(rep(0,q)) # diffuse prior for beta
B0 = diag(rep(1e+16,q))
a0 = matrix(rep(0,m)) # diffuse prior for beta

results <- run_gibbs(length_max = 100000,
                     length_min = 10000,
                     length_sample = 10000)





# 5. RESULTS ---------------------------------------------------------------

# 5.1 Compare fitted to simulated Forecasts, should be about the same
check <- round(cbind(results$alpha + S %*% results$beta, fcasts[,1]),2)
colnames(check) <- c("Fitted","Original")
print(check)

# 5.2 Compare inconsistent simulation to reconciled forecasts
# There might be small deviations between the top forecast and the sum of the bottom-levels
cmprsn <- round(cbind(fcasts,cbind(results$X), diag(results$X_var)),2)
colnames(cmprsn) <- c("Inc Mean","Inc Var","Cons Mean","Cons Var")
print(cmprsn)
cmprsn_sums <- rbind(cmprsn[1,],colSums(cmprsn[(m-q+1):m,]))
rownames(cmprsn_sums) <- c("Total","Sum of Bottom-LvL")
print(cmprsn_sums)

# 5.3 Compare OLS, GLS and B-SUR
regs <- round(cbind(solve(t(S) %*% S) %*% t(S) %*% fcasts[,1],
                    solve(t(S) %*% solve(diag(fcasts[,2])) %*% S) %*% t(S) %*% solve(diag(fcasts[,2])) %*% fcasts[,1],
                    results$beta,
                    beta_sim),2)
colnames(regs) <- c("OLS","GLS","B-SUR","Simulated")
regs


# 5.4 Compare simulated reconciliation errors and estimated ones 
print(cbind(alpha_sim, results$alpha))



