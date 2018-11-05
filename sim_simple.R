
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
n <-  1000 # Forecast draws
q <- 10 # Series at the bottom level

# Some example forecasts to play around with
beta_sim <- runif(q,100,100)
alpha_sim = c(0,runif(q,-10,10))
dat_hts <- hts(ts(t(beta_sim)))
S <- smatrix(dat_hts)
m <- nrow(S)
dat_agg <- aggts(dat_hts)
fcasts = cbind(t(dat_agg + alpha_sim), S %*% runif(q,0,10))
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

# 4.1 A0 reflects our certainty about reconciliation error being closer to zero
# series_to_be_shrunk <- c(1) # Shrink the recon error on the top level series
series_to_be_shrunk <- c(1,4,6) # Shrink selected series
# series_to_be_shrunk <- c(1:10) # Shrink all but the error on the last bottom level series
A0 <- define_prior(cov = diag(diag(var(t(Y))))/n,
                   x = series_to_be_shrunk,
                   factor = 1e+12)

# 4.2 Define irrelevant, diffuse priors
b0 = matrix(rep(0,q))
B0 = diag(rep(1e+16,q))
a0 = matrix(rep(0,m))

results <- run_gibbs(length_max = 100000,
                     length_min = 5000,
                     length_sample = 1000)





# 5. RESULTS ---------------------------------------------------------------

# # 5.1 Compare fitted to simulated Forecasts, should be about the same
# check <- round(cbind(results$alpha + S %*% results$beta, fcasts[,1]),2)
# colnames(check) <- c("Fitted","Original")
# check

# 5.2 Compare inconsistent simulation to reconciled forecasts
# There might be small deviations between the top forecast and the sum of the bottom-levels
cmprsn <- round(cbind(fcasts, S %*% results$beta, diag(results$Sigma_mean)),2)
cmprsn <- rbind(cmprsn,colSums(cmprsn[(m-q+1):m,]))
colnames(cmprsn) <- c("Inconsistent Mean","Var","Consistent Mean","Var")
rownames(cmprsn)[m+1] <- "Sum of Bottom-LvL"
print(cmprsn)

# 5.3 Compare OLS, GLS and B-SUR
data.frame("Shrinkage" = as.logical(1:m %in% series_to_be_shrunk),
           "BSUR" =  round(S %*% results$beta,2), 
           "OLS" = round(S %*% (solve(t(S) %*% S) %*% t(S) %*% fcasts[,1]),2),
           "GLS" = round(S %*% (solve(t(S) %*% solve(diag(fcasts[,2])) %*% S) %*% t(S) %*% solve(diag(fcasts[,2])) %*% fcasts[,1]),2),
           "Simulated Mean" = round(fcasts[,1],2),
           "Simulated Variance" = round(fcasts[,2],2))


# # # 5.4 Compare simulated reconciliation errors and estimated ones 
# print(cbind(alpha_sim, results$alpha))

# # Standard errors from regression output
# diag(summary(lm(matrix(Y) ~ -1 + rep(1,n) %x% diag(m), weights = rep(1/fcasts[,2],n)))$cov.unscaled)

