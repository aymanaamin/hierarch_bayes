
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

library(MCMCpack)
library(hts)

source("lib/functions.R")

# Options
options(scipen=10)
set.seed(123)


# 1. GENERATE DATA ---------------------------------------------------------

hsim <- sim_hierarch(nodes = list(2, c(2,2)))
fcasts <- hsim$fcasts

# get parameters
S <- hsim$S
m <- nrow(S)
q <- ncol(S)
n <- 1000 # draws

# approximate forecast distributions
Y <- do.call(rbind, lapply(1:m, function(mx) rnorm(n = n,
                                                    mean = fcasts[mx,1],
                                                    sd = fcasts[mx,2]^0.5)))
# Y = do.call(rbind, lapply(1:m, function(mx) sort(rnorm(n = n,
#                                                        mean = fcasts[mx,1],
#                                                        sd = fcasts[mx,2]^0.5))))



# 4. RECONCILIATION --------------------------------------------------------

# 4.1 A0 reflects our certainty about reconciliation error being closer to zero
series_to_be_shrunk <- c(2,6)

lambda <- define_lambda(series_to_be_shrunk, S,
                        nser_shr = 0,
                        xser_shr = 1e+6)


# 4.2 Define irrelevant, diffuse priors
b0 = matrix(rep(0,q))
B0 = diag(rep(1e+16,q))
a0 = hsim$alpha;a0[series_to_be_shrunk] = 0

results <- run_gibbs(Y = Y,
                     length_max = 100000,
                     length_min = 20000,
                     length_sample = 1000)





# 5. RESULTS ---------------------------------------------------------------

# 5.1 Compare inconsistent simulated base forecasts to reconciled forecasts
cmprsn <- round(cbind(fcasts, S %*% results$beta, diag(results$Sigma_mean)),2)
cmprsn <- rbind(cmprsn,colSums(cmprsn[(m-q+1):m,]))
colnames(cmprsn) <- c("Inconsistent Mean","Var","Consistent Mean","Var")
rownames(cmprsn)[m+1] <- "Sum of Bottom-LvL"
cmprsn

# 5.2 Compare OLS, GLS and B-SUR
data.frame("Shrinkage" = as.logical(1:m %in% series_to_be_shrunk),
           "BSUR" =  round(S %*% results$beta,2), 
           "OLS" = round(S %*% (solve(t(S) %*% S) %*% t(S) %*% fcasts[,1]),2),
           "GLS" = round(S %*% (solve(t(S) %*% solve(diag(fcasts[,2])) %*% S) %*% t(S) %*% solve(diag(fcasts[,2])) %*% fcasts[,1]),2),
           "Base Mean" = round(fcasts[,1],2),
           "Base Variance" = round(fcasts[,2],2))
