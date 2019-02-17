
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

library(MCMCpack)
library(hts)
set.seed(123)


source("lib/functions_model_subspace.R")


# Data
Y <- list("Y0" = c(8,1),
          "YA" = c(2,0.75),
          "YB" <- c(3,0.25))

S <- Matrix(c(1,1,0,1,0,01),3,2)
pars <- list("sparse" = T,
             "length_sample" = 1000,
             "length_max" = 1e+5,
             "fmethod" = "arima",
             "ols" = F,
             "h" = 1,
             "n" = 30,
             "m" = nrow(S),
             "q" = ncol(S),
             "shrinkage" = "none",
             "xser_shr" = 1e+5,
             "series_to_be_shrunk" = NULL)

lpars <- list("(1) OLS" = pars,
              "(2) GLS" = pars,
              "(3) Shrinkage towards Y0" = pars,
              "(4) Shrinkage towards YA" = pars)
rm(pars)

lpars$`(1) OLS`$ols <- T
lpars$`(3) Shrinkage towards Y0`$series_to_be_shrunk <- 1
lpars$`(4) Shrinkage towards YA`$series_to_be_shrunk <- 2

forecasts.list <- lapply(Y, function(yx) matrix(rnorm(lpars$`(1) OLS`$n, mean = yx[1], sd = yx[2])))

test <- lapply(names(lpars), function(lx){
  
  pars <- lpars[[lx]]
  pars$lambda <- DefineWeights(S,pars)
  results.list <- RunReconciliation(S, forecasts.list, pars)
  out <- S %*% do.call(cbind, lapply(results.list,function(x) x$beta))
  
})


dat1 <- as_tibble(t(as.matrix(do.call(cbind,test))))
colnames(dat1) <- c("Y0 ~ N(8,1)","YA ~ N(2,3/4)","YB ~ N(3,1/2)")
dat1$recon <- as_factor(names(lpars), ordered = T)
dat1 <- dat1 %>% 
  gather(ser,mean,-recon)
dat2 <- as_tibble(do.call(cbind,forecasts.list))
colnames(dat2) <- c("Y0 ~ N(8,1)","YA ~ N(2,3/4)","YB ~ N(3,1/2)")
dat2 <- dat2 %>% gather(ser,pnts)
dat <- full_join(dat1,dat2, by = "ser") %>% 
  mutate(ser = factor(ser, levels = c("YA ~ N(2,3/4)","YB ~ N(3,1/2)","Y0 ~ N(8,1)")))
  

ggplot(dat, aes(x = mean, y = pnts, color = ser)) + 
  geom_jitter(height = 0, width = 0.1, size = 0.5) +
  facet_wrap( ~ recon, ncol=2) +
  geom_abline(slope = 1, color = "grey") +
  scale_x_continuous(expression("Reconciled Forecast Mean (S"*beta*")"),breaks = seq(0,8,2)) +
  scale_y_continuous("Unreconciled Forecast Draws", breaks = seq(2,8,2)) +
  scale_color_manual("Forecasts", values = c(bpy.colors(5)[-c(1,5)])) +
  coord_cartesian(expand = FALSE, xlim = c(0,10), ylim = c(0,10)) +
  theme_bw() + theme(legend.position="bottom") +
  guides(color = guide_legend(override.aes = list(size = 2)))

ggsave("tex/fig/fig_biases.pdf", device = "pdf",
       width = 18, height = 12, units = "cm")






