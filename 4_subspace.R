
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

library(MCMCpack)
library(hts)


source("lib/functions_model_subspace.R")


# Data
Y <- list("Y0" = c(16,3),
          "YA" = c(4,2),
          "YB" <- c(6,1))

S <- Matrix(c(1,1,0,1,0,01),3,2)
pars <- list("sparse" = T,
             "length_sample" = 1000,
             "length_max" = 1e+5,
             "fmethod" = "arima",
             "ols" = F,
             "h" = 1,
             "n" = 200,
             "m" = nrow(S),
             "q" = ncol(S),
             "shrinkage" = "none",
             "xser_shr" = 1e+5,
             "series_to_be_shrunk" = NULL)

lpars <- list("ols" = pars,
              "gls1" = pars,
              "gls2" = pars,
              "gls3" = pars)
rm(pars)

lpars$ols$ols <- T
lpars$gls2$series_to_be_shrunk <- 1
lpars$gls3$series_to_be_shrunk <- 2

forecasts.list <- lapply(Y, function(yx) matrix(rnorm(lpars$ols$n, mean = yx[1], sd = yx[2])))

test <- lapply(names(lpars), function(lx){
  
  pars <- lpars[[lx]]
  pars$lambda <- DefineWeights(S,pars)
  results.list <- RunReconciliation(S, forecasts.list, pars)
  out <- S %*% do.call(cbind, lapply(results.list,function(x) x$beta))
  
})


dat1 <- as_tibble(t(as.matrix(do.call(cbind,test))))
colnames(dat1) <- c("Y0","YA","YB")
dat1$recon <- as_factor(names(lpars), ordered = T)
dat1 <- dat1 %>% 
  gather(ser,mean,-recon)
dat2 <- as_tibble(do.call(cbind,forecasts.list))
colnames(dat2) <- c("Y0","YA","YB")
dat2 <- dat2 %>% gather(ser,pnts)
dat <- full_join(dat1,dat2, by = "ser") %>% 
  mutate(ser = factor(ser, levels = c("YA","YB","Y0"))) %>% 
  mutate(recon = factor(recon, labels = c("`(1) OLS`",
                                          "`(2) GLS`",
                                          "`(3) GLS & Shrinkage towards Y`[0]",
                                          "`(4) GLS & Shrinkage towards Y`[A]")))


labs <- c(expression(Y[A] %~% N(4,2)),
          expression(Y[B] %~% N(6,1)),
          expression(Y["0"] %~% N(16,3)))

ggplot(dat, aes(x = mean, y = pnts, fill = ser, color = ser)) + 
  geom_abline(slope = 1, color = "grey") +
  geom_boxplot(width = 2, varwidth = F, outlier.size = NA,
               position = position_identity(), alpha = 0.2) +
  facet_wrap( ~ recon, ncol=2, labeller = label_parsed) +
  scale_x_continuous(expression("Reconciled Forecast Mean (S"*beta*")"), breaks = seq(5,15,5)) +
  scale_y_continuous("Unreconciled Forecast Draws", breaks = seq(0,20,5)) +
  scale_color_manual("Forecasts", values = c(bpy.colors(5)[-c(1,5)]),
                     labels = labs) +
  scale_fill_manual("Forecasts", values = c(bpy.colors(5)[-c(1,5)]),
                    labels = labs) +
  coord_cartesian(expand = FALSE, xlim = c(0,20), ylim = c(0,20)) +
  theme_bw() + theme(legend.position="bottom") 

ggsave("tex/fig/fig_biases.pdf", device = "pdf",
       width = 18, height = 12, units = "cm")






