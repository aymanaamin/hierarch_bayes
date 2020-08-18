
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

library(MCMCpack)
library(hts)
library(tidyverse)
library(bsr)
library(Matrix)
library(RColorBrewer)
library(foreach)
library(doParallel)


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
             "n" = 1000,
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
dat1$recon <- factor(names(lpars), ordered = T)
dat1 <- dat1 %>% 
  gather(ser,mean,-recon)
dat2 <- as_tibble(do.call(cbind,forecasts.list))
colnames(dat2) <- c("Y0","YA","YB")
dat2 <- dat2 %>% gather(ser,pnts)
dat <- full_join(dat1,dat2, by = "ser") %>% 
  mutate(ser = factor(ser, levels = c("YA","YB","Y0"))) %>% 
  mutate(recon = factor(recon, levels = c("ols","gls1","gls2","gls3"),
                        labels = c("`(1) No Scaling (OLS)`",
                                   "`(2) Variance Scaling (GLS)`",
                                   "`(3) Variance Scaling & Shrinkage towards Y`[0]",
                                   "`(4) Variance Scaling & Shrinkage towards Y`[A]")))


labs <- c(expression(Y[A] %~% N(4,2)),
          expression(Y[B] %~% N(6,1)),
          expression(Y["0"] %~% N(16,3)))

ggplot(dat, aes(x = mean, y = pnts, fill = ser, color = ser)) + 
  geom_abline(slope = 1, color = "grey") +
  geom_boxplot(width = 2, varwidth = F, outlier.size = -1,
               position = position_identity(), alpha = 0.2) +
  facet_wrap( ~ recon, ncol=2, labeller = label_parsed) +
  scale_x_continuous(expression("Reconciled Forecast Mean (S"*beta*")"), breaks = seq(4,16,4), minor_breaks = NULL) +
  scale_y_continuous("Unreconciled Forecast Draws", breaks = seq(4,16,4), minor_breaks = NULL) +
  scale_color_manual("Unreconciled Base Forecasts", values = brewer.pal(4, "Blues")[-1],
                     labels = labs) +
  scale_fill_manual("Unreconciled Base Forecasts", values = brewer.pal(4, "Blues")[-1],
                    labels = labs) +
  coord_flip(expand = FALSE, xlim = c(0,20), ylim = c(0,20)) +
  theme_minimal() +
  theme(legend.position="bottom", 
        strip.text = element_text(size = 11)) 

ggsave("tex/fig/fig_biases.pdf", device = "pdf",
       width = 18, height = 12, units = "cm")





# COMPARE BSR SHRINKAGE TO OUTCOMES OF OTHER METHODS ------------------

load("dat/tradehts_reduced1.Rdata")
load("dat/tradegts_reduced1.Rdata")

# parameters
dates <- seq(1998,2018,1/12)
h <- 12

cl <- makeCluster(4)
registerDoParallel(cl)

out <- foreach(dx = dates, .packages = c("bsr",
                                         "hts",
                                         "forecast","tidyverse")) %dopar% {
  
  # run reconciliations
  estim <- list("pred_bu" = forecast(window(tradegts_reduced1, end = dx-1/12), 
                                     h = h, method = "bu" , fmethod = "rw"),
                "pred_td_cat" = forecast(window(tradehts_reduced1$cat, end = dx-1/12), 
                                         h = h, method = "tdgsa" , fmethod = "rw"),
                "pred_td_reg" = forecast(window(tradehts_reduced1$reg, end = dx-1/12), 
                                         h = h, method = "tdgsa" , fmethod = "rw"),
                "pred_mint" = forecast(window(tradegts_reduced1, end = dx-1/12), 
                                     h = h, method = "comb", weights = "mint", fmethod = "rw"),
                "pred_bsr_bu" = bsr(window(tradegts_reduced1, end = dx-1/12), 
                                    length_sample = 1000, h = h, shrinkage = "bu", 
                                    fmethod = "rw"),
                "pred_bsr_td" = bsr(window(tradegts_reduced1, end = dx-1/12), 
                                    length_sample = 1000, h = h, shrinkage = "td",
                                    fmethod = "rw"),
                "pred_bsr_no" = bsr(window(tradegts_reduced1, end = dx-1/12), 
                                    length_sample = 1000, h = h,
                                    fmethod = "rw"))
  
  # get base forecast
  base <- sapply(estim$pred_bsr_bu$base_forecasts, function(x) colMeans(x))
  colnames(base) <- unname(sapply(colnames(base), function(x){
    
    if(grepl("/",x)) strsplit(x, "/", fixed = T)[[1]][2] else x
    
  }))
  tab_base <- as_tibble(base) %>% 
    add_column("model" = "base", "horizon" = 1:12) %>% 
    pivot_longer(-c(model,horizon))
  
  
  # td
  td_mat <- matrix(NA, nrow(base), ncol(base))
  colnames(td_mat) <- colnames(base)
  td_mat[,colnames(aggts(estim$pred_td_cat))] <- aggts(estim$pred_td_cat)
  td_mat[,colnames(aggts(estim$pred_td_reg))] <- aggts(estim$pred_td_reg)
  tab_td <- as_tibble(td_mat) %>% 
    add_column("model" = "td", 
               "horizon" = 1:12) %>% 
    pivot_longer(-c(model,horizon)) %>% 
    add_column("base" = tab_base$value)
  
  
  # bu
  bu_mat <- aggts(estim$pred_bu)
  colnames(bu_mat) <- colnames(base)
  tab_bu <- as_tibble(bu_mat) %>% 
    add_column("model" = "bu", "horizon" = 1:12) %>% 
    pivot_longer(-c(model,horizon)) %>% 
    add_column("base" = tab_base$value)
  
  # bu bsr
  bu_bsr_mat <- aggts(estim$pred_bsr_bu)
  colnames(bu_bsr_mat) <- colnames(base)
  tab_bu_bsr <- as_tibble(bu_bsr_mat) %>% 
    add_column("model" = "bu_bsr", "horizon" = 1:12) %>% 
    pivot_longer(-c(model,horizon))  %>% 
    add_column("base" = tab_base$value)
  
  
  # td_bsr
  td_bsr_mat <- aggts(estim$pred_bsr_td)
  colnames(td_bsr_mat) <- colnames(base)
  tab_td_bsr <- as_tibble(td_bsr_mat) %>% 
    add_column("model" = "td_bsr", "horizon" = 1:12) %>% 
    pivot_longer(-c(model,horizon)) %>% 
    add_column("base" = tab_base$value)
  
  # no_bsr
  no_bsr_mat <- aggts(estim$pred_bsr_no)
  colnames(no_bsr_mat) <- colnames(base)
  tab_no_bsr <- as_tibble(no_bsr_mat) %>% 
    add_column("model" = "no_bsr", "horizon" = 1:12) %>% 
    pivot_longer(-c(model,horizon)) %>% 
    add_column("base" = tab_base$value)
  
  
  # mint
  mint_mat <- aggts(estim$pred_mint)
  colnames(mint_mat) <- colnames(base)
  tab_mint <- as_tibble(mint_mat) %>% 
    add_column("model" = "mint", "horizon" = 1:12) %>% 
    pivot_longer(-c(model,horizon)) %>% 
    add_column("base" = tab_base$value)
  
  
  # out
  rbind(tab_td,tab_bu,tab_bu_bsr,tab_td_bsr,tab_mint,tab_no_bsr)
  
  
}

stopCluster(cl)


mat <- do.call(rbind,out) 

save(mat, file = "out/deviations.Rdata")


load("out/deviations.Rdata")

mat <- mat %>% 
  add_column("err" = abs((mat$value - mat$base)/mat$base)) %>% 
  group_by(model, name) %>% 
  # summarize(err = sqrt(mean(se))) %>%
  summarize(mape = mean(err)*100) %>% 
  ungroup()


reg_lvl <- factor(case_when(
  mat$name == "Total" ~ "World",
  nchar(mat$name) == 2 & grepl("^[A-Za-z]+$", mat$name) == F ~ "World",
  nchar(mat$name) == 3 & grepl("^[A-Za-z]+$", mat$name) == F ~ "World",
  nchar(mat$name) == 2 & grepl("^[A-Za-z]+$", mat$name) == T ~ "Region",
  nchar(mat$name) %in% c(4,5) & grepl("^[A-Za-z]+$", mat$name) == F ~ "Region"),
  levels = c("World","Region"), ordered = T)
cat_lvl <-  factor(case_when(
  mat$name == "Total" ~ "Total",
  nchar(mat$name) %in% c(2,4) & grepl("^[A-Za-z]+$", mat$name) == T ~ "Total",
  nchar(mat$name) %in% c(2) & grepl("^[A-Za-z]+$", mat$name) == F ~ "Category",
  nchar(mat$name) %in% c(4,6) & grepl("^[A-Za-z]+$", mat$name) == F ~ "Category"),
  levels = c("Total","Category"), ordered = T)

mat2 <- mat %>% 
  add_column(reg_lvl = reg_lvl, cat_lvl = cat_lvl) %>% 
  group_by(model, reg_lvl, cat_lvl) %>%
  summarize(mape = mean(mape)) %>% 
  ungroup()

mat2 %>% print(n=30)









