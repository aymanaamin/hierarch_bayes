
# 0. PRELIMINARIES --------------------------------------------------------

rm(list = ls())

# Packages
library(hts)
library(forecast)
library(data.table)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(sp)

# Data & Metadata
# load("out/results_backtest.Rdata")
load("out/tab_mase.Rdata")
load("out/tab_rmse.Rdata")

# Options
options(scipen=10)



# compute weights
load("dat/tradegts.Rdata")
tsl <- as.list(aggts(tradegts)/1e+9)
weights <- sapply(tsl, function(x) mean(x)/mean(tsl$Total))
names(weights)[grepl("/",names(weights))] <- sapply(strsplit(names(weights)[grepl("/",names(weights))],"/"),`[`,2)
rm(tsl)




# RECONCILIATION ACCURACY BY TIME -----------------------------------------

# x = year, y = accuracy, lines = reconciliation, facets = horizon
tab <- as_tibble(tab_mase) %>% 
  gather(key = level, value = accuracy, -c(date,recon,fmethod,horizon)) %>% 
  filter(fmethod == "arima" & level == "Total") %>% 
  rename(Date = date, Reconciliation = recon,
         Accuracy = accuracy,
         Horizon = horizon)

ggplot(data = tab, aes(Date, Accuracy, group = Reconciliation)) + 
  geom_line(aes(colour = Reconciliation)) +
  facet_grid(Horizon ~ .) +
  scale_x_discrete(breaks = seq(1998, 2018, by = 4)) +
  theme_bw() +
  theme(legend.position="bottom")


# RECONCILIATION ACCURACY BY METHOD ---------------------------------------

# x = reconciliation methods, y = accuracy
tab <- as_tibble(tab_mase) %>% 
  gather(key = level, value = accuracy, -c(date,recon,fmethod,horizon)) %>% 
  filter(fmethod == "arima" & level == "Total" & 
           !(recon %in% c("tdgsa_cat","tdgsa_reg",
                          "tdgsf_cat","tdgsf_reg")))  %>% 
  group_by(recon,horizon) %>%
  summarise(mean = mean(accuracy)) %>%
  mutate(Reconciliation = factor(recon,
                                 levels = c("unrecon","tdfp_cat","tdfp_reg","ols","nseries","mint","wls","mo_reg",
                                            "bu","mo_cat"),
                                 labels = c("Unreconciled","Top-Down (Categories)","Top-Down (Regions)",
                                            "OLS","nSeries","MinT","WLS","Middle-Out (Regions)",
                                            "Bottom-Up","Middle-Out (Categories)"),
                                 ordered = T)) %>% 
  rename(Accuracy = mean) %>% 
  mutate(Horizon = recode(horizon,
                          "1" = "1y",
                          "2" = "2y",
                          "3" = "3y")) 


ggplot(tab, aes(Reconciliation, Accuracy)) +   
  geom_point() +
  xlab(NULL) +
  facet_grid(. ~ Horizon, scales = "free_x") +
  ylab("Forecast Accuracy (MASE)") +
  coord_flip() +
  theme_bw() +
  theme(legend.position="bottom")        



# RECONCILIATION ACCURACY BY LEVEL ----------------------------------------

# x = reconciliation methods, y = accuracy
tab <- as_tibble(tab_mase) %>% 
  gather(key = level, value = accuracy, -c(date,recon,fmethod,horizon)) %>% 
  filter(!(recon %in% c("tdgsa_cat","tdgsa_reg","tdgsf_cat","tdgsf_reg"))) %>% 
  mutate(reg_lvl = factor(case_when(
    level == "Total" ~ "World",
    nchar(level) == 2 & grepl("^[A-Za-z]+$", level) == T ~ "Region",
    nchar(level) == 2 & grepl("^[A-Za-z]+$", level) == F ~ "World",
    nchar(level) == 4 & grepl("^[A-Za-z]+$", level) == F ~ "Region"),
    levels = c("World","Region","Country"), ordered = T),
    cat_lvl = factor(case_when(
      level == "Total" ~ "Total",
      nchar(level) == 2 & grepl("^[A-Za-z]+$", level) == T ~ "Total",
      nchar(level) == 2 & grepl("^[A-Za-z]+$", level) == F ~ "Category",
      nchar(level) == 4 & grepl("^[A-Za-z]+$", level) == F ~ "Category"),
      levels = c("Total","Category","Subcategory"), ordered = T)) %>% 
  mutate(Reconciliation = factor(recon,
                                 levels = c("unrecon","bu","mo_cat","mo_reg","tdfp_cat","tdfp_reg",
                                            "mint","wls","ols","nseries"),
                                 labels = c("Unreconciled","Bottom-Up","Middle-Out (Categories)  ","Middle-Out (Regions)",
                                            "Top-Down (Categories)  ","Top-Down (Regions)",
                                            "MinT  ","WLS","OLS","nSeries"),
                                 ordered = T),
         Grouping = recode(recon,
                           "bu" = "Basic",
                           "mo_cat" = "Basic",
                           "mo_reg" = "Basic",
                           "tdgsa_cat" = "Basic",
                           "tdgsa_reg" = "Basic",
                           "tdgsf_reg" = "Basic",
                           "tdgsf_cat" = "Basic",
                           "tdfp_reg" = "Basic",
                           "tdfp_cat" = "Basic",
                           "ols" = "Optimal",
                           "wls" = "Optimal",
                           "nseries" = "Optimal",
                           "mint" = "Optimal",
                           "unrecon" = "Basic")) %>% 
  group_by(Reconciliation,Grouping,reg_lvl,cat_lvl,level) %>%
  summarise(Accuracy = mean(accuracy))
tab2 <- tab %>% 
  add_column(weights = sapply(tab$level, function(x) weights[which(names(weights) == x)])) %>% 
  mutate(acc_weighted = Accuracy*weights) %>% 
  summarise(Accuracy = sum(acc_weighted))


ggplot(tab2, aes(x=Grouping, y=Accuracy, fill = Reconciliation)) +   
  geom_bar(stat="identity", position = position_dodge()) +
  facet_grid(reg_lvl ~ cat_lvl, switch = "y") +
  scale_y_continuous(position = "right") +
  scale_fill_manual(values = c(bpy.colors(10))) +
  ylab("Forecast Accuracy (MASE)") +
  xlab("Reconciliation Methods") +
  theme_bw() +
  coord_cartesian(ylim = c(2,2.8)) +
  theme(legend.position="bottom", legend.title = element_blank())  +
  guides(fill=guide_legend(nrow=3))

ggsave("tex/fig/fig_eval_levels.pdf", device = "pdf",
       width = 18, height = 12, units = "cm")



# FORECASTING METHODS BY TIME ---------------------------------------------

# x = year, y = accuracy, lines = fmethod, facets = horizon
tab <- as_tibble(tab_mase) %>% 
  gather(key = level, value = accuracy, -c(date,recon,fmethod,horizon)) %>% 
  filter(level == "Total" | nchar(level) == 4) %>% 
  mutate(level = as_factor(recode(level,"Total" = "Top", .default = "Bottom"))) %>% 
  group_by(date,fmethod,horizon,level) %>%
  summarise(mean = mean(accuracy,na.rm=T)) %>%
  mutate(Method = recode(fmethod,
                         "arima" = "ARIMA",
                         "ets" = "ETS",
                         "rw" = "RW")) %>% 
  mutate(Horizon = recode(horizon,
                          "1" = "1y",
                          "2" = "2y",
                          "3" = "3y")) %>% 
  rename(Date = date,
         Accuracy = mean)

ggplot(data = tab, aes(x=Date, y=Accuracy, group = Method)) + 
  geom_line(aes(colour = Method)) +
  facet_grid(Horizon ~ level) +
  scale_colour_manual(values = c(bpy.colors(4)[-4])) +
  ylab("Forecast Accuracy (MASE)") +
  xlab("Forecasted Period") +
  scale_x_discrete(breaks = seq(1998, 2018, by = 4)) +
  theme_bw() +
  theme(legend.position="bottom")

ggsave("tex/fig/fig_eval_methods.pdf", device = "pdf",
       width = 18, height = 12, units = "cm")



# by geographical region
# 
# Category = recode(recon,
#                   "bu" = "Basic Methods",
#                   "mo_cat" = "Basic Methods",
#                   "mo_reg" = "Basic Methods",
#                   "tdgsa_cat" = "Basic Methods",
#                   "tdgsa_reg" = "Basic Methods",
#                   "tdgsf_reg" = "Basic Methods",
#                   "tdgsf_cat" = "Basic Methods",
#                   "tdfp_reg" = "Basic Methods",
#                   "tdfp_cat" = "Basic Methods",
#                   "ols" = "Optimal Combination",
#                   "wls" = "Optimal Combination",
#                   "nseries" = "Optimal Combination",
#                   "mint" = "Optimal Combination",
#                   "unrecon" = "Basic Methods")