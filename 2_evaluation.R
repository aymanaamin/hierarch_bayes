
# 0. PRELIMINARIES --------------------------------------------------------

rm(list = ls())

# Packages
library(hts)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(sp)

# Options
options(scipen=10)

# Data & Metadata
load("out/tabs.Rdata")

# Weights
load("dat/tradegts_reduced2.Rdata")
tsl <- as.list(aggts(tradegts_reduced2)/1e+9)
weights <- sapply(tsl, function(x) mean(x)/mean(tsl$Total))
names(weights)[grepl("/",names(weights))] <- sapply(strsplit(names(weights)[grepl("/",names(weights))],"/"),`[`,2)
rm(tsl)




# PREDICTABILITY FOR DIFFERENT REGIONS/CATEGORIES?
# CHECK ACCURACY FOR 2015 WHEN SETTING EURO AREA COUNTRIES LOWER





# MASE BY RECONCILIATION METHOD AND LEVEL ---------------------------------

# x = reconciliation methods, y = accuracy
tab <- tabs$MASE %>% 
  gather(key = level, value = accuracy, -c(date,recon,fmethod,horizon)) %>% 
  filter(fmethod == "arima" & recon != "bsr" & horizon == "1") %>% 
  group_by(recon,level) %>%
  summarise(accuracy = mean(accuracy)) %>% 
  mutate(reg_lvl = factor(case_when(
    level == "Total" ~ "World",
    nchar(level) == 2 & grepl("^[A-Za-z]+$", level) == F ~ "World",
    nchar(level) == 3 & grepl("^[A-Za-z]+$", level) == F ~ "World",
    nchar(level) == 2 & grepl("^[A-Za-z]+$", level) == T ~ "Region",
    nchar(level) %in% c(4,5) & grepl("^[A-Za-z]+$", level) == F ~ "Region",
    nchar(level) == 4 & grepl("^[A-Za-z]+$", level) == T ~ "Country",
    nchar(level) > 5 & grepl("^[A-Za-z]+$", level) == F ~ "Country"),
    levels = c("World","Region","Country"), ordered = T),
    cat_lvl = factor(case_when(
      level == "Total" ~ "Total",
      nchar(level) %in% c(2,4) & grepl("^[A-Za-z]+$", level) == T ~ "Total",
      nchar(level) %in% c(2) & grepl("^[A-Za-z]+$", level) == F ~ "Category",
      nchar(level) %in% c(4,6) & grepl("^[A-Za-z]+$", level) == F ~ "Category",
      nchar(level) %in% c(3,5,7) & grepl("^[A-Za-z]+$", level) == F ~ "Subcategory"),
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
                           "unrecon" = "Basic"))
tab$accuracy[which(tab$accuracy > 5)] <- 5

tab2 <- tab %>% 
  add_column(weights = sapply(tab$level, function(x) weights[which(names(weights) == x)])) %>% 
  mutate(acc_weighted = accuracy*weights) %>% 
  group_by(Reconciliation,reg_lvl,cat_lvl,Grouping) %>%
  summarise(Accuracy = sum(acc_weighted, na.rm=T))



ggplot() +
  geom_hline(data = filter(tab2, Reconciliation == "Unreconciled"),
             aes(yintercept=Accuracy), color = "darkgrey") +
  geom_bar(data = filter(tab2, Reconciliation != "Unreconciled"),
           aes(x=Grouping, y=Accuracy, fill = Reconciliation),
           colour="black", stat="identity", position = position_dodge()) +
  facet_grid(reg_lvl ~ cat_lvl, switch = "y") +
  scale_y_continuous(position = "right") +
  scale_fill_manual(values = c(bpy.colors(10))) +
  ylab("Forecast Error (MASE)") +
  xlab("Reconciliation Methods") +
  theme_bw() +
  coord_cartesian(ylim = c(1,1.6)) +
  theme(legend.position="bottom", legend.title = element_blank())  +
  guides(fill=guide_legend(nrow=3))

ggsave("tex/fig/fig_eval_mase.pdf", device = "pdf",
       width = 18, height = 12, units = "cm")



# RELATIVE RMSE BY RECONCILIATION METHOD AND LEVEL -------------------------

# x = reconciliation methods, y = accuracy
tab <- tabs$RMSE %>% 
  gather(key = level, value = accuracy, -c(date,recon,fmethod,horizon)) %>% 
  filter(fmethod == "arima" & recon != "bsr" & horizon == "1") %>% 
  mutate(accuracy = accuracy^2) %>% 
  group_by(recon,level) %>%
  summarise(accuracy = mean(accuracy)) %>% 
  ungroup

tab2 <- tab %>% 
  add_column(weights = sapply(tab$level, function(x) weights[which(names(weights) == x)])) %>% 
  mutate(acc_weighted = accuracy*weights)

tab3 <- tab2 %>% 
  mutate(reg_lvl = factor(case_when(
    level == "Total" ~ "World",
    nchar(level) == 2 & grepl("^[A-Za-z]+$", level) == F ~ "World",
    nchar(level) == 3 & grepl("^[A-Za-z]+$", level) == F ~ "World",
    nchar(level) == 2 & grepl("^[A-Za-z]+$", level) == T ~ "Region",
    nchar(level) %in% c(4,5) & grepl("^[A-Za-z]+$", level) == F ~ "Region",
    nchar(level) == 4 & grepl("^[A-Za-z]+$", level) == T ~ "Country",
    nchar(level) > 5 & grepl("^[A-Za-z]+$", level) == F ~ "Country"),
    levels = c("World","Region","Country"), ordered = T),
    cat_lvl = factor(case_when(
      level == "Total" ~ "Total",
      nchar(level) %in% c(2,4) & grepl("^[A-Za-z]+$", level) == T ~ "Total",
      nchar(level) %in% c(2) & grepl("^[A-Za-z]+$", level) == F ~ "Category",
      nchar(level) %in% c(4,6) & grepl("^[A-Za-z]+$", level) == F ~ "Category",
      nchar(level) %in% c(3,5,7) & grepl("^[A-Za-z]+$", level) == F ~ "Subcategory"),
      levels = c("Total","Category","Subcategory"), ordered = T)) %>% 
  group_by(reg_lvl,cat_lvl,recon) %>%
  summarise(accuracy = sum(acc_weighted)^0.5) %>% 
  ungroup

tab4 <- tab3 %>% 
  spread(key = recon, value = accuracy) %>% 
  mutate(`Bottom-Up` = 100*(unrecon/bu-1),
         `Middle-Out (Categories)` = 100*(unrecon/mo_cat-1),
         `Middle-Out (Regions)` = 100*(unrecon/mo_reg-1),
         `Top-Down (Categories)` = 100*(unrecon/tdfp_cat-1),
         `Top-Down (Regions)` = 100*(unrecon/tdfp_reg-1),
         `MinT` = 100*(unrecon/mint-1),
         `WLS` = 100*(unrecon/wls-1),
         `OLS` = 100*(unrecon/ols-1),
         `nSeries` = 100*(unrecon/nseries-1)) %>% 
  select(-c(unrecon,bu,mint,mo_cat,mo_reg,nseries,ols,tdfp_cat,tdfp_reg,wls)) %>% 
  gather(key = Reconciliation, value = Accuracy, -c(reg_lvl,cat_lvl)) %>% 
  mutate(Reconciliation = factor(Reconciliation,
                                 levels = c("Bottom-Up","Middle-Out (Categories)","Middle-Out (Regions)",
                                            "Top-Down (Categories)","Top-Down (Regions)",
                                            "MinT","WLS","OLS","nSeries"), ordered = T),
         Grouping = recode(Reconciliation,
                           "Bottom-Up" = "Basic",
                           "Middle-Out (Categories)" = "Basic",
                           "Middle-Out (Regions)" = "Basic",
                           "Top-Down (Categories)" = "Basic",
                           "Top-Down (Regions)" = "Basic",
                           "OLS" = "Optimal",
                           "WLS" = "Optimal",
                           "nSeries" = "Optimal",
                           "MinT" = "Optimal"))


ggplot(tab4, aes(x=Grouping, y=Accuracy, fill = Reconciliation)) +   
  geom_bar(colour="black", stat="identity", position = position_dodge()) +
  geom_hline(aes(yintercept=0), color = "darkgrey") +
  facet_grid(reg_lvl ~ cat_lvl, switch = "y") +
  scale_y_continuous(position = "right") +
  scale_fill_manual(values = c(bpy.colors(10))) +
  ylab("Relative Forecast Accuracy") +
  xlab("Reconciliation Methods") +
  # coord_cartesian(ylim = c(-10,10)) +
  theme_bw() +
  theme(legend.position="bottom", legend.title = element_blank())  +
  guides(fill=guide_legend(nrow=3))

ggsave("tex/fig/fig_eval_rmse_relative.pdf", device = "pdf",
       width = 18, height = 12, units = "cm")





# RELATIVE RMSE BY TIME AND LEVEL ---------------------------------------

# x = reconciliation methods, y = accuracy
tab <- tabs$RMSE %>% 
  gather(key = level, value = accuracy, -c(date,recon,fmethod,horizon)) %>% 
  filter(!(recon %in% c("bsr","ols","nseries"))) %>% 
  filter(fmethod == "arima") %>% 
  mutate(accuracy = accuracy^2) %>% 
  group_by(date,recon,level) %>%
  summarise(accuracy = mean(accuracy))

tab2 <- tab %>% 
  add_column(weights = sapply(tab$level, function(x) weights[which(names(weights) == x)])) %>% 
  mutate(acc_weighted = accuracy*weights)

tab3 <- tab2 %>% 
  mutate(reg_lvl = factor(case_when(
    level == "Total" ~ "World",
    nchar(level) == 2 & grepl("^[A-Za-z]+$", level) == F ~ "World",
    nchar(level) == 3 & grepl("^[A-Za-z]+$", level) == F ~ "World",
    nchar(level) == 2 & grepl("^[A-Za-z]+$", level) == T ~ "Region",
    nchar(level) %in% c(4,5) & grepl("^[A-Za-z]+$", level) == F ~ "Region",
    nchar(level) == 4 & grepl("^[A-Za-z]+$", level) == T ~ "Country",
    nchar(level) > 5 & grepl("^[A-Za-z]+$", level) == F ~ "Country"),
    levels = c("World","Region","Country"), ordered = T),
    cat_lvl = factor(case_when(
      level == "Total" ~ "Total",
      nchar(level) %in% c(2,4) & grepl("^[A-Za-z]+$", level) == T ~ "Total",
      nchar(level) %in% c(2) & grepl("^[A-Za-z]+$", level) == F ~ "Category",
      nchar(level) %in% c(4,6) & grepl("^[A-Za-z]+$", level) == F ~ "Category",
      nchar(level) %in% c(3,5,7) & grepl("^[A-Za-z]+$", level) == F ~ "Subcategory"),
      levels = c("Total","Category","Subcategory"), ordered = T)) %>% 
  group_by(reg_lvl,cat_lvl,recon,date) %>%
  summarise(accuracy = sum(acc_weighted)^0.5)

tab4 <- tab3 %>% 
  spread(key = recon, value = accuracy) %>% 
  mutate(`Bottom-Up` = 100*(unrecon/bu-1),
         `Middle-Out (Categories)` = 100*(unrecon/mo_cat-1),
         `Middle-Out (Regions)` = 100*(unrecon/mo_reg-1),
         `Top-Down (Categories)` = 100*(unrecon/tdfp_cat-1),
         `Top-Down (Regions)` = 100*(unrecon/tdfp_reg-1),
         `MinT` = 100*(unrecon/mint-1),
         `WLS` = 100*(unrecon/wls-1)) %>% 
  select(-c(unrecon,bu,mint,mo_cat,mo_reg,tdfp_cat,tdfp_reg,wls)) %>% 
  gather(key = Reconciliation, value = Accuracy, -c(date,reg_lvl,cat_lvl)) %>% 
  mutate(Reconciliation = factor(Reconciliation,
                                 levels = c("Bottom-Up","Middle-Out (Categories)","Middle-Out (Regions)",
                                            "Top-Down (Categories)","Top-Down (Regions)",
                                            "MinT","WLS","OLS","nSeries"), ordered = T))

ggplot(tab4, aes(x=date, y=Accuracy, group = Reconciliation)) +
  geom_hline(aes(yintercept=0), color = "darkgrey") +
  geom_line(aes(color= Reconciliation, linetype = Reconciliation)) +
  facet_grid(reg_lvl ~ cat_lvl, switch = "y") +
  scale_linetype_manual(values=c(4,2,2,3,3,1,1,1)) +
  scale_y_continuous(position = "right") +
  scale_x_continuous(breaks = seq(1998, 2018, by = 4), labels = c("98","02","06","10","14","18")) +
  scale_color_manual(values = c(bpy.colors(9)[-c(8,9)])) +
  ylab("Relative Forecast Accuracy") +
  xlab("Reconciliation Methods") +
  theme_bw() +
  theme(legend.position="bottom", legend.title = element_blank())  +
  guides(fill=guide_legend(nrow=3))

ggsave("tex/fig/fig_eval_rmse_time.pdf", device = "pdf",
       width = 18, height = 12, units = "cm")



# FORECASTING AND RECONCILIATION METHODS BY LEVEL ---------------------------------------------

# Top
tab <- rbind(tabs$MASE %>% 
               gather(key = level, value = accuracy, -c(date,recon,fmethod,horizon)) %>% 
               filter(level == "Total" & recon == "unrecon") %>% 
               group_by(date,fmethod,horizon,level) %>%
               summarise(accuracy = mean(accuracy,na.rm=T)) %>% 
               add_column(measure = factor("MASE", levels = c("RMSE","MAPE","MASE"))),
             tabs$RMSE %>% 
               gather(key = level, value = accuracy, -c(date,recon,fmethod,horizon)) %>% 
               filter(level == "Total" & recon == "unrecon") %>% 
               group_by(date,fmethod,horizon,level) %>%
               summarise(accuracy = mean(accuracy,na.rm=T)/1e+9) %>% 
               add_column(measure = factor("RMSE", levels = c("RMSE","MAPE","MASE"))),
             tabs$MAPE %>% 
               gather(key = level, value = accuracy, -c(date,recon,fmethod,horizon)) %>% 
               filter(level == "Total" & recon == "unrecon") %>% 
               group_by(date,fmethod,horizon,level) %>%
               summarise(accuracy = mean(accuracy,na.rm=T)) %>% 
               add_column(measure = factor("MAPE", levels = c("RMSE","MAPE","MASE"))))

tab2 <- tab %>%
  mutate(Method = recode(fmethod,
                         "arima" = "ARIMA",
                         "ets" = "ETS",
                         "rw" = "RW")) %>% 
  mutate(horizon2 = recode(horizon,
                           "1" = "1y",
                           "2" = "2y",
                           "3" = "3y"))

ggplot(tab2, aes(x=date, y=accuracy, group = Method)) + 
  geom_line(aes(colour = Method)) +
  facet_grid(measure ~ horizon2, scales = "free_y") +
  scale_colour_manual(values = c(bpy.colors(4)[-4])) +
  ylab("Forecast Accuracy") +
  xlab("Forecasted Period") +
  scale_x_discrete(breaks = seq(1998, 2018, by = 4)) +
  theme_bw() +
  theme(legend.position="bottom")

ggsave("tex/fig/fig_eval_methods_top.pdf", device = "pdf",
       width = 18, height = 12, units = "cm")


# Bottom
tab <- rbind(tabs$MASE %>% 
               gather(key = level, value = accuracy, -c(date,recon,fmethod,horizon)) %>% 
               filter(nchar(level) == 7 & recon == "unrecon") %>% 
               group_by(date,fmethod,horizon,level) %>%
               summarise(accuracy = mean(accuracy,na.rm=T)) %>% 
               add_column(measure = factor("MASE", levels = c("RMSE","MAPE","MASE"))),
             tabs$RMSE %>% 
               gather(key = level, value = accuracy, -c(date,recon,fmethod,horizon)) %>% 
               filter(nchar(level) == 7 & recon == "unrecon") %>% 
               group_by(date,fmethod,horizon,level) %>%
               summarise(accuracy = mean(accuracy^2,na.rm=T)) %>% 
               add_column(measure = factor("RMSE", levels = c("RMSE","MAPE","MASE"))),
             tabs$MAPE %>% 
               gather(key = level, value = accuracy, -c(date,recon,fmethod,horizon)) %>% 
               filter(nchar(level) == 7 & recon == "unrecon") %>% 
               group_by(date,fmethod,horizon,level) %>%
               summarise(accuracy = mean(accuracy,na.rm=T)) %>% 
               add_column(measure = factor("MAPE", levels = c("RMSE","MAPE","MASE"))))

tab2 <- tab %>% spread(key = measure, value = accuracy) %>% 
  mutate(RMSE = replace(RMSE, !is.finite(RMSE), NA),
         MAPE = replace(MAPE, !is.finite(MAPE) | MAPE > 100, NA),
         MASE = replace(MASE, !is.finite(MASE), NA))
tab3 <- tab2 %>%  
  add_column(weights = sapply(tab2$level, function(x) weights[which(names(weights) == x)])) %>% 
  mutate(rmse_weighted = RMSE*weights,
         mape_weighted = MAPE*weights,
         mase_weighted = MASE*weights) %>% 
  group_by(date,fmethod,horizon) %>%
  summarise(RMSE = (sum(rmse_weighted,na.rm=T)^0.5)/1e+9,
            MAPE = sum(mape_weighted,na.rm=T),
            MASE = sum(mase_weighted,na.rm=T)) %>% 
  ungroup

tab4 <- tab3 %>% 
  gather(key = measure, value = accuracy, -c(date,fmethod,horizon)) %>%
  mutate(Method = recode(fmethod,
                         "arima" = "ARIMA",
                         "ets" = "ETS",
                         "rw" = "RW")) %>% 
  mutate(horizon2 = recode(horizon,
                           "1" = "1y",
                           "2" = "2y",
                           "3" = "3y")) %>% 
  mutate(measure = factor(measure,
                          levels = c("RMSE","MAPE","MASE"), ordered = T))


ggplot(tab4, aes(x=date, y=accuracy, group = Method)) + 
  geom_line(aes(colour = Method)) +
  facet_grid(measure ~ horizon2, scales = "free_y") +
  scale_colour_manual(values = c(bpy.colors(4)[-4])) +
  ylab("Forecast Accuracy") +
  xlab("Forecasted Period") +
  scale_x_discrete(breaks = seq(1998, 2018, by = 4)) +
  theme_bw() +
  theme(legend.position="bottom")

ggsave("tex/fig/fig_eval_methods_bottom.pdf", device = "pdf",
       width = 18, height = 12, units = "cm")

