
# 0. PRELIMINARIES --------------------------------------------------------

rm(list = ls())

# Packages
library(hts)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(sp)
library(scales)

# Options
options(scipen=10)

# Data & Metadata
load("out/tabs.Rdata")

# Weights & Volumes
load("dat/tradegts_reduced2.Rdata")
tsl <- as.list(aggts(tradegts_reduced2)/1e+9)
weights <- sapply(tsl, function(x) mean(window(x, start = 1998))/mean(window(tsl$Total, start = 1998)))
names(weights)[grepl("/",names(weights))] <- sapply(strsplit(names(weights)[grepl("/",names(weights))],"/"),`[`,2)
rm(tsl)





# MASE BY RECONCILIATION METHOD AND LEVEL ---------------------------------

# x = reconciliation methods, y = accuracy
tab <- tabs$MASE %>% 
  gather(key = level, value = accuracy, -c(date,recon,fmethod)) %>% 
  filter(fmethod == "arima") %>% 
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
                                            "mint","wls","ols","nseries","bsr"),
                                 labels = c(" Unreconciled "," Bottom-Up "," Middle-Out (Categories) ", 
                                            " Middle-Out (Regions) ", " Top-Down (Categories)  ",
                                            " Top-Down (Regions) ", " MinT    "," WLS    ",
                                            " OLS    "," nseries"," BSR    "),
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
                           "bsr" = "Optimal",
                           "unrecon" = "Basic"))
tab$accuracy[which(tab$accuracy > 5)] <- 5

tab2 <- tab %>% 
  add_column(weights = sapply(tab$level, function(x) weights[which(names(weights) == x)])) %>% 
  mutate(acc_weighted = accuracy*weights) %>% 
  group_by(Reconciliation,reg_lvl,cat_lvl,Grouping) %>%
  summarise(Accuracy = sum(acc_weighted, na.rm=T))



ggplot() +
  geom_hline(data = filter(tab2, Reconciliation == " Unreconciled "),
             aes(yintercept=Accuracy), color = "darkgrey") +
  geom_bar(data = filter(tab2, Reconciliation != " Unreconciled "),
           aes(x=Grouping, y=Accuracy, fill = Reconciliation),
           colour="black", lwd = 0.25, stat="identity", position = position_dodge()) +
  facet_grid(reg_lvl ~ cat_lvl, switch = "y") +
  scale_y_continuous(position = "right", breaks = seq(1,2.5,0.5)) +
  scale_fill_manual(values = c(bpy.colors(11)[-11])) +
  ylab("Forecast Error (MASE)") +
  xlab("Reconciliation Methods") +
  theme_bw() +
  coord_cartesian(ylim = c(1,2.5)) +
  theme(legend.position="bottom", legend.title = element_blank())  +
  guides(fill=guide_legend(nrow=2))

ggsave("tex/fig/fig_eval_mase.pdf", device = "pdf",
       width = 18, height = 12, units = "cm")







# MAPE BY RECONCILIATION METHOD AND LEVEL ---------------------------------

# x = reconciliation methods, y = accuracy
tab <- tabs$MAPE %>% 
  gather(key = level, value = accuracy, -c(date,recon,fmethod)) %>% 
  filter(fmethod == "arima") %>% 
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
                                            "mint","wls","ols","nseries","bsr"),
                                 labels = c(" Unreconciled "," Bottom-Up "," Middle-Out (Categories) ", 
                                            " Middle-Out (Regions) ", " Top-Down (Categories)  ",
                                            " Top-Down (Regions) ", " MinT    "," WLS    ",
                                            " OLS    "," nseries"," BSR    "),
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
                           "bsr" = "Optimal",
                           "unrecon" = "Basic"))
tab$accuracy[which(tab$accuracy > 500)] <- NA

tab2 <- tab %>% 
  add_column(weights = sapply(tab$level, function(x) weights[which(names(weights) == x)])) %>% 
  mutate(acc_weighted = accuracy*weights) %>% 
  group_by(Reconciliation,reg_lvl,cat_lvl,Grouping) %>%
  summarise(Accuracy = sum(acc_weighted, na.rm=T))



ggplot() +
  geom_hline(data = filter(tab2, Reconciliation == " Unreconciled "),
             aes(yintercept=Accuracy), color = "darkgrey") +
  geom_bar(data = filter(tab2, Reconciliation != " Unreconciled "),
           aes(x=Grouping, y=Accuracy, fill = Reconciliation),
           colour="black", lwd = 0.25, stat="identity", position = position_dodge()) +
  facet_grid(reg_lvl ~ cat_lvl, switch = "y") +
  # scale_y_continuous(position = "right", breaks = seq(0,100)) +
  scale_fill_manual(values = c(bpy.colors(11)[-11])) +
  ylab("Forecast Error (MAPE)") +
  xlab("Reconciliation Methods") +
  theme_bw() +
  # coord_cartesian(ylim = c(1,2.5)) +
  theme(legend.position="bottom", legend.title = element_blank())  +
  guides(fill=guide_legend(nrow=2))

ggsave("tex/fig/fig_eval_mape.pdf", device = "pdf",
       width = 18, height = 12, units = "cm")





# RELATIVE RMSE BY RECONCILIATION METHOD AND LEVEL -------------------------

# x = reconciliation methods, y = accuracy
tab <- tabs$RMSE %>% 
  filter(fmethod == "arima") %>% 
  gather(key = level, value = accuracy, -c(date,recon,fmethod)) %>% 
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
  summarise(accuracy = sum(acc_weighted)) %>% 
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
         `BSR` = 100*(unrecon/bsr-1),
         `nseries` = 100*(unrecon/nseries-1)) %>% 
  select(-c(unrecon,bu,mint,mo_cat,mo_reg,ols,tdfp_cat,tdfp_reg,wls,bsr)) %>% 
  gather(key = Reconciliation, value = Accuracy, -c(reg_lvl,cat_lvl)) %>% 
  mutate(Grouping = recode(Reconciliation,
                           "Bottom-Up" = 1,
                           "Middle-Out (Categories)" = 1,
                           "Middle-Out (Regions)" = 1,
                           "Top-Down (Categories)" = 1,
                           "Top-Down (Regions)" = 1,
                           "OLS" = 2,
                           "WLS" = 2,
                           "BSR" = 2,
                           "nseries" = 2,
                           "MinT" = 2),
         Reconciliation = factor(Reconciliation,
                                 levels = c("Bottom-Up","Middle-Out (Categories)","Middle-Out (Regions)",
                                            "Top-Down (Categories)","Top-Down (Regions)",
                                            "MinT","WLS","OLS","nseries","BSR"), 
                                 labels = c(" Bottom-Up"," Middle-Out (Categories) "," Middle-Out (Regions) ",
                                            " Top-Down (Categories) "," Top-Down (Regions) ",
                                            " MinT    "," WLS    "," OLS    "," nseries"," BSR    "),
                                 ordered = T))

ggplot(tab4, aes(x=Grouping, y=Accuracy, fill = Reconciliation)) +   
  geom_ribbon(ymin = (qf(0.025,12*12*21,12*12*21)-1)*100,
              ymax = (qf(0.975,12*12*21,12*12*21)-1)*100,
              x = seq(0,5,length.out = 90), alpha=0.5, fill = "grey90", col = "grey60") +
  geom_hline(aes(yintercept=0), color = "black", lwd = 0.25) +
  facet_grid(reg_lvl ~ cat_lvl, switch = "y") +
  geom_bar(colour="black", stat="identity", lwd = 0.25, position = position_dodge()) +
  scale_y_continuous(position = "right", breaks = seq(-30,30,15),
                     limits = c(-40,40), minor_breaks = NULL) +
  scale_x_continuous(breaks = c(1,2),labels = c("Single\nLevel","Optimal\nCombination")) +
  scale_fill_manual(values = c(bpy.colors(11)[-11])) +
  ylab("Relative MSE") +
  xlab("Reconciliation Methods") +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank())  +
  guides(fill=guide_legend(nrow=2))


ggsave("tex/fig/fig_eval_rmse_relative.pdf", device = "pdf",
       width = 18, height = 12, units = "cm")


# ggplot(tab4, aes(x=Grouping, y=Accuracy, fill = Reconciliation)) +   
#   geom_ribbon(ymin = (qf(0.025,12*12*21,12*12*21)-1)*100,
#               ymax = (qf(0.975,12*12*21,12*12*21)-1)*100,
#               x = seq(0,5,length.out = 90), alpha=0.5, fill = "grey80", col = "grey60") +
#   geom_hline(aes(yintercept=0), color = "black", lwd = 0.25) +
#   facet_grid(reg_lvl ~ cat_lvl, switch = "y") +
#   geom_bar(colour="black", stat="identity", lwd = 0.25, position = position_dodge()) +
#   scale_y_continuous(position = "right", breaks = seq(-30,30,15),
#                      limits = c(-40,40), minor_breaks = NULL) +
#   scale_x_continuous(breaks = c(1,2),labels = c("Basic","Optimal")) +
#   scale_fill_manual(values = c(bpy.colors(11)[-11])) +
#   ylab("Relative MSE") +
#   xlab("Reconciliation Methods") +
#   theme_bw() +
#   theme(legend.position="right",
#         legend.title = element_blank(),
#         panel.grid.minor.x = element_blank()) +
#   guides(fill=guide_legend(ncol=1))
# 
# 
# ggsave("tex/fig/fig_eval_rmse_relative_p.pdf", device = "pdf",
#        width = 22, height = 7, units = "cm")






# RELATIVE RMSE BY TIME AND LEVEL ---------------------------------------

# x = reconciliation methods, y = accuracy
tab <- tabs$RMSE %>% 
  gather(key = level, value = accuracy, -c(date,recon,fmethod)) %>% 
  filter(!(recon %in% c("ols","nseries","bu","mo_cat","mo_reg","tdfp_reg","tdfp_cat"))) %>% 
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
  summarise(accuracy = sum(acc_weighted)) %>% 
  ungroup

tab4 <- tab3 %>% 
  spread(key = recon, value = accuracy) %>% 
  mutate(`MinT` = 100*(unrecon/mint-1),
         `WLS` = 100*(unrecon/wls-1),
         `BSR` = 100*(unrecon/bsr-1)) %>% 
  select(-c(unrecon,mint,wls,bsr)) %>% 
  gather(key = Reconciliation, value = Accuracy, -c(date,reg_lvl,cat_lvl)) %>% 
  mutate(Reconciliation = factor(Reconciliation,
                                 levels = c("MinT","WLS","BSR"), ordered = T))

ggplot(tab4, aes(x=as.numeric(date), y=Accuracy, group = Reconciliation, size = Reconciliation)) +
  geom_ribbon(ymin = (qf(0.025,12*12,12*12)-1)*100,
              ymax = (qf(0.975,12*12,12*12)-1)*100,
              alpha=0.25, fill = "grey80", col = "grey60", show.legend = F) +
  geom_hline(yintercept=0, color = "black", lwd = 0.25) +
  facet_grid(reg_lvl ~ cat_lvl, switch = "y") +
  geom_line(aes(color= Reconciliation, linetype = Reconciliation, size = Reconciliation)) +
  scale_linetype_manual(values=c(1,1,1)) +
  scale_size_manual(values = c(0.5,0.5,0.5)) +
  scale_y_continuous(position = "right", breaks = seq(0,600,200), minor_breaks = NULL) +
  scale_x_continuous(breaks = seq(2000, 2015, by = 5),
                     labels = c("2000","2005","2010","2015"),
                     expand = c(0,0)) +
  scale_color_manual(values = bpy.colors(11)[c(6,7,10)]) +
  ylab("Relative MSE") +
  xlab("Time") +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank())  +
  guides(fill=guide_legend(nrow=3))

ggsave("tex/fig/fig_eval_rmse_time.pdf", device = "pdf",
       width = 18, height = 12, units = "cm")


# ggplot(tab4, aes(x=as.numeric(date), y=Accuracy, group = Reconciliation, size = Reconciliation)) +
#   geom_ribbon(ymin = (qf(0.025,12*12,12*12)-1)*100,
#               ymax = (qf(0.975,12*12,12*12)-1)*100,
#               alpha=0.25, fill = "grey80", col = "grey60", show.legend = F) +
#   geom_hline(yintercept=0, color = "black", lwd = 0.5) +
#   facet_grid(reg_lvl ~ cat_lvl, switch = "y") +
#   geom_line(aes(color= Reconciliation, linetype = Reconciliation, size = Reconciliation)) +
#   scale_linetype_manual(values=c(1,1,1)) +
#   scale_size_manual(values = c(0.5,0.5,0.5)) +
#   scale_y_continuous(position = "right", breaks = seq(0,600,200), minor_breaks = NULL) +
#   scale_x_continuous(breaks = seq(2000, 2015, by = 5),
#                      labels = c("2000","2005","2010","2015"),
#                      expand = c(0,0)) +
#   scale_color_manual(values = bpy.colors(11)[c(6,7,10)]) +
#   ylab("Relative MSE") +
#   xlab("Time") +
#   theme_bw() +
#   theme(legend.position="right",
#         legend.title = element_blank(),
#         panel.grid.minor.x = element_blank())  +
#   guides(fill=guide_legend(nrow=3))
# 
# ggsave("tex/fig/fig_eval_rmse_time_p.pdf", device = "pdf",
#        width = 22, height = 7, units = "cm")



# FORECASTING AND RECONCILIATION METHODS BY LEVEL ---------------------------------------------

# Top
tab <- rbind(tabs$MASE %>% 
               select(c(date,recon,fmethod,Total)) %>% 
               gather(key = level, value = accuracy, -c(date,recon,fmethod)) %>% 
               filter(recon == "unrecon") %>% 
               group_by(date,fmethod,level) %>%
               summarise(accuracy = mean(accuracy,na.rm=T)) %>% 
               add_column(measure = factor("MASE", levels = c("RMSE","MAPE","MASE"))),
             tabs$RMSE %>% 
               select(c(date,recon,fmethod,Total)) %>% 
               gather(key = level, value = accuracy, -c(date,recon,fmethod)) %>% 
               filter( recon == "unrecon") %>% 
               group_by(date,fmethod,level) %>%
               summarise(accuracy = mean(accuracy,na.rm=T)) %>% 
               add_column(measure = factor("RMSE", levels = c("RMSE","MAPE","MASE"))),
             tabs$MAPE %>% 
               select(c(date,recon,fmethod,Total)) %>% 
               gather(key = level, value = accuracy, -c(date,recon,fmethod)) %>% 
               filter(recon == "unrecon") %>% 
               group_by(date,fmethod,level) %>%
               summarise(accuracy = mean(accuracy,na.rm=T)) %>% 
               add_column(measure = factor("MAPE", levels = c("RMSE","MAPE","MASE")))) %>% 
  ungroup

tab_top <- tab %>%
  mutate(Method = recode(fmethod,
                         "arima" = "ARIMA",
                         "ets" = "ETS",
                         "rw" = "RW")) %>% 
  select(c(date,accuracy,Method,measure)) %>% 
  add_column(level = factor("Top Level", levels = c("Top Level","Bottom Level")))


# Bottom
tab <- rbind(tabs$MASE %>% 
               select(c(date,recon,fmethod,which(nchar(colnames(tabs$MASE)) == 7))) %>% 
               gather(key = level, value = accuracy, -c(date,recon,fmethod)) %>% 
               filter(recon == "unrecon") %>% 
               group_by(date,fmethod,level) %>%
               summarise(accuracy = mean(accuracy,na.rm=T)) %>% 
               add_column(measure = factor("MASE", levels = c("RMSE","MAPE","MASE"))),
             tabs$RMSE %>%
               select(c(date,recon,fmethod,which(nchar(colnames(tabs$RMSE)) == 7))) %>% 
               gather(key = level, value = accuracy, -c(date,recon,fmethod)) %>% 
               filter(recon == "unrecon") %>% 
               group_by(date,fmethod,level) %>%
               summarise(accuracy = mean(accuracy^2,na.rm=T)) %>% 
               add_column(measure = factor("RMSE", levels = c("RMSE","MAPE","MASE"))),
             tabs$MAPE %>% 
               select(c(date,recon,fmethod,which(nchar(colnames(tabs$MAPE)) == 7))) %>% 
               gather(key = level, value = accuracy, -c(date,recon,fmethod)) %>% 
               filter(recon == "unrecon") %>% 
               group_by(date,fmethod,level) %>%
               summarise(accuracy = mean(accuracy,na.rm=T)) %>% 
               add_column(measure = factor("MAPE", levels = c("RMSE","MAPE","MASE"))))

tab2 <- tab %>% spread(key = measure, value = accuracy) %>% 
  mutate(RMSE = replace(RMSE, !is.finite(RMSE), NA),
         MAPE = replace(MAPE, !is.finite(MAPE) | MAPE > 30, NA),
         MASE = replace(MASE, !is.finite(MASE), NA))
tab3 <- tab2 %>%  
  add_column(weights2 = sapply(tab2$level, function(x) weights[which(names(weights) == x)])) %>% 
  mutate(rmse_weighted = RMSE*weights2,
         mape_weighted = MAPE*weights2,
         mase_weighted = MASE*weights2) %>% 
  group_by(date,fmethod) %>%
  summarise(RMSE = sum(rmse_weighted,na.rm=T)^0.5,
            MAPE = sum(mape_weighted,na.rm=T),
            MASE = sum(mase_weighted,na.rm=T)) %>% 
  ungroup

tab_bottom <- tab3 %>% 
  gather(key = measure, value = accuracy, -c(date,fmethod)) %>%
  mutate(Method = recode(fmethod,
                         "arima" = "ARIMA",
                         "ets" = "ETS",
                         "rw" = "RW")) %>% 
  mutate(measure = factor(measure,
                          levels = c("RMSE","MAPE","MASE"), ordered = T)) %>% 
  select(c(date,accuracy,Method,measure)) %>% 
  add_column(level = factor("Bottom Level", levels = c("Top Level","Bottom Level")))



ggplot(tab_bottom, aes(x=as.numeric(date), y=accuracy, group = Method, size = Method)) + 
  geom_line(aes(colour = Method)) +
  facet_grid(measure ~ ., scales = "free_y") +
  scale_colour_manual(values = c(bpy.colors(5)[-c(1,5)])) +
  scale_size_manual(values = c(0.5,0.5,0.5)) +
  ylab("Forecast Accuracy") +
  xlab("Forecasted Period") +
  scale_x_continuous(breaks = seq(2000, 2015, by = 5),
                     labels = c("2000","2005","2010","2015"),
                     expand = c(0,0)) +
  scale_y_continuous(labels = scientific) +
  theme_bw() +
  theme(legend.position="bottom",
        panel.grid.minor.x = element_blank())


ggsave("tex/fig/fig_eval_methods_bottom.pdf", device = "pdf",
       width = 18, height = 10, units = "cm")


ggplot(tab_top, aes(x=as.numeric(date), y=accuracy, group = Method, size = Method)) + 
  geom_line(aes(colour = Method)) +
  facet_grid(measure ~ ., scales = "free_y") +
  scale_colour_manual(values = c(bpy.colors(5)[-c(1,5)])) +
  scale_size_manual(values = c(0.5,0.5,0.5)) +
  ylab("Forecast Accuracy") +
  xlab("Forecasted Period") +
  scale_x_continuous(breaks = seq(1998, 2018, by = 4), labels = seq(1998, 2018, by = 4)) +
  scale_y_continuous(labels = scientific) +
  theme_bw() +
  theme(legend.position="bottom",
        panel.grid.minor.x = element_blank())

ggsave("tex/fig/fig_eval_methods_top.pdf", device = "pdf",
       width = 18, height = 10, units = "cm")




# SCATTERPLOTS BY REGION AND CATEGORY -------------------------------------

# Region
tab1 <- tabs$RMSE %>%
  filter(recon %in% c("bsr","unrecon") & fmethod == "arima") %>% 
  gather(key = level, value = accuracy, -c(date,recon,fmethod)) %>% 
  filter((nchar(level) == 4 & grepl("^[A-Za-z]+$", level) == T) | 
           (nchar(level) == 2 & grepl("^[A-Za-z]+$", level) == T)) %>% 
  group_by(recon,level) %>%
  summarise(accuracy = mean(accuracy^2,na.rm=T)) %>% 
  ungroup %>% 
  spread(key = recon, value = accuracy) %>% 
  mutate(relacc = 100*(unrecon/bsr-1)) %>% 
  select(-c(unrecon,bsr))

tab2 <- tab1 %>% 
  mutate(Regions = factor(case_when(
    substr(level,1,2) == "AF" ~ "Africa\nand\nMiddle\nEast",
    substr(level,1,2) == "AO" ~ "Australia\nand\nOceania",
    substr(level,1,2) == "EA" ~ "East\nAsia",
    substr(level,1,2) == "EU" ~ "Europe",
    substr(level,1,2) == "CA" ~ "Central\nAsia",
    substr(level,1,2) == "LA" ~ "Latin\nAmerica",
    substr(level,1,2) == "NA" ~ "North\nAmerica",
    substr(level,1,2) == "SA" ~ "South\nAsia"),
    levels = c("Europe","North\nAmerica","East\nAsia",
               "Africa\nand\nMiddle\nEast","Latin\nAmerica",
               "Central\nAsia","South\nAsia","Australia\nand\nOceania"), ordered = T)) %>%
  mutate(Level = factor(case_when(
    nchar(level) == 2 ~ "Region", 
    nchar(level) == 4 ~ "Country"),
    levels = c("Region","Country"), ordered = T)) %>%
  add_column(Share = sapply(tab1$level, function(x) weights[which(names(weights) == x)])*100)

ggplot(tab2, aes(Regions, relacc)) +
  geom_hline(aes(yintercept=0), color = "darkgrey") +
  geom_jitter(aes(color = Regions, size = Share, shape = Level), width = 0.1, height = 0) +
  coord_cartesian(ylim = c(-10,40)) +
  scale_shape_manual(name = "Level", values = c(1,20)) +
  scale_size_continuous(name = "Export Shares (in %)",
                        breaks = c(1,10,20), range = c(1,10)) +
  scale_colour_manual(values = c(bpy.colors(13)[-13]),guide=F) +
  scale_y_continuous(breaks = seq(-20,40,20)) + 
  ylab("Relative MSE") +
  xlab(NULL) +
  theme_bw() +
  guides(shape = guide_legend(order = 1),  size = guide_legend(order = 2)) +
  theme(legend.position="bottom")

ggsave("tex/fig/fig_eval_regions.pdf", device = "pdf",
       width = 18, height = 10, units = "cm")  


# Category
tab1 <- tabs$RMSE %>% 
  filter(recon %in% c("bsr","unrecon") & fmethod == "arima") %>% 
  gather(key = level, value = accuracy, -c(date,recon,fmethod)) %>% 
  filter((nchar(level) == 3) | (nchar(level) == 2 & grepl("^[A-Za-z]+$", level) == F)) %>% 
  group_by(recon,level) %>%
  summarise(accuracy = mean(accuracy^2,na.rm=T)) %>% 
  ungroup %>% 
  spread(key = recon, value = accuracy) %>% 
  mutate(relacc = 100*(unrecon/bsr-1)) %>% 
  select(-c(unrecon,bsr))

tab2 <- tab1 %>% 
  mutate(Categories = factor(case_when(
    substr(level,1,2) == "01" ~ "Agricult.\nProducts",
    substr(level,1,2) == "02" ~ "Energy\nSource",
    substr(level,1,2) == "03" ~ "Textiles",
    substr(level,1,2) == "04" ~ "Graphical\nProducts",
    substr(level,1,2) == "05" ~ "Leather,\nRubber,\nPlastics",
    substr(level,1,2) == "06" ~ "Chem.\nand\nPharma.",
    substr(level,1,2) == "07" ~ "Stones\nand\nEarth",
    substr(level,1,2) == "08" ~ "Metals",
    substr(level,1,2) == "09" ~ "Machines\nand\nElectronics",
    substr(level,1,2) == "10" ~ "Vehicles",
    substr(level,1,2) == "11" ~ "Prec.\nInstr.",
    substr(level,1,2) == "12" ~ "Various\nGoods"),
    levels = c("Chem.\nand\nPharma.","Prec.\nInstr.",
               "Machines\nand\nElectronics","Metals","Agricult.\nProducts",
               "Vehicles","Textiles","Leather,\nRubber,\nPlastics",
               "Energy\nSource","Graphical\nProducts",
               "Various\nGoods","Stones\nand\nEarth"), ordered = T)) %>%
  mutate(Level = factor(case_when(
    nchar(level) == 2 ~ "Category",
    nchar(level) == 3 ~ "Subcategory"),
    levels = c("Category","Subcategory"), ordered = T)) %>%
  add_column(Share = sapply(tab1$level, function(x) weights[which(names(weights) == x)])*100)

ggplot(tab2, aes(Categories, relacc)) +
  geom_hline(aes(yintercept=0), color = "darkgrey") +
  geom_jitter(aes(color = Categories, size = Share, shape = Level), width = 0.1, height = 0) +
  coord_cartesian(ylim = c(-20,60)) +
  scale_shape_manual(name = "Level", values = c(1,20)) +
  scale_size_continuous(name = "Export Shares (in %)",
                        breaks = c(1,10,20), range = c(1,10), limits = c(0,40)) +
  scale_colour_manual(values = c(bpy.colors(13)[-13]),guide=F) +
  scale_y_continuous(breaks = seq(-20,60,20)) +
  ylab("Relative MSE") +
  xlab(NULL) +
  theme_bw() +
  guides(shape = guide_legend(order = 1),  size = guide_legend(order = 2)) +
  theme(legend.position="bottom")

ggsave("tex/fig/fig_eval_categories.pdf", device = "pdf",
       width = 18, height = 10, units = "cm")  





# DIEBOLD MARIANO ---------------------------------------------------------

load("out/dm.Rdata")

tab1 <- as_tibble(do.call(cbind,dm)) %>% 
  add_column(h = 1:36) %>% 
  gather(method,pval, -h) %>% 
  mutate(Reconciliation = factor(method,
                                 levels = c("bu","mo_cat","mo_reg","tdfp_cat","tdfp_reg",
                                            "mint","wls","ols","nseries", "bsr"),
                                 labels = c(" Bottom-Up "," Middle-Out (Categories) ", 
                                            " Middle-Out (Regions) ", " Top-Down (Categories)  ",
                                            " Top-Down (Regions) ", " MinT    "," WLS    ",
                                            " OLS    "," nseries", " BSR    "),
                                 ordered = T),
         Grouping = factor(recode(method,
                           "bu" = "Single Level",
                           "mo_cat" = "Single Level",
                           "mo_reg" = "Single Level",
                           "tdfp_reg" = "Single Level",
                           "tdfp_cat" = "Single Level",
                           "ols" = "Optimal Combination",
                           "wls" = "Optimal Combination",
                           "nseries" = "Optimal Combination",
                           "mint" = "Optimal Combination",
                           "bsr" = "Optimal Combination"), levels = c("Single Level","Optimal Combination")))


ggplot(tab1, aes(x = h, y = pval, group = Reconciliation, color = Reconciliation)) +
  geom_point(size = 1) +
  geom_smooth(lwd = 0.5, se = F) +
  facet_grid(. ~ Grouping) +
  theme_bw() +
  coord_cartesian(ylim = c(0,1), expand = F) +
  scale_x_continuous(breaks = c(10,20,30)) + 
  scale_y_continuous(breaks = seq(0,1,0.2)) + 
  scale_color_manual(values = c(bpy.colors(11)[-11])) +
  ylab("P-Values of Diebold-Mariano Test") +
  xlab("Forecast Horizon (in Months)") +
  theme(legend.position="bottom", legend.title = element_blank())  +
  guides(fill=guide_legend(nrow=2))

ggsave("tex/fig/fig_dm.pdf", device = "pdf",
       width = 18, height = 10, units = "cm")

ggsave("tex/fig/fig_dm_p.pdf", device = "pdf",
       width = 18, height = 12, units = "cm")


