
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








# WEIGHTED RELATIVE RMSE BY RECONCILIATION METHOD AND LEVEL -------------------------

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
  mutate(acc_weighted = (accuracy*weights)^0.5)

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
  mutate(`Bottom-Up` = log(unrecon/bu),
         `Middle-Out (Category)` = log(unrecon/mo_cat),
         `Middle-Out (Region)` = log(unrecon/mo_reg),
         `Top-Down (Category)` = log(unrecon/tdfp_cat),
         `Top-Down (Region)` = log(unrecon/tdfp_reg),
         `MinT` = log(unrecon/mint),
         `Variance Scaling` = log(unrecon/wls),
         `No Scaling` = log(unrecon/ols),
         `BSR` = log(unrecon/bsr),
         `Structural Scaling` = log(unrecon/nseries)) %>% 
  dplyr::select(-c(unrecon,bu,mint,mo_cat,mo_reg,ols,tdfp_cat,tdfp_reg,wls,bsr,nseries)) %>% 
  gather(key = "Reconciliation", value = "Accuracy", -c(reg_lvl,cat_lvl)) %>% 
  mutate(Grouping = recode(Reconciliation,
                           "Bottom-Up" = 1,
                           "Middle-Out (Category)" = 1,
                           "Middle-Out (Region)" = 1,
                           "Top-Down (Category)" = 1,
                           "Top-Down (Region)" = 1,
                           "No Scaling" = 2,
                           "Variance Scaling" = 2,
                           "BSR" = 2,
                           "Structural Scaling" = 2,
                           "MinT" = 2),
         Reconciliation = factor(Reconciliation,
                                 levels = c("Bottom-Up","Middle-Out (Category)","Middle-Out (Region)",
                                            "Top-Down (Category)","Top-Down (Region)","No Scaling",
                                            "Structural Scaling","Variance Scaling","MinT","BSR"),
                                 ordered = T))

ggplot(tab4, aes(x=Grouping, y=Accuracy, fill = Reconciliation)) +   
  geom_hline(aes(yintercept=0), color = "black", lwd = 0.25) +
  facet_grid(reg_lvl ~ cat_lvl, switch = "y") +
  geom_bar(colour="black", stat="identity", width = 0.5,lwd = 0.1, position = position_dodge()) +
  scale_y_continuous(position = "right", breaks = seq(-0.2,0.2,0.1),
                     limits = c(-0.15,0.15), minor_breaks = NULL) +
  scale_x_continuous(breaks = c(1,2),labels = c("Single\nLevel","Optimal\nCombination")) +
  scale_fill_manual(values = c(bpy.colors(11)[-11])) +
  ylab("log relative RMSFE (weighted average)") +
  xlab("Reconciliation Methods") +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size=0.2),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.key.size = unit(0.2, 'cm'),
        legend.text = element_text(size = 10))  +
  guides(fill=guide_legend(nrow=2, override.aes = list(color="white")))


ggsave("tex/fig/fig_eval_rmse_relative.pdf", device = "pdf",
       width = 18, height = 12, units = "cm")





# RELATIVE RMSE BY RECONCILIATION METHOD AND LEVEL (JITTER) --------

# x = reconciliation methods, y = accuracy
tab <- tabs$RMSE %>% 
  filter(fmethod == "arima") %>% 
  gather(key = level, value = accuracy, -c(date,recon,fmethod)) %>% 
  mutate(accuracy = accuracy^2) %>% 
  group_by(recon,level) %>%
  summarise(accuracy = mean(accuracy)^0.5) %>% 
  ungroup

tab2 <- tab %>% 
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
      levels = c("Total","Category","Subcategory"), ordered = T))

tab3 <- tab2 %>% 
  spread(key = recon, value = accuracy) %>% 
  mutate(`Bottom-Up` = log(unrecon/bu),
         `Middle-Out (Category)` = log(unrecon/mo_cat),
         `Middle-Out (Region)` = log(unrecon/mo_reg),
         `Top-Down (Category)` = log(unrecon/tdfp_cat),
         `Top-Down (Region)` = log(unrecon/tdfp_reg),
         `MinT` = log(unrecon/mint),
         `Variance Scaling` = log(unrecon/wls),
         `No Scaling` = log(unrecon/ols),
         `BSR` = log(unrecon/bsr),
         `Structural Scaling` = log(unrecon/nseries)) %>% 
  dplyr::select(-c(level,unrecon,bu,mint,mo_cat,mo_reg,ols,tdfp_cat,tdfp_reg,wls,bsr,nseries)) %>% 
  gather(key = Reconciliation, value = Accuracy, -c(reg_lvl,cat_lvl)) %>% 
  mutate(Reconciliation = factor(Reconciliation,
                                 levels = c("Bottom-Up","Middle-Out (Category)","Middle-Out (Region)",
                                            "Top-Down (Category)","Top-Down (Region)","No Scaling",
                                            "Structural Scaling","Variance Scaling","MinT","BSR"),
                                 ordered = T))

tab3$Accuracy[which(!is.finite(tab3$Accuracy))] <- NA
idx = which(tab3$reg_lvl !="World" & tab3$cat_lvl != "Total") # sample randomly to avoid overplotting
tab3$Accuracy[sample(x = idx, size = 0.8*length(idx), replace = F)] <- NA


ggplot(tab3, aes(x=Reconciliation, y=Accuracy)) +   
  geom_hline(aes(yintercept=0), color = "black", lwd = 0.1) +
  geom_jitter(aes(color = Reconciliation), size = 0.1, width = 0.25, height = 0) + 
  facet_grid(reg_lvl ~ cat_lvl, switch = "y") +
  scale_y_continuous(position = "right", breaks = seq(-15,10,5), limits = c(-15,5), minor_breaks = NULL) +
  scale_x_discrete(breaks = c("Middle-Out (Region)","Variance Scaling"), labels = c("Single\nLevel","Optimal\nCombination")) +
  scale_color_manual(values = c(bpy.colors(11)[-11])) +
  ylab("log relative RMSFE") +
  xlab("Reconciliation Methods") +
  theme_minimal() +
  theme(legend.position="bottom",
        panel.border = element_rect(colour = "black", fill = NA, size=0.2),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.2, 'cm'),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())  +
  guides(color = guide_legend(nrow=2, byrow = F,
                              override.aes = list(size=2)))



ggsave("tex/fig/fig_rmsfe_dots.pdf", device = "pdf",
       width = 18, height = 12, units = "cm")







# RELATIVE RMSE BY TIME AND LEVEL ---------------------------------------

# x = reconciliation methods, y = accuracy
tab <- tabs$RMSE %>% 
  filter(fmethod == "arima") %>% 
  filter(!(recon %in% c("bu","mo_cat","mo_reg","tdfp_reg","tdfp_cat"))) %>% 
  gather(key = level, value = accuracy, -c(date,recon,fmethod)) %>%
  mutate(accuracy = accuracy^2)

tab2 <- tab %>% 
  add_column(weights = sapply(tab$level, function(x) weights[which(names(weights) == x)])) %>% 
  mutate(acc_weighted = (accuracy*weights)^0.5)

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
  mutate(`No Scaling` = log(unrecon/ols),
         `Structural Scaling` = log(unrecon/nseries),
         `Variance Scaling` = log(unrecon/wls),
         `MinT` = log(unrecon/mint),
         `BSR` = log(unrecon/bsr)) %>% 
  dplyr::select(-c(unrecon,mint,wls,bsr,nseries,ols)) %>% 
  gather(key = Reconciliation, value = Accuracy, -c(date,reg_lvl,cat_lvl)) %>% 
  mutate(Reconciliation = factor(Reconciliation,
                                 levels = c("No Scaling","Structural Scaling","Variance Scaling","MinT","BSR"), ordered = T))

ggplot(tab4, aes(x=as.numeric(date), y=Accuracy, group = Reconciliation, size = Reconciliation)) +
  geom_hline(yintercept=0, color = "black", lwd = 0.25) +
  facet_grid(reg_lvl ~ cat_lvl, switch = "y") +
  geom_line(aes(color= Reconciliation, linetype = Reconciliation, size = Reconciliation)) +
  scale_linetype_manual(values=c(1,5,6,4,2)) +
  scale_size_manual(values = rep(0.5,5)) +
  scale_y_continuous(position = "right", breaks = seq(-0.4,0.8,0.4), minor_breaks = NULL) +
  scale_x_continuous(breaks = seq(2000, 2015, by = 5),
                     labels = c("2000","2005","2010","2015"),
                     expand = c(0,0)) +
  scale_color_manual(values = bpy.colors(11)[c(6:10)]) +
  ylab("log relative RMSFE") +
  xlab(NULL) +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size=0.2),
        legend.text = element_text(size = 10),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())

ggsave("tex/fig/fig_eval_rmse_time.pdf", device = "pdf",
       width = 20, height = 14, units = "cm")




# FORECASTING AND RECONCILIATION METHODS BY LEVEL ---------------------------------------------

# Top
tab <- rbind(tabs$MASE %>% 
               dplyr::select(c(date,recon,fmethod,Total)) %>% 
               gather(key = level, value = accuracy, -c(date,recon,fmethod)) %>% 
               filter(recon == "unrecon") %>% 
               group_by(date,fmethod,level) %>%
               summarise(accuracy = mean(accuracy,na.rm=T)) %>% 
               add_column(measure = factor("MASE", levels = c("RMSE","MAPE","MASE"))),
             tabs$RMSE %>% 
               dplyr::select(c(date,recon,fmethod,Total)) %>% 
               gather(key = level, value = accuracy, -c(date,recon,fmethod)) %>% 
               filter( recon == "unrecon") %>% 
               group_by(date,fmethod,level) %>%
               summarise(accuracy = mean(accuracy,na.rm=T)^0.5) %>% 
               add_column(measure = factor("RMSE", levels = c("RMSE","MAPE","MASE"))),
             tabs$MAPE %>% 
               dplyr::select(c(date,recon,fmethod,Total)) %>% 
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
  dplyr::select(c(date,accuracy,Method,measure)) %>% 
  add_column(level = factor("Top Level", levels = c("Top Level","Bottom Level")))


# Bottom
tab <- rbind(tabs$MASE %>% 
               dplyr::select(c(date,recon,fmethod,which(nchar(colnames(tabs$MASE)) == 7))) %>% 
               gather(key = level, value = accuracy, -c(date,recon,fmethod)) %>% 
               filter(recon == "unrecon") %>% 
               group_by(date,fmethod,level) %>%
               summarise(accuracy = mean(accuracy,na.rm=T)) %>% 
               add_column(measure = factor("MASE", levels = c("RMSE","MAPE","MASE"))),
             tabs$RMSE %>%
               dplyr::select(c(date,recon,fmethod,which(nchar(colnames(tabs$RMSE)) == 7))) %>% 
               gather(key = level, value = accuracy, -c(date,recon,fmethod)) %>% 
               filter(recon == "unrecon") %>% 
               group_by(date,fmethod,level) %>%
               summarise(accuracy = mean(accuracy^2,na.rm=T)) %>% 
               add_column(measure = factor("RMSE", levels = c("RMSE","MAPE","MASE"))),
             tabs$MAPE %>% 
               dplyr::select(c(date,recon,fmethod,which(nchar(colnames(tabs$MAPE)) == 7))) %>% 
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
  dplyr::select(c(date,accuracy,Method,measure)) %>% 
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
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())


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
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())

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
  summarise(accuracy = mean(accuracy^2,na.rm=T)^0.5) %>% 
  ungroup %>% 
  spread(key = recon, value = accuracy) %>% 
  mutate(relacc = log(unrecon/bsr))

tab2 <- tab1 %>% 
  mutate(Regions = factor(case_when(
    substr(level,1,2) == "AF" ~ "Africa\nand\nMiddle\nEast",
    substr(level,1,2) == "AO" ~ "Australia\nand\nOceania",
    substr(level,1,2) == "EA" ~ "East\nAsia",
    substr(level,1,2) == "EU" ~ "Western\nEurope",
    substr(level,1,2) == "CA" ~ "Eastern\nEurope\nand\nCentral\nAsia",
    substr(level,1,2) == "LA" ~ "Latin\nAmerica\nand the\nCaribbean",
    substr(level,1,2) == "NA" ~ "North\nAmerica",
    substr(level,1,2) == "SA" ~ "South\nAsia"),
    levels = c("Western\nEurope","North\nAmerica","East\nAsia",
               "Africa\nand\nMiddle\nEast","Latin\nAmerica\nand the\nCaribbean",
               "Eastern\nEurope\nand\nCentral\nAsia","South\nAsia","Australia\nand\nOceania"), ordered = T)) %>%
  mutate(Level = factor(case_when(
    nchar(level) == 2 ~ "Region", 
    nchar(level) == 4 ~ "Country"),
    levels = c("Region","Country"), ordered = T)) %>%
  add_column(Share = sapply(tab1$level, function(x) weights[which(names(weights) == x)])*100)

ggplot(tab2, aes(Regions, relacc)) +
  geom_hline(aes(yintercept=0), color = "darkgrey") +
  geom_jitter(aes(color = Regions, size = Share, shape = Level), width = 0.1, height = 0) +
  scale_shape_manual(name = "Level", values = c(1,20)) +
  scale_size_continuous(name = "Export Shares (in %)",
                        breaks = c(1,10,20), range = c(1,10)) +
  scale_colour_manual(values = c(bpy.colors(13)[-13]),guide=F) +
  scale_y_continuous(breaks = seq(-0.05,0.2,0.05), limits = c(-0.05,0.2)) + 
  ylab("log relative RMSFE") +
  xlab(NULL) +
  theme_bw() +
  guides(shape = guide_legend(order = 1),  size = guide_legend(order = 2)) +
  theme(legend.position="bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

ggsave("tex/fig/fig_eval_regions.pdf", device = "pdf",
       width = 18, height = 10, units = "cm")  


# Category
tab1 <- tabs$RMSE %>% 
  filter(recon %in% c("bsr","unrecon") & fmethod == "arima") %>% 
  gather(key = level, value = accuracy, -c(date,recon,fmethod)) %>% 
  filter((nchar(level) == 3) | (nchar(level) == 2 & grepl("^[A-Za-z]+$", level) == F)) %>% 
  group_by(recon,level) %>%
  summarise(accuracy = mean(accuracy^2,na.rm=T)^0.5) %>% 
  ungroup %>% 
  spread(key = recon, value = accuracy) %>% 
  mutate(relacc = log(unrecon/bsr)) %>% 
  dplyr::select(-c(unrecon,bsr))

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
  scale_shape_manual(name = "Level", values = c(1,20)) +
  scale_size_continuous(name = "Export Shares (in %)",
                        breaks = c(1,10,20), range = c(1,10), limits = c(0,40)) +
  scale_colour_manual(values = c(bpy.colors(13)[-13]),guide=F) +
  scale_y_continuous(breaks = seq(-0.05,0.2,0.05), limits = c(-0.05,0.2)) + 
  ylab("log relative RMSFE") +
  xlab(NULL) +
  theme_bw() +
  guides(shape = guide_legend(order = 1),  size = guide_legend(order = 2)) +
  theme(legend.position="bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

ggsave("tex/fig/fig_eval_categories.pdf", device = "pdf",
       width = 18, height = 10, units = "cm")  








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
                                            "ols","nseries","wls","mint","bsr"),
                                 labels = c("Unreconciled","Bottom-Up","Middle-Out (Category)","Middle-Out (Region)",
                                            "Top-Down (Category)","Top-Down (Region)","No Scaling",
                                            "Structural Scaling","Variance Scaling","MinT","BSR"),
                                 ordered = T)) %>% 
  mutate(Grouping = recode(Reconciliation,
                           "Bottom-Up" = 1,
                           "Middle-Out (Category)" = 1,
                           "Middle-Out (Region)" = 1,
                           "Top-Down (Category)" = 1,
                           "Top-Down (Region)" = 1,
                           "Variance Scaling" = 2,
                           "No Scaling" = 2,
                           "BSR" = 2,
                           "Structural Scaling" = 2,
                           "MinT" = 2,
                           "Unreconciled" = 2))
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
           colour="black", width = 0.5,lwd = 0.1, stat="identity", position = position_dodge()) +
  facet_grid(reg_lvl ~ cat_lvl, switch = "y") +
  scale_y_continuous(position = "right", breaks = seq(1,2.5,0.5)) +
  scale_x_continuous(breaks = c(1,2),labels = c("Single\nLevel","Optimal\nCombination")) +
  scale_fill_manual(values = c(bpy.colors(11)[-11])) +
  ylab("Forecast Error (MASE)") +
  xlab("Reconciliation Methods") +
  theme_bw() +
  coord_cartesian(ylim = c(1,2.5)) +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.key.size = unit(0.2, 'cm'),
        legend.text = element_text(size = 10))  +
  guides(fill=guide_legend(nrow=2, override.aes = list(color="white")))

ggsave("tex/fig/fig_eval_mase.pdf", device = "pdf",
       width = 18, height = 11, units = "cm")




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
                                            "ols","nseries","wls","mint","bsr"),
                                 labels = c("Unreconciled","Bottom-Up","Middle-Out (Category)","Middle-Out (Region)",
                                            "Top-Down (Category)","Top-Down (Region)","No Scaling",
                                            "Structural Scaling","Variance Scaling","MinT","BSR"),
                                 ordered = T)) %>% 
  mutate(Grouping = recode(Reconciliation,
                           "Bottom-Up" = 1,
                           "Middle-Out (Category)" = 1,
                           "Middle-Out (Region)" = 1,
                           "Top-Down (Category)" = 1,
                           "Top-Down (Region)" = 1,
                           "Variance Scaling" = 2,
                           "No Scaling" = 2,
                           "BSR" = 2,
                           "Structural Scaling" = 2,
                           "MinT" = 2,
                           "Unreconciled" = 2))
tab$accuracy[which(tab$accuracy > 500)] <- NA

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
           colour="black", width = 0.5,lwd = 0.1, stat="identity", position = position_dodge()) +
  facet_grid(reg_lvl ~ cat_lvl, switch = "y") +
  scale_fill_manual(values = c(bpy.colors(11)[-11])) +
  scale_y_continuous(position = "right") +
  scale_x_continuous(breaks = c(1,2),labels = c("Single\nLevel","Optimal\nCombination")) +
  ylab("Forecast Error (MAPE)") +
  xlab("Reconciliation Methods") +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.key.size = unit(0.2, 'cm'),
        legend.text = element_text(size = 10))  +
  guides(fill=guide_legend(nrow=2, override.aes = list(color="white")))

ggsave("tex/fig/fig_eval_mape.pdf", device = "pdf",
       width = 18, height = 11, units = "cm")

