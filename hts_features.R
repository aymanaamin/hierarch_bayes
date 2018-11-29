
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

# Packages
library(hts)
library(LaF)
library(forecast)
library(data.table)
library(tsfeatures)
library(ggplot2)
library(gridExtra)
library(tidyverse)

# Functions
source("lib/functions.R")

# Metadata
load("dat/countries.rda")

# Options
options(scipen=10)


# 1. IMPORT DATA -----------------------------------------------------------

total <- import_data()

total_exp <- total[substr(tsKey,1,1) == "E",]
total_exp_reg <- total_exp[, .(value = sum(value)), by = list(period, tsKey = substr(tsKey,2,5))]
total_exp_cat <- total_exp[, .(value = sum(value)), by = list(period, tsKey = substr(tsKey,6,14))]
dat_exp_reg <- dt2ts(total_exp_reg)
dat_exp_cat <- dt2ts(total_exp_cat)







# 2. GENERATE HIERARCHY ----------------------------------------------------

# define categories and countries hierarchies
tradehts <- list(cat = hts(y = dat_exp_cat, characters = c(2,1,1,2,2)),
                 reg = hts(y = dat_exp_reg, characters = c(2,2)))

rm(dat_exp_reg,dat_exp_cat,total_exp,total_exp_reg,total_exp_cat,total)







# 3. FEATURE PLOTS --------------------------------------------------------

# categorical
catlist <- as.list(aggts(tradehts$cat))
clvl <- sapply(tradehts$cat$nodes, function(x) length(x))
cgroup <- recode(substr(names(catlist),1,2),
                 "To" = "Total",
                 "01" = "Agricultural Products",
                 "02" = "Energy Source",
                 "03" = "Textiles",
                 "04" = "Graphical Products",
                 "05" = "Leather, Rubber, Plastics",
                 "06" = "Chemicals and Pharma",
                 "07" = "Stones and Earth",
                 "08" = "Metals",
                 "09" = "Machines and Electronics",
                 "10" = "Vehicles",
                 "11" = "Precision Instruments",
                 "12" = "Various Goods")
catfeatures <- tsfeatures(catlist) %>% 
  select("trend","spike","linearity","curvature","e_acf1",
         "e_acf10","seasonal_strength","peak","trough",         
         "entropy","x_acf1","x_acf10","diff1_acf1","diff1_acf10","diff2_acf1",       
         "diff2_acf10","seas_acf1") %>% 
  prcomp(scale=TRUE) %$% 
  x %>% 
  as_tibble() %>%
  select(PC1,PC2) %>% 
  add_column(Category = c(rep(names(clvl), clvl),rep("Level 6",198)),
             Goods = cgroup,
             Hierarchy = "Category") %>% 
  arrange(sample(x = length(catlist),size = length(catlist),replace = F))

# regional
reglist <- as.list(aggts(tradehts$reg))
rlvl <- sapply(tradehts$reg$nodes, function(x) length(x))
rgroup <- recode(substr(names(reglist),1,2),
                 "To" = "World",
                 "AF" = "Africa and Middle East",
                 "AO" = "Australia and Oceania",
                 "EA" = "East Asia",
                 "EU" = "Europe",
                 "CA" = "Central Asia",
                 "LA" = "Latin America",
                 "NA" = "North America",
                 "SA" = "South Asia")
regfeatures <- tsfeatures(reglist) %>% 
  select("trend","spike","linearity","curvature","e_acf1",
         "e_acf10","seasonal_strength","peak","trough",         
         "entropy","x_acf1","x_acf10","diff1_acf1","diff1_acf10","diff2_acf1",       
         "diff2_acf10","seas_acf1") %>% 
  prcomp(scale=TRUE) %$% 
  x %>% 
  as_tibble() %>%
  select(PC1,PC2) %>% 
  add_column(Region = c(rep(names(rlvl), rlvl),rep("Level 3",245)),
             Continents = rgroup,
             Hierarchy = "Region") %>% 
  arrange(sample(x = length(reglist),size = length(reglist),replace = F))

# Plot by levels
grid.arrange(ggplot(catfeatures, aes(x=PC1, y=PC2)) + 
               geom_point(aes(colour = Category), size = 1) +
               scale_color_grey(start=0.1, end=0.9) +
               scale_x_continuous(limits = c(-6, 8)) +
               scale_y_continuous(limits = c(-6, 6)) +
               theme_bw() +
               theme(legend.position="bottom") + 
               guides(color=guide_legend(nrow=3)),
             
             ggplot(regfeatures, aes(x=PC1, y=PC2)) + 
               geom_point(aes(colour = Region), size = 1) + 
               scale_color_grey(start=0.1, end=0.9) +
               scale_x_continuous(limits = c(-8, 8)) +
               scale_y_continuous(limits = c(-6, 6)) +
               theme_bw() +
               theme(legend.position="bottom") + 
               guides(color=guide_legend(nrow=3)),
             nrow = 1)

grid.arrange(ggplot(catfeatures, aes(x=PC1, y=PC2)) + 
               geom_point(aes(colour = Goods), size = 1) + 
               scale_colour_hue() +
               scale_x_continuous(limits = c(-6, 8)) +
               scale_y_continuous(limits = c(-6, 6)) +
               theme_bw() +
               theme(legend.position="bottom") + 
               guides(color=guide_legend(nrow=7,title = NULL)),
             
             ggplot(regfeatures, aes(x=PC1, y=PC2)) + 
               geom_point(aes(colour = Continents), size = 1) + 
               scale_colour_hue() +
               scale_x_continuous(limits = c(-8, 8)) +
               scale_y_continuous(limits = c(-6, 6)) +
               theme_bw() +
               theme(legend.position="bottom") + 
               guides(color=guide_legend(nrow=7,title = NULL)),
             nrow = 1)




# 
# # MDS ---------------------------------------------------------------------
# 
# # categorical
# catlist <- as.list(aggts(tradehts$cat))
# clvl <- sapply(tradehts$cat$nodes, function(x) length(x))
# cgroup <- substr(names(catlist),1,2)
# cgroup[1] <- "Total"
# catfeatures <- tsfeatures(catlist) %>% 
#   select("trend","spike","linearity","curvature","e_acf1",
#          "e_acf10","seasonal_strength","peak","trough",         
#          "entropy","x_acf1","x_acf10","diff1_acf1","diff1_acf10","diff2_acf1",       
#          "diff2_acf10","seas_acf1") %>% 
#   dist() %>%
#   embedding(method = "MDSiso") %>%
#   as_tibble() %>%
#   select(Comp1,Comp2) %>% 
#   add_column(Category = c(rep(names(clvl), clvl),rep("Level 6",198)),
#              Goods = cgroup,
#              Hierarchy = "Category")
# 
# # regional
# reglist <- as.list(aggts(tradehts$reg))
# rlvl <- sapply(tradehts$reg$nodes, function(x) length(x))
# rgroup <- substr(names(reglist),1,2)
# rgroup[1] <- "Total"
# regfeatures <- tsfeatures(reglist) %>% 
#   select("trend","spike","linearity","curvature","e_acf1",
#          "e_acf10","seasonal_strength","peak","trough",         
#          "entropy","x_acf1","x_acf10","diff1_acf1","diff1_acf10","diff2_acf1",       
#          "diff2_acf10","seas_acf1") %>% 
#   dist() %>%
#   embedding(method = "MDSiso") %>%
#   as_tibble() %>%
#   select(Comp1,Comp2) %>% 
#   add_column(Region = c(rep(names(rlvl), rlvl),rep("Level 3",245)),
#              Continents = rgroup,
#              Hierarchy = "Region")
# 
# # Plot by levels
# grid.arrange(ggplot(catfeatures, aes(x=Comp1, y=Comp2)) + 
#                geom_point(aes(colour = Category), size = 1.5) + 
#                scale_color_grey(start=0.1, end=0.9) +
#                scale_x_continuous(limits = c(-3, 3)) +
#                scale_y_continuous(limits = c(-3, 3)) +
#                theme_bw() +
#                theme(legend.position="bottom") + 
#                guides(color=guide_legend(nrow=3)),
#              
#              ggplot(regfeatures, aes(x=Comp1, y=Comp2)) + 
#                geom_point(aes(colour = Region), size = 1.5) + 
#                scale_color_grey(start=0.1, end=0.9) +
#                scale_x_continuous(limits = c(-3, 3)) +
#                scale_y_continuous(limits = c(-3, 3)) +
#                theme_bw() +
#                theme(legend.position="bottom") + 
#                guides(color=guide_legend(nrow=3)),
#              nrow = 1)
# 
# grid.arrange(ggplot(catfeatures, aes(x=Comp1, y=Comp2)) + 
#                geom_point(aes(colour = Goods), size = 1.5) + 
#                scale_colour_hue() +
#                scale_x_continuous(limits = c(-3, 3)) +
#                scale_y_continuous(limits = c(-3, 3)) +
#                theme_bw() +
#                theme(legend.position="bottom") + 
#                guides(color=guide_legend(nrow=2)),
#              
#              ggplot(regfeatures, aes(x=Comp1, y=Comp2)) + 
#                geom_point(aes(colour = Continents), size = 1.5) + 
#                scale_colour_hue() +
#                scale_x_continuous(limits = c(-3, 3)) +
#                scale_y_continuous(limits = c(-3, 3)) +
#                theme_bw() +
#                theme(legend.position="bottom") + 
#                guides(color=guide_legend(nrow=2)),
#              nrow = 1)
# 
# 







# # 4. FEATURES BY GROUP
# 
# # categorical
# catlist <- as.list(aggts(tradehts$cat))
# cgroup <- substr(names(catlist),1,2)
# cgroup[1] <- "Total"
# catfeatures <- tsfeatures(catlist) %>% 
#   dplyr::select("trend","spike","linearity","curvature","e_acf1",
#          "e_acf10","seasonal_strength","peak","trough",         
#          "entropy","x_acf1","x_acf10","diff1_acf1","diff1_acf10","diff2_acf1",       
#          "diff2_acf10","seas_acf1") %>% 
#   prcomp(scale=TRUE) %$% 
#   x %>% 
#   as_tibble() %>%
#   select(PC1,PC2) %>% 
#   add_column(Category = c(rep(names(clvl), clvl),rep("Level 6",198)),
#              Goods = cgroup,
#              Hierarchy = "Category")
# 
# # regional
# reglist <- as.list(aggts(tradehts$reg))
# rgroup <- substr(names(reglist),1,2)
# rgroup[1] <- "Total"
# regfeatures <- tsfeatures(reglist) %>% 
#   select("trend","spike","linearity","curvature","e_acf1",
#          "e_acf10","seasonal_strength","peak","trough",         
#          "entropy","x_acf1","x_acf10","diff1_acf1","diff1_acf10","diff2_acf1",       
#          "diff2_acf10","seas_acf1") %>% 
#   prcomp(scale=TRUE) %$% 
#   x %>% 
#   as_tibble() %>%
#   select(PC1,PC2) %>% 
#   add_column(Region = c(rep(names(rlvl), rlvl),rep("Level 3",245)),
#              Continents = rgroup,
#              Hierarchy = "Region")
# 
# grid.arrange(ggplot(catfeatures, aes(x=PC1, y=PC2)) + 
#                geom_point(aes(colour = Goods), size = 1.5) + 
#                scale_colour_hue() +
#                scale_x_continuous(limits = c(-6, 8)) +
#                scale_y_continuous(limits = c(-6, 6)) +
#                theme_bw() +
#                theme(legend.position="bottom") + 
#                guides(color=guide_legend(nrow=2)),
#              
#              ggplot(regfeatures, aes(x=PC1, y=PC2)) + 
#                geom_point(aes(colour = Continents), size = 1.5) + 
#                scale_colour_hue() +
#                scale_x_continuous(limits = c(-8, 8)) +
#                scale_y_continuous(limits = c(-6, 6)) +
#                theme_bw() +
#                theme(legend.position="bottom") + 
#                guides(color=guide_legend(nrow=2)),
#              nrow = 1)

catfeatures <- tsfeatures(catlist) %>%
  select("trend","spike","linearity","curvature","e_acf1",
         "e_acf10","seasonal_strength","peak","trough",
         "entropy","x_acf1","x_acf10","diff1_acf1","diff1_acf10","diff2_acf1",
         "diff2_acf10","seas_acf1") %>%
  dist() %>%
  embedding(method = "MDS") %>%
  add_column(lvl = c(rep(names(clvl), clvl),rep("Level 6",198)))

# categorical
catlist <- as.list(aggts(tradehts$cat))
clvl <- sapply(tradehts$cat$nodes, function(x) length(x))
cgroup <- substr(names(catlist),1,2)
cgroup[1] <- "Total"
catfeatures <- tsfeatures(catlist) %>% 
  select("trend","spike","linearity","curvature","e_acf1",
         "e_acf10","seasonal_strength","peak","trough",         
         "entropy","x_acf1","x_acf10","diff1_acf1","diff1_acf10","diff2_acf1",       
         "diff2_acf10","seas_acf1") %>% 
  dist() %>%
  embedding(method = "MDS") %>%
  as_tibble() %>%
  select(Comp1,Comp2) %>% 
  add_column(Category = c(rep(names(clvl), clvl),rep("Level 6",198)),
             Goods = cgroup,
             Hierarchy = "Category")

