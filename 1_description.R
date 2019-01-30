
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

# Packages
library(hts)
library(data.table)
library(tsfeatures)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(sp)

# Functions
source("lib/functions_evaluation.R")

# Data
load("dat/countries.rda")
load("dat/tradegts.Rdata")
load("dat/tradehts.Rdata")
load("dat/tradegts_reduced2.Rdata")
tsl <- as.list(aggts(tradegts)/1e+9)

# Options
options(scipen=10)


# 1. STACKED AREA PLOTS ---------------------------------------------------

tab_reg <- tibble("Date" = time(tsl$`Regions Total/AF`),
                  "Europe (54.3%)" = tsl$`Regions Total/EU`,
                  "North America (16.9%)" = tsl$`Regions Total/NA`,
                  "East Asia (16.6%)" = tsl$`Regions Total/EA`,
                  "Africa and Middle East (5.6%)"  = tsl$`Regions Total/AF`,
                  "Australia and Oceania (1.2%)" = tsl$`Regions Total/AO`,
                  "Central Asia (1.4%)" = tsl$`Regions Total/CA`,
                  "Latin America (3.0%)" = tsl$`Regions Total/LA`,
                  "South Asia (1.1%)" = tsl$`Regions Total/SA`) %>% 
  gather(key = Region, value = Exports, -Date) %>% 
  mutate(Region = fct_reorder(factor(Region), Exports, last, .desc = T))


ggplot(tab_reg, aes(x = Date, y = Exports)) +
  geom_area(aes(fill = Region), alpha=0.75) +
  scale_x_continuous(breaks = seq(1990,2015,5)) +
  coord_cartesian(expand = FALSE) +
  scale_y_continuous() +
  labs(x = NULL, y = "Exports (nominal, in billion CHF)") +
  scale_fill_manual(values = bpy.colors(8)) +
  theme_bw(base_size = 9) +
  theme(legend.position="bottom") +
  guides(fill = guide_legend(ncol = 4,title = NULL))

ggsave("tex/fig/fig_area_reg.pdf", device = "pdf",
       width = 16, height = 10, units = "cm")

tab_cat <- tibble("Date" = time(tsl$`Goods Total Lvl 1/01`),
                  "Agricultural Products (4.4%)" = tsl$`Goods Total Lvl 1/01`,
                  "Energy Source (0.9%)" = tsl$`Goods Total Lvl 1/02`,
                  "Textiles (1.9%)" = tsl$`Goods Total Lvl 1/03`,
                  "Graphical Products (0.8%)" = tsl$`Goods Total Lvl 1/04`,
                  "Leather, Rubber, Plastics (1.9%)" = tsl$`Goods Total Lvl 1/05`,
                  "Chemicals and Pharmaceuticals (44.7%)" = tsl$`Goods Total Lvl 1/06`,
                  "Stones and Earth (0.4%)" = tsl$`Goods Total Lvl 1/07`,
                  "Metals (6.2%)" = tsl$`Goods Total Lvl 1/08`,
                  "Machines and Electronics (14.5%)" = tsl$`Goods Total Lvl 1/09`,
                  "Vehicles (2.5%)" = tsl$`Goods Total Lvl 1/10`,
                  "Precision Instruments (21.2%)" = tsl$`Goods Total Lvl 1/11`,
                  "Various Goods (0.6%)" = tsl$`Goods Total Lvl 1/12`) %>% 
  gather(key = Categories, value = Exports, -Date) %>% 
  mutate(Categories = fct_reorder(factor(Categories), Exports, last, .desc = T))


ggplot(tab_cat, aes(x = Date, y = Exports)) +
  geom_area(aes(fill = Categories), alpha=0.75) +
  scale_x_continuous(breaks = seq(1990,2015,5)) +
  scale_y_continuous() +
  coord_cartesian(expand = FALSE) +
  labs(x = NULL, y = "Exports (nominal, in billion CHF)") +
  scale_fill_manual(values = bpy.colors(12)) +
  theme_bw(base_size = 9) +
  theme(legend.position="bottom") +
  guides(fill = guide_legend(ncol = 3,title = NULL))

ggsave("tex/fig/fig_area_cat.pdf", device = "pdf",
       width = 16, height = 10, units = "cm")

# tot <- tab_cat %>% spread(key = Categories, value = Exports) %>% filter(Date >= 2017 & Date < 2018)
# round(colSums(tot[,-1])/sum(tot[,-1])*100,1)
# tot <- tab_reg %>% spread(key = Region, value = Exports) %>% filter(Date >= 2017 & Date < 2018)
# round(colSums(tot[,-1])/sum(tot[,-1])*100,1)




# 2. FEATURE PLOTS --------------------------------------------------------

# regional
reglist <- as.list(aggts(tradehts$reg))
rlvl <- sapply(tradehts$reg$nodes, function(x) length(x))
reg_exports <- sapply(as.list(aggts(tradehts$reg)), 
                      function(x) mean(tail(x,12)))
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
regfeatures <- tsfeatures(aggts(tradehts$reg)) %>% 
  select("trend","spike","linearity","curvature","e_acf1",
         "e_acf10","seasonal_strength","peak","trough",         
         "entropy","x_acf1","x_acf10","diff1_acf1","diff1_acf10","diff2_acf1",       
         "diff2_acf10","seas_acf1") %>% 
  prcomp(scale=TRUE) %$% 
  x %>% 
  as_tibble() %>%
  select(PC1) %>% 
  add_column(Region = rgroup,
             Level = c(rep(1:length(rlvl), rlvl),rep(3,245)),
             Exports = reg_exports) %>% 
  arrange(sample(x = length(reglist),size = length(reglist),replace = F)) %>% 
  mutate(Region = factor(Region, levels = c("Europe",
                                            "North America",
                                            "East Asia",
                                            "Africa and Middle East",
                                            "Latin America",
                                            "Central Asia","South Asia",
                                            "Australia and Oceania",
                                            "World"))) %>% 
  filter(Exports > 1)

regfeatures$PC1 <- -regfeatures$PC1 

ggplot(regfeatures, aes(x=Exports, y=PC1)) + 
  geom_point(aes(colour = Region, size = Level), alpha = 0.75) +
  scale_color_manual(values = c(bpy.colors(8),"black")) +
  scale_size(breaks = 1:3, labels = c("World","Region","Country"), trans = "reciprocal") +
  coord_trans(x = "log") +
  scale_x_continuous(breaks = c(1e+1,1e+2,1e+3,1e+4,1e+5,1e+6,1e+7,1e+8,1e+9,1e+10),
                     labels = c("1e+1","1e+2","1e+3","1e+4","1e+5","1e+6","1e+7",
                                "1e+8","1e+9","1e+10")) +
  theme_bw() +
  xlab("Export Volume (in bn CHF, log scale)") +
  ylab("Predictability") +
  theme(legend.position="bottom", legend.box = "horizontal") + 
  guides(color=guide_legend(nrow=3), size=guide_legend(nrow=3))

ggsave("tex/fig/fig_confetti.pdf", device = "pdf",
       width = 18, height = 10, units = "cm")



# 4. SEASONALITIES --------------------------------------------------------

tot <- matrix(window(tsl$`Goods Total Lvl 2/011`, start = 2012),nrow =12)*1e+3
aus <- matrix(window(tsl$`Goods Lvl 2 per Country/AOAU011`, start = 2012),nrow =12)*1e+3

data <- rbind(tibble(series = factor("World"),
                     avg = apply(tot,1,FUN=mean),
                     min = apply(tot,1,FUN=min),
                     max = apply(tot,1,FUN=max),
                     mon = as_factor(month.abb,ordered = T)),
              tibble(series = "Australia",
                     avg = apply(aus,1,FUN=mean),
                     min = apply(aus,1,FUN=min),
                     max = apply(aus,1,FUN=max),
                     mon = as_factor(month.abb,ordered = T)))


ggplot(data, aes(x = mon, group = series)) +
  geom_ribbon(aes(ymin = min, ymax = max), fill = "grey70", alpha = 0.5) +
  geom_line(aes(y = avg)) +
  facet_grid(series ~ ., scales = "free_y") +
  ylab("Volume (in Mio. CHF)") +
  xlab(NULL) +
  theme_bw()

ggsave("tex/fig/fig_season.pdf", device = "pdf",
       width = 18, height = 5, units = "cm")


# Plot Forecasts next to seasonality plot


