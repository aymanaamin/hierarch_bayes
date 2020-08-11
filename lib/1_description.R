
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
library(treemapify)
library(tempdisagg)
library(RColorBrewer)

# Data
load("dat/countries.rda")
load("dat/countries.rda")
load("dat/tradegts.Rdata")
load("dat/tradehts.Rdata")
load("dat/tradehts_reduced.Rdata")
load("dat/tradegts_reduced2.Rdata")
tsl <- as.list(aggts(tradegts_reduced2))

# Options
options(scipen=10)


# 1. STACKED AREA PLOTS ---------------------------------------------------

tab <- bind_rows(tibble("Date" = time(tsl$`Regions Total/AF`),
                        "Western Europe (53.4%)" = tsl$`Regions Total/EU`,
                        "North America (17.8%)" = tsl$`Regions Total/NA`,
                        "East Asia (16.6%)" = tsl$`Regions Total/EA`,
                        "Africa and Middle East (5.3%)"  = tsl$`Regions Total/AF`,
                        "Australia and Oceania (1.3%)" = tsl$`Regions Total/AO`,
                        "Eastern Europe & Central Asia (1.5%)" = tsl$`Regions Total/CA`,
                        "Latin America & the Caribbean (3.0%)" = tsl$`Regions Total/LA`,
                        "South Asia (1.0%)" = tsl$`Regions Total/SA`) %>% 
                   gather(key = Name, value = Exports, -Date) %>% 
                   add_column("Classification" = factor("by Region",levels = c("by Region","by Category"))),
                 tibble("Date" = time(tsl$`Goods Total Lvl 1/01`),
                        "Agricultural Products (4.3%)" = tsl$`Goods Total Lvl 1/01`,
                        "Energy Source (1.2%)" = tsl$`Goods Total Lvl 1/02`,
                        "Textiles (2.1%)" = tsl$`Goods Total Lvl 1/03`,
                        "Graphical Products (0.7%)" = tsl$`Goods Total Lvl 1/04`,
                        "Leather, Rubber, Plastics (2.0%)" = tsl$`Goods Total Lvl 1/05`,
                        "Chemicals and Pharmaceuticals (44.7%)" = tsl$`Goods Total Lvl 1/06`,
                        "Stones and Earth (0.4%)" = tsl$`Goods Total Lvl 1/07`,
                        "Metals (6.2%)" = tsl$`Goods Total Lvl 1/08`,
                        "Machines and Electronics (14.4%)" = tsl$`Goods Total Lvl 1/09`,
                        "Vehicles (2.2%)" = tsl$`Goods Total Lvl 1/10`,
                        "Precision Instruments (21.3%)" = tsl$`Goods Total Lvl 1/11`,
                        "Various Goods (0.6%)" = tsl$`Goods Total Lvl 1/12`) %>% 
                   gather(key = Name, value = Exports, -Date) %>% 
                   add_column("Classification" = factor("by Category",levels = c("by Region","by Category"))))

tab2 <- tab %>% 
  add_row(Date = 2000,
          Name = "Regions",
          Exports = 0,
          Classification = factor("by Region", levels = c("by Region","by Category"))) %>% 
  add_row(Date = 2000,
          Name = "Categories",
          Exports = 0,
          Classification = factor("by Category",levels = c("by Region","by Category"))) %>% 
  add_row(Date = 2000,
          Name = " ", 
          Exports = 0,
          Classification = factor("by Category",levels = c("by Region","by Category"))) %>% 
  mutate(Name = factor(Name,levels = c("Regions",
                                       "Western Europe (53.4%)",
                                       "North America (17.8%)",
                                       "East Asia (16.6%)" ,
                                       "Africa and Middle East (5.3%)" ,
                                       "Latin America & the Caribbean (3.0%)" ,
                                       "Eastern Europe & Central Asia (1.5%)" ,
                                       "South Asia (1.0%)",
                                       "Australia and Oceania (1.3%)",
                                       " ",
                                       "Categories",
                                       "Chemicals and Pharmaceuticals (44.7%)",
                                       "Precision Instruments (21.3%)",
                                       "Machines and Electronics (14.4%)",
                                       "Metals (6.2%)",
                                       "Agricultural Products (4.3%)",
                                       "Vehicles (2.2%)" ,
                                       "Textiles (2.1%)" ,
                                       "Leather, Rubber, Plastics (2.0%)",
                                       "Energy Source (1.2%)",
                                       "Graphical Products (0.7%)" ,
                                       "Various Goods (0.6%)",
                                       "Stones and Earth (0.4%)"), ordered = T)) %>% 
  mutate(Exports = Exports/1e+9)



ggplot(tab2, aes(x = Date, y = Exports)) + 
  geom_area(aes(fill = Name)) +
  scale_x_continuous(breaks = seq(1990,2015,5)) +
  coord_cartesian(expand = FALSE) +
  scale_y_continuous() +
  facet_grid(Classification ~ .) +
  labs(x = NULL, y = "Monthly Exports (in billion CHF)") +
  scale_fill_manual(values = c("white",colorRampPalette(rev(brewer.pal(9,"Blues")))(9)[-9],
                               "white","white",colorRampPalette(rev(brewer.pal(9,"Blues")))(13)[-13])) +
  theme_minimal(base_size = 9) +
  theme(legend.position="right",
        text = element_text(size = 11),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(title = NULL, ncol = 1))


ggsave("tex/fig/fig_area.pdf", device = "pdf",
       width = 18, height = 14.8, units = "cm")




# 2. FEATURE PLOTS --------------------------------------------------------

# regional
reglist <- as.list(aggts(tradehts$reg))
rlvl <- sapply(tradehts$reg$nodes, function(x) length(x))
reg_exports <- sapply(as.list(aggts(tradehts$reg)), 
                      function(x) mean(tail(x,12)))
rgroup <- recode(substr(names(reglist),1,2),
                 "To" = "World",
                 "AF" = "Africa & Middle East",
                 "AO" = "Australia & Oceania",
                 "EA" = "East Asia",
                 "EU" = "Western Europe",
                 "CA" = "Eastern Europe & Central Asia",
                 "LA" = "Latin America & Caribbean",
                 "NA" = "North America",
                 "SA" = "South Asia")
regfeatures <- tsfeatures(aggts(tradehts$reg)) %>% 
  dplyr::select("trend","spike","linearity","curvature","e_acf1",
                "e_acf10","seasonal_strength","peak","trough",         
                "entropy","x_acf1","x_acf10","diff1_acf1","diff1_acf10","diff2_acf1",       
                "diff2_acf10","seas_acf1") %>% 
  prcomp(scale=TRUE)

regfeatures <- regfeatures$x %>% 
  as_tibble() %>%
  dplyr::select(PC1) %>% 
  add_column(Region = rgroup,
             Level = c(rep(1:length(rlvl), rlvl),rep(3,245)),
             Exports = reg_exports) %>% 
  arrange(sample(x = length(reglist),size = length(reglist),replace = F)) %>% 
  mutate(Region = factor(Region, levels = c("Western Europe",
                                            "North America",
                                            "East Asia",
                                            "Africa & Middle East",
                                            "Latin America & Caribbean",
                                            "Eastern Europe & Central Asia",
                                            "South Asia",
                                            "Australia & Oceania",
                                            "World"))) %>% 
  filter(Exports > 1)

regfeatures$PC1 <- -regfeatures$PC1 

ggplot(regfeatures, aes(x=Exports, y=PC1)) + 
  geom_point(aes(colour = Region, size = Level)) +
  scale_color_manual(values = c(rev(brewer.pal(9,"Blues"))[-9],"black")) +
  scale_size(breaks = 1:3, labels = c("World","Region","Country"), trans = "reciprocal") +
  coord_trans(x = "log") +
  scale_x_continuous(breaks = c(1e+1,1e+2,1e+3,1e+4,1e+5,1e+6,1e+7,1e+8,1e+9,1e+10),
                     minor_breaks = NULL,
                     labels = c("1e+1","1e+2","1e+3","1e+4","1e+5","1e+6","1e+7",
                                "1e+8","1e+9","1e+10")) +
  scale_y_continuous(breaks = seq(-4,8,2), minor_breaks = NULL, limits = c(-4,7)) +
  theme_minimal() +
  xlab("Export Volume (in CHF, log scale)") +
  ylab("Predictability") +
  theme(legend.position="bottom",
        legend.box = "horizontal",
        panel.grid.major.x = element_blank()) + 
  guides(color=guide_legend(nrow=3,override.aes = list(size=2)),
         size=guide_legend(nrow=3))



ggsave("tex/fig/fig_confetti.pdf", device = "pdf",
       width = 18.5, height = 10, units = "cm")






# 3. TREEMAP ---------------------------------------------------------------

# Get Metadata
cntr <- as_tibble(countries) %>% 
  filter(validTo == "12.2999" & isoCode != "CH")


# Aggregate Geographical Data
dat <- as.tibble(sapply(1:nrow(cntr),
                        function(x) tsl[[paste0("Countries Total/",
                                                cntr$regCode[x],
                                                cntr$isoCode[x])]]/tsl$Total)) %>%
  add_column(date = floor(seq(1988, to = 2019-1/12, by = 1/12)), .before = 1)
colnames(dat) <- c("date",cntr$isoCode)
dat <- dat %>% 
  gather(key = isoCode, value = share, -date) %>% 
  group_by(date,isoCode) %>% 
  summarise(share2 = mean(share)) %>% 
  ungroup 

# Get Country and Region Names
dat <- inner_join(x = dat, y = cntr, by = "isoCode")

# Get Regional weights
dat_regional <- dat %>% 
  group_by(date,regCode) %>% 
  summarise(share1 = sum(share2)) %>% 
  ungroup 

dat_reg <- full_join(dat,dat_regional, by = c("date", "regCode")) %>% 
  filter(date %in% c(1988,2018)) %>% 
  mutate(lvl1 = recode(regCode,
                       "AF" = "Africa and Middle East",
                       "AO" = "Australia and Oceania",
                       "EA" = "East Asia",
                       "EU" = "Europe",
                       "CA" = "Central Asia",
                       "LA" = "Latin America",
                       "NA" = "North America",
                       "SA" = "South Asia")) %>% 
  rename(lvl2 = "country") %>% 
  select(date,share1,share2,lvl1,lvl2) %>% 
  mutate(share1 = share1*100, share2=share2*100) %>% 
  add_column("hierarchy" = factor("by Region",levels = c("by Region","by Category")))


# Aggregate Categorical Data
load("dat/goods.rdata")
goods <- as_tibble(goods) %>% 
  filter(nchar(code) == 4) %>% 
  filter(!(code %in% c("13.1","13.2","14.1","14.2")))
goods$code <- gsub(pattern = ".", replacement = "", x = goods$code, fixed=T)

dat <- as.tibble(sapply(goods$code,
                        function(x) tsl[[paste0("Goods Total Lvl 2/",x)]]/tsl$Total)) %>%
  add_column(date = floor(seq(1988, to = 2019-1/12, by = 1/12)), .before = 1)
colnames(dat) <- c("date",goods$code)
dat <- dat %>% 
  gather(key = code, value = share, -date) %>% 
  group_by(date,code) %>% 
  summarise(share2 = mean(share)) %>% 
  ungroup %>% 
  mutate(catcode = substr(code,1,2))

# Get category weights
dat_categorical <- dat %>% 
  group_by(date,catcode) %>% 
  summarise(share1 = sum(share2)) %>% 
  ungroup 

dat_cat <- full_join(dat,dat_categorical, by = c("date", "catcode")) %>% 
  filter(date %in% c(1988,2018)) %>% 
  mutate(lvl1 = recode(catcode,
                       "01"  = "Agricultural Products",
                       "02"  = "Energy Source",
                       "03"  = "Textiles",
                       "04"  = "Graphical Products",
                       "05"  = "Leather, Rubber, Plastics",
                       "06"  = "Chemicals and Pharmaceuticals",
                       "07"  = "Stones and Earth",
                       "08"  = "Metals",
                       "09"  = "Machines and Electronics",
                       "10"  = "Vehicles",
                       "11"  = "Precision Instruments",
                       "12"  = "Various Goods"))

dat_cat <- inner_join(dat_cat,goods, by = "code") %>% 
  rename(lvl2 = "description") %>% 
  select(date,share1,share2,lvl1,lvl2) %>% 
  mutate(share1 = share1*100, share2=share2*100) %>% 
  add_column("hierarchy" = factor("by Category",levels = c("by Region","by Category")))

dat <- bind_rows(dat_reg,dat_cat) %>% 
  mutate(date = as.character(date)) %>% 
  mutate(date = recode(date,"1988" = "in 1988","2018" = "in 2018"))

ggplot(dat, aes(area = share2, fill = share1, label = lvl2, subgroup = lvl1)) +
  geom_treemap(colour = "dark grey") +
  facet_grid(hierarchy ~ date, switch = "y") +
  scale_fill_gradient(name = "Regional or Categorical Share of Exports (in %)" , low = "#a2ddff", high = "#08306B") +
  geom_treemap_text(colour = "white", place = "bottomleft", size = 8, 
                    min.size = 8, grow = F, alpha = 0.5) +
  geom_treemap_subgroup_border(colour = "black", lwd = 1) +
  geom_treemap_subgroup_text(colour = "white", size = 10, min.size = 10, 
                             place = "centre", reflow = T, grow = F) +
  theme_minimal() + 
  theme(legend.position="bottom", 
        strip.text = element_text(size = 10))  +
  guides(fill=guide_colourbar(barwidth=10,barheight = 0.3))

ggsave("tex/fig/fig_treemap.pdf", device = "pdf",
       width = 18, height = 12, units = "cm")

ggsave("tex/fig/fig_treemap_pres.pdf", device = "pdf",
       width = 20, height = 9, units = "cm")





# SUMMARY TABLE -----------------------------------------------------------

library(tidyverse)
library(xtable)

level <- unname(sapply(names(tsl), function(x){
  
  if(grepl("/",x)) strsplit(x, "/", fixed = T)[[1]][2] else x
  
}))


reg_lvl <- factor(case_when(
  level == "Total" ~ "World",
  nchar(level) == 2 & grepl("^[A-Za-z]+$", level) == F ~ "World",
  nchar(level) == 3 & grepl("^[A-Za-z]+$", level) == F ~ "World",
  nchar(level) == 2 & grepl("^[A-Za-z]+$", level) == T ~ "Region",
  nchar(level) %in% c(4,5) & grepl("^[A-Za-z]+$", level) == F ~ "Region",
  nchar(level) == 4 & grepl("^[A-Za-z]+$", level) == T ~ "Country",
  nchar(level) > 5 & grepl("^[A-Za-z]+$", level) == F ~ "Country"),
  levels = c("World","Region","Country"), ordered = T)

cat_lvl <- factor(case_when(
  level == "Total" ~ "Total",
  nchar(level) %in% c(2,4) & grepl("^[A-Za-z]+$", level) == T ~ "Total",
  nchar(level) %in% c(2) & grepl("^[A-Za-z]+$", level) == F ~ "Category",
  nchar(level) %in% c(4,6) & grepl("^[A-Za-z]+$", level) == F ~ "Category",
  nchar(level) %in% c(3,5,7) & grepl("^[A-Za-z]+$", level) == F ~ "Subcategory"),
  levels = c("Total","Category","Subcategory"), ordered = T)

tab <- tibble("level" = level,
              "region" = reg_lvl,
              "category" = cat_lvl)

matrix(t(table(tab[,-1])))

grid <- expand.grid(unique(tab$category),unique(tab$region))
out <- do.call(rbind,lapply(1:nrow(grid), function(ix){
  
  ser <- tsl[which(tab$category == grid[ix,1] & tab$region == grid[ix,2])]
  ser_mn <- sapply(ser, mean)
  round(c("n" = length(ser_mn),
          "mean" = mean(ser_mn)/1e+6,
          "sd" = sd(ser_mn)/1e+6,
          "min" = min(ser_mn)/1e+6,
          "max" = max(ser_mn)/1e+6,
          "iqr" = IQR(ser_mn)/1e+6))
  
}))

print(xtable(cbind(as.data.frame(grid)[,c(2,1)],out),
             type = "latex", digits = 0), include.rownames = F)





# # PRESENTATION ------------------------------------------------------------
# 
# tab_reg <- tibble("Date" = time(tsl$`Regions Total/AF`),
#                   "Europe" = tsl$`Regions Total/EU`,
#                   "North America" = tsl$`Regions Total/NA`,
#                   "East Asia" = tsl$`Regions Total/EA`,
#                   "Africa and Middle East"  = tsl$`Regions Total/AF`,
#                   "Australia and Oceania" = tsl$`Regions Total/AO`,
#                   "Central Asia" = tsl$`Regions Total/CA`,
#                   "Latin America" = tsl$`Regions Total/LA`,
#                   "South Asia" = tsl$`Regions Total/SA`) %>%
#   gather(key = Region, value = Exports, -Date) %>%
#   mutate(Exports = as.numeric(Exports)/1e+9) %>% 
#   mutate(Region = fct_reorder(factor(Region), Exports, last, .desc = T))
# 
# tab_cat <- tibble("Date" = time(tsl$`Goods Total Lvl 1/01`),
#                   "Agricultural Products" = tsl$`Goods Total Lvl 1/01`,
#                   "Energy Source" = tsl$`Goods Total Lvl 1/02`,
#                   "Textiles" = tsl$`Goods Total Lvl 1/03`,
#                   "Graphical Products" = tsl$`Goods Total Lvl 1/04`,
#                   "Leather, Rubber, Plastics" = tsl$`Goods Total Lvl 1/05`,
#                   "Chemicals and Pharmaceuticals" = tsl$`Goods Total Lvl 1/06`,
#                   "Stones and Earth" = tsl$`Goods Total Lvl 1/07`,
#                   "Metals" = tsl$`Goods Total Lvl 1/08`,
#                   "Machines and Electronics" = tsl$`Goods Total Lvl 1/09`,
#                   "Vehicles" = tsl$`Goods Total Lvl 1/10`,
#                   "Precision Instruments" = tsl$`Goods Total Lvl 1/11`,
#                   "Various Goods" = tsl$`Goods Total Lvl 1/12`) %>%
#   gather(key = Categories, value = Exports, -Date) %>%
#   mutate(Exports = as.numeric(Exports)/1e+9) %>% 
#   mutate(Categories = fct_reorder(factor(Categories), Exports, last, .desc = T))
# 
# tot <- tab_cat %>% spread(key = Categories, value = Exports) %>% filter(Date >= 2018 & Date < 2019)
# round(colSums(tot[,-1])/sum(tot[,-1])*100,1)
# tot <- tab_reg %>% spread(key = Region, value = Exports) %>% filter(Date >= 2018 & Date < 2019)
# round(colSums(tot[,-1])/sum(tot[,-1])*100,1)
# 
# ggplot(tab_reg, aes(x = Date, y = Exports)) +
#   geom_area(aes(fill = Region), alpha=0.75) +
#   scale_x_continuous(breaks = seq(1990,2015,5)) +
#   coord_cartesian(expand = FALSE) +
#   scale_y_continuous() +
#   labs(x = NULL, y = "Exports (nominal, in billion CHF)") +
#   scale_fill_manual(values = bpy.colors(9)[-9]) +
#   theme_bw(base_size = 9) +
#   theme(legend.position="right",
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.minor.y = element_blank()) +
#   guides(fill = guide_legend(title = NULL))
# 
# ggsave("tex/fig/fig_area_reg_pres.pdf", device = "pdf",
#        width = 18, height = 8, units = "cm")
# 
# ggplot(tab_cat, aes(x = Date, y = Exports)) +
#   geom_area(aes(fill = Categories), alpha=0.75) +
#   scale_x_continuous(breaks = seq(1990,2015,5)) +
#   scale_y_continuous() +
#   coord_cartesian(expand = FALSE) +
#   labs(x = NULL, y = "Exports (nominal, in billion CHF)") +
#   scale_fill_manual(values = bpy.colors(12)) +
#   theme_bw(base_size = 9) +
#   theme(legend.position="right",
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.minor.y = element_blank()) +
#   guides(fill = guide_legend(title = NULL))
# 
# ggsave("tex/fig/fig_area_cat_pres.pdf", device = "pdf",
#        width = 18, height = 8, units = "cm")
