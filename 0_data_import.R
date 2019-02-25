
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

# Packages
library(hts)
library(LaF)
library(data.table)

# Functions
source("lib/functions_import.R")

# Metadata
load("dat/countries.rda")



# 1. IMPORT DATA -----------------------------------------------------------

total <- import_data()
# total$value <- total$value/1e+6

total_exp <- total[substr(tsKey,1,1) == "E",]
# total_imp <- total[substr(tsKey,1,1) == "I",]
total_exp_reg <- total_exp[, .(value = sum(value)), by = list(period, tsKey = substr(tsKey,2,5))]
total_exp_cat <- total_exp[, .(value = sum(value)), by = list(period, tsKey = substr(tsKey,6,14))]

dat_exp <- dt2ts(total_exp)
# dat_imp <- dt2ts(total_imp)
colnames(dat_exp) <-  substr(colnames(dat_exp),2,14)
# colnames(dat_imp) <-  substr(colnames(dat_imp),2,14)
dat_exp_reg <- dt2ts(total_exp_reg)
dat_exp_cat <- dt2ts(total_exp_cat)





# 2. GENERATE HIERARCHY ----------------------------------------------------

# define grouped hierarchy
tradegts <- gts(y = dat_exp,
                gnames = c("Regions Total", "Countries Total" ,"Goods Total Lvl 1","Goods Total Lvl 2",
                           "Goods Total Lvl 3","Goods Total Lvl 4","Goods Total Lvl 5","Goods Lvl 1 per Region",
                           "Goods Lvl 2 per Region","Goods Lvl 3 per Region","Goods Lvl 4 per Region",
                           "Goods Lvl 5 per Region","Goods Lvl 1 per Country","Goods Lvl 2 per Country",
                           "Goods Lvl 3 per Country","Goods Lvl 4 per Country"),
                characters = list(c(2,2), c(2,1,1,2,2)))

# tradegts_imp <- gts(y = dat_imp,
                # gnames = c("Regions Total", "Countries Total" ,"Goods Total Lvl 1","Goods Total Lvl 2",
                #            "Goods Total Lvl 3","Goods Total Lvl 4","Goods Total Lvl 5","Goods Lvl 1 per Region",
                #            "Goods Lvl 2 per Region","Goods Lvl 3 per Region","Goods Lvl 4 per Region",
                #            "Goods Lvl 5 per Region","Goods Lvl 1 per Country","Goods Lvl 2 per Country",
                #            "Goods Lvl 3 per Country","Goods Lvl 4 per Country"),
                # characters = list(c(2,2), c(2,1,1,2,2)))

# define categories and countries hierarchies
tradehts <- list(cat = hts(y = dat_exp_cat, characters = c(2,1,1,2,2)),
                 reg = hts(y = dat_exp_reg, characters = c(2,2)))

# define reduced hierarchies, only until level 1
agggts_red1 <- aggts(tradegts, levels = 8)
colnames(agggts_red1) <- substr(colnames(agggts_red1), 24,40)
tradegts_reduced1 <- gts(y = agggts_red1,
                        gnames = c("Regions Total", "Goods Total Lvl 1"),
                        characters = list(2, 2))

# define reduced hierarchies, only until level 2
agggts_red2 <- aggts(tradegts, levels = 14)
colnames(agggts_red2) <- substr(colnames(agggts_red2), 25,40)
tradegts_reduced2 <- gts(y = agggts_red2,
                         gnames = c("Regions Total", "Countries Total",
                                    "Goods Total Lvl 1","Goods Total Lvl 2","Goods Lvl 1 per Region",
                                    "Goods Lvl 2 per Region","Goods Lvl 1 per Country"),
                         characters = list(c(2,2), c(2,1)))

# agggts_red2_imp <- aggts(tradegts_imp, levels = 14)
# colnames(agggts_red2_imp) <- substr(colnames(agggts_red2_imp), 25,40)
# tradegts_reduced2_imp <- gts(y = agggts_red2_imp,
#                          gnames = c("Regions Total", "Countries Total",
#                                     "Goods Total Lvl 1","Goods Total Lvl 2","Goods Lvl 1 per Region",
#                                     "Goods Lvl 2 per Region","Goods Lvl 1 per Country"),
#                          characters = list(c(2,2), c(2,1)))

tradehts_reduced <- list(cat = hts(y = aggts(tradehts$cat, levels = 2), characters = c(2,1)),
                         reg = hts(y = aggts(tradehts$reg, levels = 2), characters = c(2,2)))

rm(dat_exp,dat_imp,dat_exp_reg,dat_exp_cat,total_exp,total_exp_reg,
   total_exp_cat,agggts_red1,agggts_red2,total,agggts_red2_imp)


# 3. INSPECT DATA ----------------------------------------------------------

# aggregate
agg_gts <- as.list(aggts(tradegts))
agg_gts_red1 <- as.list(aggts(tradegts_reduced1))
agg_gts_red2 <- as.list(aggts(tradegts_reduced2))
agg_hts_reg <- as.list(aggts(tradehts$reg))
agg_hts_cat <- as.list(aggts(tradehts$cat))

# check out some series
plot(agg_gts$Total/1e+9)
plot(agg_gts$'Goods Total Lvl 1/06') # Total Pharmaceutical products, very inelastic
plot(agg_gts$'Goods Total Lvl 1/09') # Total Machinery & Equipment, depends strongly on European business cycle and CHF exchange rate
plot(agg_gts$'Goods Lvl 1 per Country/AOAU11') # Watches to Australia
plot(agg_gts$'Goods Lvl 1 per Region/AF10') # Vehicles to Region 'Africa and Middle East'

# crosscheck to ensure the different hierarchies are aggregated correctly
all.equal(agg_hts_reg$Total,agg_gts$Total)
all.equal(agg_gts$Total,agg_gts_red2$Total)
all.equal(agg_hts_reg$AOAU,agg_gts$`Countries Total/AOAU`)
all.equal(agg_hts_cat$`06`,agg_gts_red1$`Goods Total Lvl 1/06`)
all.equal(agg_hts_reg$EU,agg_gts_red2$`Regions Total/EU`)
all.equal(agg_hts_cat$`061101`,agg_gts$`Goods Total Lvl 4/061101`)

rm(agg_hts_reg,agg_hts_cat,agg_gts_red1,agg_gts_red2)



# 4. SAVE DATA ------------------------------------------------------------

save(tradegts, file = "dat/tradegts.Rdata")
save(tradegts_imp, file = "dat/tradegts_imp.Rdata")
save(tradehts, file = "dat/tradehts.Rdata")
save(tradegts_reduced1, file = "dat/tradegts_reduced1.Rdata")
save(tradegts_reduced2, file = "dat/tradegts_reduced2.Rdata")
save(tradegts_reduced2_imp, file = "dat/tradegts_reduced2_imp.Rdata")
save(tradehts_reduced, file = "dat/tradehts_reduced.Rdata")
