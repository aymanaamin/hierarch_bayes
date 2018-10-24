
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

# Packages
library(MCMCpack)
library(hts)
library(LaF)
library(data.table)
library(bsts)

# Functions
source("lib/functions.R")

# Metadata
load("data/countries.rda")


# 1. IMPORT DATA -----------------------------------------------------------

tsl <- import_data()




# 2. GENERATE HIERARCHY ----------------------------------------------------

# extract all exports from tsl
dat_exp <- do.call(cbind,tsl[which(substr(names(tsl),1,1) == "E")])
colnames(dat_exp) = substr(colnames(dat_exp),2,13)

# define hierarchy
tradegts <- gts(y = dat_exp,
               gnames = c("Regions Total", "Countries Total" ,"Goods Lvl 1","Goods Lvl 2",
                          "Goods Lvl 3","Goods Lvl 4","Goods Lvl 5","Goods Lvl 1 per Region",
                          "Goods Lvl 2 per Region","Goods Lvl 3 per Region","Goods Lvl 4 per Region",
                          "Goods Lvl 5 per Region","Goods Lvl 1 per Country","Goods Lvl 2 per Country",
                          "Goods Lvl 3 per Country","Goods Lvl 4 per Country"),
               characters = list(c(2,2), c(2,1,1,2,2)))


# aggregate
# agg_gts <- as.list(aggts(tradegts))



# 3. FORECASTS -------------------------------------------------------------

# Set up training and testing sample
data <- window(tradegts, start=1988, end=c(2015,12))
test <- window(tradegts, start=2016, end=c(2017,12))


# 3.1 Bottom-up
result_bu <- forecast(data, h=24, method="bu", parallel = T, num.cores = 4)
summary1 <- accuracy.gts(result_bu, test)
c(mean(summary1["RMSE",]),mean(summary1["MAPE",is.finite(summary1["MAPE",])]))

# ...

