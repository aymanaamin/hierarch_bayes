
# 0. PRELIMINARIES --------------------------------------------------------

rm(list = ls())

# Packages
library(hts)
library(forecast)
library(data.table)
library(ggplot2)


# Functions
source("lib/functions.R")

# Data & Metadata
load("dat/results_backtest.Rdata")
load("dat/countries.rda")

# Options
options(scipen=10)




# INSPECT DATA ------------------------------------------------------------

# drill into lists
head(t(results$mo_cat$ets$`2000`$`1`),9)
head(t(results$bu$ets$`2000`$`1`))

# get the entire accuracy test
test <-  get_values("bu","ets","2000","1")

# get selective information
tab <- get_table(recon = "bu",
                 fmethod = c("ets","rw"),
                 fdate = seq(2000,2006,2),
                 horizon = 1,
                 measure = "MASE",
                 levels = c("Total"))





# GRAPHICAL VISUALIZATION -------------------------------------------------

# by aggregation method



# by forecasting horizon


# x = year, y = accuracy
tab <- get_table(recon = "bu",
                 fmethod = c("ets","rw","arima"),
                 fdate = seq(2000,2014,2),
                 horizon = 1,
                 measure = "MASE",
                 levels = c("Total"))

p <- ggplot(data = tab, aes(Date, Total)) + geom_point()
p + facet_grid(Forecast ~ .)


# by forecasting method
dat <- sapply(c("rw","ets","arima"), function(x) get_values("bu",x,"2000","1"))
p <- ggplot(data = mpg, aes(displ, cty)) + geom_point()
p + facet_grid(. ~ drv)

# by hierarchical level



# by category


# by geographical region
