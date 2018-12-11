
# 0. PRELIMINARIES --------------------------------------------------------

rm(list = ls())

# Packages
library(hts)
library(forecast)
library(data.table)
library(ggplot2)
library(gridExtra)
library(tidyverse)

# Functions
source("lib/functions_evaluation.R")

# Data & Metadata
load("dat/results_backtest.Rdata")
load("dat/tradegts.Rdata")
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

# 1. by reconciliation method
# x = year, y = accuracy, lines = reconciliation, facets = horizon
tab <- get_table(recon = names(results),
                 fmethod = c("arima"),
                 fdate = seq(2000,2014,1),
                 horizon = 1:3,
                 measure = "MASE",
                 levels = c("Total"))

ggplot(data = tab, aes(Date, Total, group = Reconciliation)) + 
  geom_line(aes(colour = Reconciliation)) +
  facet_grid(Horizon ~ .) +
  theme_bw() +
  theme(legend.position="bottom")


# 2. by reconciliation method
# x = year, y = accuracy, lines = reconciliation, facets = horizon
tab <- get_table(recon = c("bu","mo_cat","tdfp_cat","unrecon",
                           "ols","wls_cat","nseries"),
                 fmethod = c("arima"),
                 fdate = seq(2000,2015,1),
                 horizon = 1:3,
                 measure = "MASE",
                 levels = c("Total"))


ggplot(data = tab, aes(Date, Total, col = Reconciliation, group = Reconciliation)) + 
  geom_line() +
  facet_grid(Horizon ~ Category) +
  scale_x_discrete(breaks = seq(2000, 2018, by = 4)) +
  theme_bw() +
  theme(legend.position="bottom") +
  guides(color=guide_legend(nrow=4))


# 3. by forecasting method
# x = year, y = accuracy, lines = fmethod, facets = horizon
tab <- get_table(recon = "ols",
                 fmethod = c("ets","rw","arima"),
                 fdate = seq(2000,2014,1),
                 horizon = 1:3,
                 measure = "MASE",
                 levels = c("Total"))

ggplot(data = tab, aes(x=Date, y=Total, group = Forecast)) + 
  geom_line(aes(colour = Forecast)) +
  facet_grid(Horizon ~ .) +
  theme_bw() +
  theme(legend.position="bottom")



# by hierarchical level


# by category


# by geographical region
