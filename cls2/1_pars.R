
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

# Packages
library(hts)
library(forecast)
library(data.table)
library(doParallel)
library(foreach)
library(Matrix)

# Functions
source("lib/functions_model_lite.R")

# Data
load("dat/tradehts_reduced.Rdata")
load("dat/tradegts_reduced2.Rdata")


# Define aggregation and forecasting methods over which to loop
fmethods <- c("ets" = "ets", "arima" = "arima", "rw" = "rw")
fdate <- 1995:2018; names(fdate) = fdate
horizons <- seq(1,3); names(horizons) = horizons # h = 1 means we have data until just before fdate


