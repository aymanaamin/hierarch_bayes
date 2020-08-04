
# 0. PRELIMINARIES ---------------------------------------------------------

rm(list = ls())

# Packages
library(hts)
library(forecast)
library(data.table)
library(doParallel)
library(foreach)
library(Matrix)
library(bsr)


# Data
load("dat/tradehts_reduced.Rdata")
load("dat/tradegts_reduced2.Rdata")


# Define aggregation and forecasting methods over which to loop
fmethods <- c("ets" = "ets", "arima" = "arima", "rw" = "rw")
fdate <- seq(2019,2019+11/12,1/12); names(fdate) = fdate
horizon <- 36


# Pars
cores_l <- 24
cores_s <- 12
cores_s_euler <- 7
