
rm(list = ls())

# Packages
library(hts)
library(data.table)
library(tsfeatures)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(sp)
library(Matrix)
library(MCMCpack)

# Model
source("lib/functions_model_test.R")

# Data
load("dat/countries.rda")
load("dat/tradegts_reduced1.Rdata")
load("dat/tradehts_reduced.Rdata")
tsl <- as.list(aggts(tradegts_reduced1)/1e+6)

# Options
options(scipen=10)



# 1. SEASONALITIES --------------------------------------------------------

# Data
yr <- 2017
training <- window(tradegts_reduced1, end = yr-1/12)
# training_hts <- window(tradehts_reduced$reg, end = yr-1/12)
training$bts <- training$bts/1e+6 

# BSR
fc_bsr <- RunBSR_test(object = training, h = 12, fmethod = "arima", shrinkage = "none")
fcm <- as.list(aggts(fc_bsr$forecast))
fcv <- as.list(fc_bsr$variance)

# Others
# fc_td <- forecast(training, h = 12, method = "tdfp", fmethod = "arima")

ts1 <- "Total"
ts2 <- "Regions Total/AO"

tot_m <- c(fcm[[ts1]])
aus_m <- c(fcm[[ts2]])
tot_v <- c(fcv[[ts1]])
aus_v <- c(fcv[[ts2]])

data <- bind_rows(tibble(series = factor("World", levels = c("World","Australia")),
                         rea = c(window(tsl[[ts1]], start = yr, end = yr + 11/12)),
                         avg = tot_m,
                         min1 = tot_m - 2.58*sqrt(tot_v),
                         max1 = tot_m + 2.58*sqrt(tot_v),
                         min2 = tot_m - 1.96*sqrt(tot_v),
                         max2 = tot_m + 1.96*sqrt(tot_v),
                         min3 = tot_m - 1.64*sqrt(tot_v),
                         max3 = tot_m + 1.64*sqrt(tot_v),
                         min4 = tot_m - 1.28*sqrt(tot_v),
                         max4 = tot_m + 1.28*sqrt(tot_v),
                         mon = as_factor(month.abb,ordered = T)),
                  tibble(series = factor("Australia", levels = c("World","Australia")),
                         rea = c(window(tsl[[ts2]], start = yr, end = yr + 11/12)),
                         avg = aus_m,
                         min1 = aus_m - 2.58*sqrt(aus_v),
                         max1 = aus_m + 2.58*sqrt(aus_v),
                         min2 = aus_m - 1.96*sqrt(aus_v),
                         max2 = aus_m + 1.96*sqrt(aus_v),
                         min3 = aus_m - 1.64*sqrt(aus_v),
                         max3 = aus_m + 1.64*sqrt(aus_v),
                         min4 = aus_m - 1.28*sqrt(aus_v),
                         max4 = aus_m + 1.28*sqrt(aus_v),
                         mon = as_factor(month.abb,ordered = T)))


ggplot(data, aes(x = mon, group = series)) +
  geom_ribbon(aes(ymin = min1, ymax = max1), fill = "grey70", alpha = 0.2) +
  geom_ribbon(aes(ymin = min2, ymax = max2), fill = "grey70", alpha = 0.3) +
  geom_ribbon(aes(ymin = min3, ymax = max3), fill = "grey70", alpha = 0.4) +
  geom_ribbon(aes(ymin = min4, ymax = max4), fill = "grey70", alpha = 0.5) +
  geom_line(aes(y = avg), color = "black") +
  geom_line(aes(y = rea), color = "red", lty = 2) +
  facet_grid(series ~ ., scales = "free_y") +
  ylab("Volume (in Mio. CHF)") +
  xlab(NULL) +
  theme_bw()






# DOWNWEIGHTING SERIES ----------------------------------------------------


# Data
training <- window(tradegts_reduced1, end = 2015-1/12)
test <- window(tradegts_reduced1, start = 2015, end = 2015+11/12)
training$bts <- training$bts/1e+6
test$bts <- test$bts/1e+6

# Define Shrinkage
# ea <- toupper(c("at","be","cy","ee","fi","fr","de","gr","ie","it","lv","lt",
#         "lu","mt","nl","pt","sk","si","es"))
# down <- which(!(substr(colnames(aggts(training)),3,4) %in% ea))
down <- which(substr(colnames(aggts(training)),1,2) != "EU")


# BSR with and without shrinkage for non-EA countries
fc_bsr_shr <- RunBSR_test(object = training, h = 12, fmethod = "arima",
                          shrinkage = "none", series_to_be_shrunk = down)
fc_bsr_non <- RunBSR_test(object = training, h = 12, fmethod = "arima",
                      shrinkage = "none")

# Evaluation
acc_bsr_shr <- t(accuracy(fc_bsr_shr$forecast, test))
acc_bsr_non <- t(accuracy(fc_bsr_non$forecast, test))




# data <- bind_rows(tibble(series = factor("World", levels = c("World","Australia")),
#                          rea = c(window(tsl[[ts1]], start = yr, end = yr + 11/12)),
#                          avg = tot_m,
#                          min = tot_m - 2*sqrt(tot_v),
#                          max = tot_m + 2*sqrt(tot_v),
#                          mon = as_factor(month.abb,ordered = T)),
#                   tibble(series = factor("Australia", levels = c("World","Australia")),
#                          rea = c(window(tsl[[ts2]], start = yr, end = yr + 11/12)),
#                          avg = aus_m,
#                          min = aus_m - 2*sqrt(aus_v),
#                          max = aus_m + 2*sqrt(aus_v),
#                          mon = as_factor(month.abb,ordered = T)))
# 
# 
# ggplot(data, aes(x = mon, group = series)) +
#   geom_ribbon(aes(ymin = min, ymax = max), fill = "grey70", alpha = 0.5) +
#   geom_line(aes(y = avg), color = "black") +
#   geom_line(aes(y = rea), color = "red") +
#   facet_grid(series ~ ., scales = "free_y") +
#   ylab("Volume (in Mio. CHF)") +
#   xlab(NULL) +
#   theme_bw()



