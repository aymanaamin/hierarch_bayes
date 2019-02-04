
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
                         min = tot_m - 2*sqrt(tot_v),
                         max = tot_m + 2*sqrt(tot_v),
                         mon = as_factor(month.abb,ordered = T)),
                  tibble(series = factor("Australia", levels = c("World","Australia")),
                         rea = c(window(tsl[[ts2]], start = yr, end = yr + 11/12)),
                         avg = aus_m,
                         min = aus_m - 2*sqrt(aus_v),
                         max = aus_m + 2*sqrt(aus_v),
                         mon = as_factor(month.abb,ordered = T)))


ggplot(data, aes(x = mon, group = series)) +
  geom_ribbon(aes(ymin = min, ymax = max), fill = "grey70", alpha = 0.5) +
  geom_line(aes(y = avg), color = "black") +
  geom_line(aes(y = rea), color = "red") +
  facet_grid(series ~ ., scales = "free_y") +
  ylab("Volume (in Mio. CHF)") +
  xlab(NULL) +
  theme_bw()












# 
# tot <- matrix(window(tsl$`Goods Total Lvl 2/011`, start = 2012),nrow =12)*1e+3
# aus <- matrix(window(tsl$`Goods Lvl 2 per Country/AOAU011`, start = 2012),nrow =12)*1e+3
# 
# data <- rbind(tibble(series = factor("World"),
#                      avg = apply(tot,1,FUN=mean),
#                      min = apply(tot,1,FUN=min),
#                      max = apply(tot,1,FUN=max),
#                      mon = as_factor(month.abb,ordered = T)),
#               tibble(series = "Australia",
#                      avg = apply(aus,1,FUN=mean),
#                      min = apply(aus,1,FUN=min),
#                      max = apply(aus,1,FUN=max),
#                      mon = as_factor(month.abb,ordered = T)))
# 
# 
# ggplot(data, aes(x = mon, group = series)) +
#   geom_ribbon(aes(ymin = min, ymax = max), fill = "grey70", alpha = 0.5) +
#   geom_line(aes(y = avg)) +
#   facet_grid(series ~ ., scales = "free_y") +
#   ylab("Volume (in Mio. CHF)") +
#   xlab(NULL) +
#   theme_bw()
# ggsave("tex/fig/fig_season.pdf", device = "pdf",
#        width = 18, height = 5, units = "cm")
