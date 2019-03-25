
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
library(MASS)


# Model
source("lib/functions_model_electricity.R")

# Data
load("dat/countries.rda")
load("dat/tradegts_reduced1.Rdata")
load("dat/tradehts_reduced.Rdata")
load("dat/tradegts_imp.Rdata")
load("dat/tradegts_reduced2_imp.Rdata")
# tsl_imp <- as.list(aggts(tradegts_imp))
tsl <- as.list(aggts(tradegts_reduced1))

# Options
options(scipen=10)



# 1. SEASONALITIES --------------------------------------------------------

# Data
yr <- 2018
training <- window(tradegts_reduced1, end = yr-1/12)
fc_td <- forecast(window(tradehts_reduced$reg, end = yr-1/12), h = 12, fmethod = "arima")

# BSR
fc_bsr <- RunBSR(object = training, h = 12, fmethod = "arima", shrinkage = "none")
fcm <- as.list(aggts(fc_bsr))
fcv <- as.list(fc_bsr$var)

ts1 <- "Goods Total Lvl 1/09"
ts2 <- "AO09"

tot_m <- c(fcm[[ts1]])
aus_m <- c(fcm[[ts2]])
tot_v <- c(fcv[[ts1]])
aus_v <- c(fcv[[ts2]])

data <- bind_rows(tibble(series = factor("to the World", levels = c("to the World","to Australia")),
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
                  tibble(series = factor("to Australia", levels = c("to the World","to Australia")),
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

test <- data %>%
  gather(state,mean, -c(series,mon,min1,max1,min2,max2,min3,max3,min4,max4)) %>% 
  mutate(state = factor(recode(state, "rea" = "Realization in 2018", "avg" = "Forecast Mean in 2018")))


ggplot(test, aes(x = mon, group = series)) +
  geom_ribbon(aes(ymin = min1, ymax = max1), fill = "grey70", alpha = 0.2) +
  geom_ribbon(aes(ymin = min2, ymax = max2), fill = "grey70", alpha = 0.3) +
  geom_ribbon(aes(ymin = min3, ymax = max3), fill = "grey70", alpha = 0.4) +
  geom_ribbon(aes(ymin = min4, ymax = max4), fill = "grey70", alpha = 0.5) +
  geom_line(aes(y = mean, group = state, color = state, linetype = state)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_colour_manual("", values=c("black","blue")) +
  scale_linetype_manual("", values=c("solid","dashed")) +
  facet_grid(series ~ ., scales = "free_y") +
  ylab("Volume (in Mio. CHF)") +
  xlab(NULL) +
  theme_bw() + theme(legend.position="bottom")

ggsave("tex/fig/fig_forecast.pdf", device = "pdf",
       width = 18, height = 10, units = "cm")








# ELECTRICITY -------------------------------------------------------------

# Data
training <- window(tradegts_reduced1, end = 2002)
test <- window(tradegts_reduced1, start = 2002+1/12, end = 2002+11/12)


# BSR with and without shrinkage
shr <- which(names(as.list(aggts(training))) == "Goods Total Lvl 1/02")
fc_bsr_shr <- RunBSR_electric(object = training, 
                              h = 11, fmethod = "ets",
                              shrinkage = "none", 
                              series_to_be_shrunk = shr)
fc_bsr_non <- RunBSR_electric(object = training, h = 11, 
                              fmethod = "ets",
                              shrinkage = "none")

# evaluation
acc_bsr_shr <- t(accuracy(fc_bsr_shr, test))
acc_bsr_non <- t(accuracy(fc_bsr_non, test))
acc_bsr_shr[,"RMSE"]^2/acc_bsr_non[,"RMSE"]^2


# get mean and variances
shr_m_ls <- as.list(aggts(fc_bsr_shr))
shr_v_ls <- as.list(fc_bsr_shr$var)
non_m_ls <- as.list(aggts(fc_bsr_non))
non_v_ls <- as.list(fc_bsr_non$var)

# define series
ts1 <- "Goods Total Lvl 1/02"

# extract series
m0 <- window(tsl[[ts1]], start = 2002, end = 2002)
shr_m <- ts(c(m0,shr_m_ls[[ts1]]), start = 2002, frequency = 12)
shr_v <- ts(c(0,shr_v_ls[[ts1]]), start = 2002, frequency = 12)
non_m <- ts(c(m0,non_m_ls[[ts1]]), start = 2002, frequency = 12)
non_v <- ts(c(0,non_v_ls[[ts1]]), start = 2002, frequency = 12)


dat <- rbind(as_tibble(cbind("date" = time(window(tsl[[ts1]], start = 2002-4/12, end = 2002 + 11/12)),
                             "Realization" = window(tsl[[ts1]], start = 2002, end = 2002 + 11/12),
                             "Historical Data" = window(tsl[[ts1]], start = 2002-4/12, end = 2002),
                             "Reconciled Forecast Mean" = shr_m,
                             "min1" = shr_m - 2.58*sqrt(shr_v),
                             "max1" = shr_m + 2.58*sqrt(shr_v),
                             "min2" = shr_m - 1.96*sqrt(shr_v),
                             "max2" = shr_m + 1.96*sqrt(shr_v),
                             "min3" = shr_m - 1.64*sqrt(shr_v),
                             "max3" = shr_m + 1.64*sqrt(shr_v))) %>%
               gather(series,value, -c(date,min1, max1,min2,max2,min3,max3)) %>% 
               add_column(fct = factor("weighted", levels = c("weighted","unweighted"))) %>% 
               mutate(series = factor(series, levels = c("Historical Data","Realization","Reconciled Forecast Mean"))),
             as_tibble(cbind("date" = time(window(tsl[[ts1]], start = 2002-4/12, end = 2002 + 11/12)),
                             "Realization" = window(tsl[[ts1]], start = 2002, end = 2002 + 11/12),
                             "Historical Data" = window(tsl[[ts1]], start = 2002-4/12, end = 2002),
                             "Reconciled Forecast Mean" = non_m,
                             "min1" = non_m - 2.58*sqrt(non_v),
                             "max1" = non_m + 2.58*sqrt(non_v),
                             "min2" = non_m - 1.96*sqrt(non_v),
                             "max2" = non_m + 1.96*sqrt(non_v),
                             "min3" = non_m - 1.64*sqrt(non_v),
                             "max3" = non_m + 1.64*sqrt(non_v))) %>%
               gather(series,value, -c(date,min1, max1,min2,max2,min3,max3)) %>% 
               add_column(fct = factor("unweighted", levels = c("weighted","unweighted"))) %>% 
               mutate(series = factor(series, levels = c("Historical Data","Realization","Reconciled Forecast Mean"))))



ggplot(dat, aes(x = date)) +
  geom_ribbon(aes(ymin = min1, ymax = max1), fill = "grey70", alpha = 0.2) +
  geom_ribbon(aes(ymin = min1, ymax = max1), fill = "grey70", alpha = 0.2) +
  geom_ribbon(aes(ymin = min2, ymax = max2), fill = "grey70", alpha = 0.3) +
  geom_ribbon(aes(ymin = min2, ymax = max2), fill = "grey70", alpha = 0.3) +
  geom_ribbon(aes(ymin = min3, ymax = max3), fill = "grey70", alpha = 0.4) +
  geom_ribbon(aes(ymin = min3, ymax = max3), fill = "grey70", alpha = 0.4) +
  geom_line(aes(y = value, color = series, linetype = series)) +
  scale_x_continuous(expand = c(0,0),
                     breaks = c(2001.75,2002,2002.25,2002.5,2002.75),
                     labels = c("Oct","Jan","Apr","Jul","Oct"),
                     minor_breaks = seq(2001.75,2003,1/12)) +
  geom_vline(mapping=aes(xintercept=2002), color="black", size = 0.25) +
  scale_colour_manual("Exports of Energy Sources", values = c("blue","blue","black")) +
  facet_grid(. ~ fct) +
  scale_linetype_manual("Exports of Energy Sources", values = c("solid","dashed","solid")) +
  ylab("Volume (in Mio. CHF)") +
  xlab(NULL) +
  theme_bw() + theme(legend.position="bottom")


ggsave("tex/fig/fig_electricity.pdf", device = "pdf",
       width = 18, height = 8, units = "cm")


