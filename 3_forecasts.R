
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
source("lib/functions_model_electricity.R")

# Data
load("dat/countries.rda")
load("dat/tradegts_reduced1.Rdata")
load("dat/tradehts_reduced.Rdata")
load("dat/tradegts_imp.Rdata")
load("dat/tradegts_reduced2_imp.Rdata")
tsl_imp <- as.list(aggts(tradegts_imp))
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









# AIRPLANES ---------------------------------------------------------------

# Data
training <- window(tradegts_reduced2_imp, start = 2010, end = 2018-1/12)
test <- window(tradegts_reduced2_imp, start = 2018, end = 2018)


# BSR with and without shrinkage
shr <- which(names(as.list(aggts(training))) == "Goods Total Lvl 1/02")
fc_bsr_shr <- RunBSR(object = training, h = 1, fmethod = "arima",
                     shrinkage = "none", series_to_be_shrunk = shr)
fc_bsr_non <- RunBSR(object = training, h = 1, fmethod = "arima",
                     shrinkage = "none", series_to_be_shrunk = )

# Evaluation
acc_bsr_shr <- t(accuracy(fc_bsr_shr$forecast, test))
acc_bsr_non <- t(accuracy(fc_bsr_non$forecast, test))




# ELECTRICITY -------------------------------------------------------------

# Data
training <- window(tradegts_reduced1, end = 2002-1/12)
test <- window(tradegts_reduced1, start = 2002, end = 2002+11/12)


# BSR with and without shrinkage
shr <- which(names(as.list(aggts(training))) == "Goods Total Lvl 1/02")
fc_bsr_shr <- RunBSR(object = training, h = 12, fmethod = "arima",
                     shrinkage = "none", series_to_be_shrunk = shr)
fc_bsr_non <- RunBSR(object = training, h = 12, fmethod = "arima",
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


# Define Shrinkage
# ea <- toupper(c("at","be","cy","ee","fi","fr","de","gr","ie","it","lv","lt",
#         "lu","mt","nl","pt","sk","si","es"))
# down <- which(!(substr(colnames(aggts(training)),3,4) %in% ea))
# down <- which(substr(colnames(aggts(training)),1,2) != "EU")
