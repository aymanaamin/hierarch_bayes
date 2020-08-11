
rm(list = ls())

# Packages
library(hts)
library(tidyverse)
library(bsr)

# Data
load("dat/countries.rda")
load("dat/tradegts_reduced1.Rdata")
load("dat/tradehts_reduced.Rdata")




# ELECTRICITY -------------------------------------------------------------

# data
load("dat/tradegts_reduced1.Rdata")
tradegts_reduced1$bts <- tradegts_reduced1$bts/1e+6
test <- window(tradegts_reduced1, start = 2002, end = 2003)

# backtest for electricity
dates <- 0
bt <- lapply(dates, function(dx){
  
  training <- window(tradegts_reduced1, end = 2002+dx/12)
  
  hx <- 12
  ix <- which(colnames(aggts(training)) == "Goods Total Lvl 1/02")
  fx <- as.numeric(tail(aggts(training)[,"Goods Total Lvl 1/02"],1))
  
  fc_bsr_shr <- bsr(object = training, 
                    h = hx,
                    exogen = list("Goods Total Lvl 1/02" = rep(fx,hx)),
                    fmethod = "ets",
                    shrinkage = which(colnames(aggts(training)) =="Goods Total Lvl 1/02"))
  
  fc_bsr_non <- bsr(object = training,
                    h = hx, 
                    exogen = list("Goods Total Lvl 1/02" = rep(fx,hx)),
                    fmethod = "ets")
  
  fc_bsr_non2 <- bsr(object = training,
                     h = hx, 
                     fmethod = "ets")
  
  out <- cbind(aggts(fc_bsr_shr)[,11],
               aggts(fc_bsr_non)[,11],
               aggts(fc_bsr_non2)[,11])
  
  colnames(out) <- c("shr","rwf","base")
  
  return(out)
  
}); names(bt) <- 2002+dates/12


save(bt, file = "out/forecast_shrinkage.Rdata")


# # evaluation
# acc_bsr_shr <- t(accuracy(fc_bsr_shr, test))
# acc_bsr_non <- t(accuracy(fc_bsr_non, test))
# hist(t(log(acc_bsr_shr[,"RMSE"]/acc_bsr_non[,"RMSE"])), breaks = 100)


tab <- window(cbind(window(aggts(test)[,11], start = 2002),
                    aggts(window(tradegts_reduced1, end = 2001+11/12))[,11],
                    bt$`2002`[,1],
                    bt$`2002`[,2],
                    bt$`2002`[,3]), start = 2000, end = 2003)

tab[which(time(tab) == 2002),] <- tab[which(time(tab) == 2002),1]

tib <- tibble("date" = as.numeric(time(tab)),
              "true" = tab[,1],
              "hist" = tab[,2],
              "bsr" = tab[,3],
              "rwf" = tab[,4],
              "no" = tab[,5]) %>% 
  gather("series","value",-date) %>% 
  mutate(series = factor(series,
                         levels = c("hist","true","no","rwf","bsr"),
                         labels = c("Historical Data",
                                    "Realization",
                                    "(1) Uninformed Forecast",
                                    "(2) Informed Forecast",
                                    "(3) Informed Forecast & Shrinkage")))

# RColorBrewer::brewer.pal(5,"Blues")

ggplot(tib, aes(x = date)) +
  geom_rect(mapping=aes(xmin=2002, xmax=Inf, ymin=-Inf, ymax=Inf), fill="grey98", alpha = 0.1) +
  geom_line(aes(y = value, color = series, linetype = series)) +
  geom_hline(aes(yintercept = 0), color = "black", lwd = 0.2) +
  scale_y_continuous(limits = c(0,NA), expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(2000,2003,1/12,),
                     labels = c(rep("",6), "2000", rep("",5),
                                rep("",6), "2001", rep("",5),
                                rep("",6), "2002", rep("",6)),
                     minor_breaks = seq(2001,2003,1/12)) +
  scale_colour_manual(NULL, values = c("black","black","#6BAED6","#3182BD","#08519C")) +
  scale_linetype_manual(NULL, values = c("solid","dashed","dotdash","dotdash","dotdash")) +
  ylab("Export Volume (in Mio. CHF)") +
  xlab(NULL) +
  theme_minimal() + 
  theme(legend.position = c(0.21,0.75),
        legend.title=element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        legend.background=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_line(color = c("black",rep(NA,11),"black",rep(NA,11),"black",rep(NA,11))))


ggsave("tex/fig/fig_electricity.pdf", device = "pdf",
       width = 18, height = 7, units = "cm")



# # get mean and variances
# shr_m_ls <- as.list(aggts(fc_bsr_shr))
# shr_v_ls <- as.list(fc_bsr_shr$var)
# non_m_ls <- as.list(aggts(fc_bsr_non))
# non_v_ls <- as.list(fc_bsr_non$var)
# 
# # define series
# ts1 <- "Goods Total Lvl 1/02"
# 
# # extract series
# m0 <- window(tsl[[ts1]], start = 2002, end = 2002)
# shr_m <- ts(c(m0,shr_m_ls[[ts1]]), start = 2002, frequency = 12)
# shr_v <- ts(c(0,shr_v_ls[[ts1]]), start = 2002, frequency = 12)
# non_m <- ts(c(m0,non_m_ls[[ts1]]), start = 2002, frequency = 12)
# non_v <- ts(c(0,non_v_ls[[ts1]]), start = 2002, frequency = 12)
# 
# 
# dat <- rbind(as_tibble(cbind("date" = time(window(tsl[[ts1]], start = 2002-4/12, end = 2002 + 11/12)),
#                              "Realization" = window(tsl[[ts1]], start = 2002, end = 2002 + 11/12),
#                              "Historical Data" = window(tsl[[ts1]], start = 2002-4/12, end = 2002),
#                              "Reconciled Forecast Mean" = shr_m,
#                              "min1" = shr_m - 2.58*sqrt(shr_v),
#                              "max1" = shr_m + 2.58*sqrt(shr_v),
#                              "min2" = shr_m - 1.96*sqrt(shr_v),
#                              "max2" = shr_m + 1.96*sqrt(shr_v),
#                              "min3" = shr_m - 1.64*sqrt(shr_v),
#                              "max3" = shr_m + 1.64*sqrt(shr_v))) %>%
#                gather(series,value, -c(date,min1, max1,min2,max2,min3,max3)) %>% 
#                add_column(fct = factor("weighted", levels = c("weighted","unweighted"))) %>% 
#                mutate(series = factor(series, levels = c("Historical Data","Realization","Reconciled Forecast Mean"))),
#              as_tibble(cbind("date" = time(window(tsl[[ts1]], start = 2002-4/12, end = 2002 + 11/12)),
#                              "Realization" = window(tsl[[ts1]], start = 2002, end = 2002 + 11/12),
#                              "Historical Data" = window(tsl[[ts1]], start = 2002-4/12, end = 2002),
#                              "Reconciled Forecast Mean" = non_m,
#                              "min1" = non_m - 2.58*sqrt(non_v),
#                              "max1" = non_m + 2.58*sqrt(non_v),
#                              "min2" = non_m - 1.96*sqrt(non_v),
#                              "max2" = non_m + 1.96*sqrt(non_v),
#                              "min3" = non_m - 1.64*sqrt(non_v),
#                              "max3" = non_m + 1.64*sqrt(non_v))) %>%
#                gather(series,value, -c(date,min1, max1,min2,max2,min3,max3)) %>% 
#                add_column(fct = factor("unweighted", levels = c("weighted","unweighted"))) %>% 
#                mutate(series = factor(series, levels = c("Historical Data","Realization","Reconciled Forecast Mean"))))
# 
# dat[,c("min1","min2","min3","max1","max2","max3","value")] <- 
#   dat[,c("min1","min2","min3","max1","max2","max3","value")]/1e+6
# 
# ggplot(dat, aes(x = date)) +
#   geom_ribbon(aes(ymin = min1, ymax = max1), fill = "grey70", alpha = 0.2) +
#   geom_ribbon(aes(ymin = min1, ymax = max1), fill = "grey70", alpha = 0.2) +
#   geom_ribbon(aes(ymin = min2, ymax = max2), fill = "grey70", alpha = 0.3) +
#   geom_ribbon(aes(ymin = min2, ymax = max2), fill = "grey70", alpha = 0.3) +
#   geom_ribbon(aes(ymin = min3, ymax = max3), fill = "grey70", alpha = 0.4) +
#   geom_ribbon(aes(ymin = min3, ymax = max3), fill = "grey70", alpha = 0.4) +
#   geom_line(aes(y = value, color = series, linetype = series)) +
#   scale_x_continuous(expand = c(0,0),
#                      breaks = c(2001.75,2002,2002.25,2002.5,2002.75),
#                      labels = c("Oct","Jan","Apr","Jul","Oct"),
#                      minor_breaks = seq(2001.75,2003,1/12)) +
#   geom_vline(mapping=aes(xintercept=2002), color="grey60", lty = 1, size = 0.5) +
#   scale_colour_manual("Exports of Energy Sources", values = c("blue","blue","black")) +
#   facet_grid(. ~ fct) +
#   scale_linetype_manual("Exports of Energy Sources", values = c("solid","dashed","solid")) +
#   ylab("Volume (in Mio. CHF)") +
#   xlab(NULL) +
#   theme_bw() + theme(legend.position="bottom",
#                      panel.grid.major.x = element_blank(),
#                      panel.grid.minor.x = element_blank(),
#                      panel.grid.minor.y = element_blank())
# 
# 
# ggsave("tex/fig/fig_electricity.pdf", device = "pdf",
#        width = 18, height = 8, units = "cm")
