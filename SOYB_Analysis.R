rm(list=ls())
library(readxl)
library(tidyverse)
library(ggthemes)

SOYB <- read_excel("G:/My Drive/3_Massa Research/Neff Paper/Working_Folder/Data_Update.xlsx", 
                   sheet = "SOYB", col_types = c("date", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric"))
SOYB <- SOYB[order(SOYB$DATE),]
SOYB$asset_basket <- (SOYB$`F1(.35)` * 0.35) + (SOYB$`F2(.3)` * 0.30) + (SOYB$`F3(.35)` * 0.35)

#-------------------------Calculate Returns and Errors------------------------------#
SOYB$per_asset_return <- log(SOYB$asset_basket / lag(SOYB$asset_basket))
SOYB$per_ETF_return <- log(SOYB$SOYB_MID / lag(SOYB$SOYB_MID))
SOYB$per_NAV_return <- log(SOYB$SOYB_NAV / lag(SOYB$SOYB_NAV))
SOYB$etf_asset_error <- SOYB$per_ETF_return - SOYB$per_asset_return
SOYB <- na.omit(SOYB)
#---------------------Exploratory Plots--------------------------------------#
qplot(SOYB$DATE, (SOYB$etf_asset_error * 100), geom = 'line') + theme_bw() +
  ggtitle("Daily Return Error: SOYB ETF - Asset Basket") + ylab("Error (%)") +
  xlab("Date")

