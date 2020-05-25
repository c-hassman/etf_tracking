rm(list = ls())
library(readxl)
library(tidyverse)

#-----------------Import Data from Excel and order------------#
UGA <- read_excel("G:/My Drive/3_Massa Research/Neff Paper/Working_Folder/Data_Update.xlsx", 
                  sheet = "UGA", col_types = c("date", 
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
                                               "numeric"))
UGA <- UGA[order(UGA$DATE),]

#-----------------------Calculate Returns and Errors--------------#
UGA$per_asset_return <- log(UGA$Futures/lag(UGA$Futures)) 
UGA$per_ETF_return <- log(UGA$UGA_MID/lag(UGA$UGA_MID))
UGA$per_NAV_return <- log(UGA$UGA_NAV/lag(UGA$UGA_NAV))
UGA$etf_asset_error <- UGA$per_ETF_return - UGA$per_asset_return
UGA <- na.omit(UGA)

#----------------------Exploratory Plot--------------------------#
qplot(UGA$DATE, (UGA$etf_asset_error * 100), geom = 'line') + theme_bw() +
  ggtitle("Daily Return Error: UGA ETF - Asset Basket") + ylab("Error (%)") +
  xlab("Date")
