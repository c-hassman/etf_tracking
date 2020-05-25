rm(list = ls())
library(readxl)
library(tidyverse)

#-----------------Import Data from Excel and order------------#
WEAT <- read_excel("G:/My Drive/3_Massa Research/Neff Paper/Working_Folder/Data_Update.xlsx", 
                   sheet = "WEAT", col_types = c("date", 
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
WEAT <- WEAT[order(WEAT$DATE),] #order by date
WEAT$asset_basket <- (WEAT$`F1(.35)` * 0.35) + (WEAT$`F2(.3)` * 0.3) + (WEAT$`F3(.35)` * 0.35) #construct asset baskets

#-----------------------Calculate Returns and Errors--------------#
WEAT$per_asset_return <- log(WEAT$asset_basket/lag(WEAT$asset_basket)) #calculate percent asset return 
WEAT$per_ETF_return <- log(WEAT$WEAT_MID/lag(WEAT$WEAT_MID)) #calculate percent ETF return
WEAT$per_NAV_return <- log(WEAT$WEAT_NAV/lag(WEAT$WEAT_NAV)) #calculate percent NAV return
WEAT$etf_asset_error <- WEAT$per_ETF_return - WEAT$per_asset_return #calculate percent ETF return
WEAT <- na.omit(WEAT) #Omit Rows with NAs

#---------------------Exploratory Plots--------------------------#
qplot(WEAT$DATE, (WEAT$etf_asset_error * 100), geom = 'line') + theme_bw() +
  ggtitle("Daily Return Error: WEAT ETF - Asset Basket") + ylab("Error (%)") +
  xlab("Date")
  