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
#------ETF, Asset Basket Error
qplot(WEAT$DATE, (WEAT$etf_asset_error * 100), geom = 'line') + theme_bw() +
  ggtitle("Daily Return Error: WEAT ETF - Asset Basket") + ylab("Error (%)") +
  xlab("Date")
#-----ETF and Asset Basket 
etfcolor <- "black"
assetcolor <- "darkgrey"
coeff <- WEAT$asset_basket[1] / WEAT$WEAT_MID[1]
ggplot(data = WEAT, aes(x = DATE)) +
  geom_line(aes(y = WEAT_MID), color = etfcolor) +
  geom_line(aes(y = asset_basket / coeff), color = assetcolor) +
  scale_y_continuous(
    name = 'ETF Price ($)', 
    sec.axis = sec_axis(~.*coeff, name = "Asset Basket Price (cents per bushel)")
  ) + theme_bw() + ggtitle('WEAT ETF and Asset Basket Price')
#----Premium/Discount to NAV
qplot(WEAT$DATE, ((WEAT$WEAT_MID - WEAT$WEAT_NAV)/WEAT$WEAT_NAV * 100), geom = 'line') +
  theme_bw() + ylab("Premium/Discount (%)") + xlab("Date") + ggtitle("WEAT Premium/Discount to NAV")
