rm(list = ls())
library(readxl)
library(tidyverse)
library(ggthemes)

#-----------------Import Data from Excel and order------------#
CORN <- read_excel("G:/My Drive/3_Massa Research/Neff Paper/Working_Folder/Data_Update.xlsx", 
                   sheet = "CORN", col_types = c("date", 
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
CORN <- CORN[order(CORN$DATE),] # order by date
CORN$asset_basket <- (CORN$`F1(.35)` * 0.35) + (CORN$`F2(.3)` * 0.3) + (CORN$`F3(.35)` * 0.35) #reconstruct asset basket

#-----------------------Calculate Returns and Errors--------------#
CORN$per_asset_return <- log(CORN$asset_basket/lag(CORN$asset_basket)) # calculate percent asset basket return
CORN$per_ETF_return <- log(CORN$CORN_MID/lag(CORN$CORN_MID)) #calculate percent ETF return
CORN$per_NAV_return <- log(CORN$CORN_NAV/lag(CORN$CORN_NAV)) #calculate percent NAV return
CORN$etf_asset_error <- CORN$per_ETF_return - CORN$per_asset_return #calculate error between ETF and Asset
CORN <- na.omit(CORN) #Omit the rows with NAs, which are the final roll days

#-----------------------Exploratory Plots-----------------------#
#------ETF, Asset Basket Error
qplot(CORN$DATE, (CORN$etf_asset_error * 100), geom = 'line') + theme_bw() +
  ggtitle('Daily Return Error: CORN ETF - Asset Basket') +
  ylab('Error (%)') + xlab("Date")
#-----ETF and Asset Basket 
etfcolor <- "black"
assetcolor <- "darkgrey"
coeff <- CORN$asset_basket[1] / CORN$CORN_MID[1]
ggplot(data = CORN, aes(x = DATE)) +
  geom_line(aes(y = CORN_MID), color = etfcolor) +
  geom_line(aes(y = asset_basket / coeff), color = assetcolor) +
  scale_y_continuous(
    name = "ETF Price ($)", 
    sec.axis = sec_axis(~.*coeff, name = "Asset Basket Price (cents per bushel")
  ) + theme_bw() + ggtitle("CORN ETF and Asset Basket Price") + xlab("Date") 
