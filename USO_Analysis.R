rm(list=ls())
library(readxl)
library(tidyverse)
library(ggthemes)


#------------------------Load in Data from Excel------------------------------#
USO <- read_excel("G:/My Drive/3_Massa Research/Neff Paper/Working_Folder/Data_Update.xlsx", 
                   sheet = "USO", col_types = c("date", 
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
USO <- USO[order(USO$DATE),]

#--------------------Calculate Returns and Errors ---------------------------#
USO$per_asset_return <- log(USO$Futures/lag(USO$Futures))
USO$per_ETF_return <- log(USO$USO_MID /lag(USO$USO_MID))
USO$per_NAV_return <- log(USO$USO_NAV/lag(USO$USO_NAV))
USO$etf_asset_error <- USO$per_ETF_return - USO$per_asset_return
USO <- na.omit(USO)

#---------------------Exploratory Plots--------------------------------------#
#------ETF, Asset Basket Error
qplot(USO$DATE, (USO$etf_asset_error * 100), geom = 'line') + theme_bw() +
  ggtitle("Daily Return Error: USO ETF - Asset Basket") + ylab("Error (%)") +
  xlab("Date")
#-----ETF and Asset Basket
etfcolor <- "black"
assetcolor <- "darkgrey"
coeff <- USO$Futures[1]/USO$USO_MID[1]
ggplot(data = USO, aes(x = DATE)) +
  geom_line(aes(y = USO_MID), color = etfcolor) +
  geom_line(aes(y = Futures / coeff), color = assetcolor) +
  scale_y_continuous(
    name = 'ETF Price ($)',
    sec.axis = sec_axis(~.*coeff, name = "Asset Basket ($ per Barrel")
  ) + theme_bw() + ggtitle("USO ETF and Asset Basket Price") + xlab("Date")
