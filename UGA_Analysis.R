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
#------ETF, Asset Basket Error
qplot(UGA$DATE, (UGA$etf_asset_error * 100), geom = 'line') + theme_bw() +
  ggtitle("Daily Return Error: UGA ETF - Asset Basket") + ylab("Error (%)") +
  xlab("Date")
#-----ETF and Asset Basket 
etfcolor <- "black"
assetcolor <- "darkgrey"
coeff <- UGA$Futures[1] / UGA$UGA_MID[1]
ggplot(data = UGA, aes(x = DATE)) +
  geom_line(aes(y = UGA_MID), color = etfcolor) +
  geom_line(aes(y = Futures / coeff), color = assetcolor) +
  scale_y_continuous(
    name = 'ETF Price ($)',
    sec.axis = sec_axis(~.*coeff, name = "Asset Basket Price (Dollars per Gallon)")
) + theme_bw() + ggtitle("UGA ETF and Asset Basket Price") + xlab("Date") 
#----Premium/Discount to NAV
qplot(UGA$DATE, ((UGA$UGA_MID - UGA$UGA_NAV)/UGA$UGA_NAV * 100), geom = 'line') +
  theme_bw() + ylab("Premium/Discount (%)") + xlab("Date") + ggtitle("UGA Premium/Discount to NAV")

#-------------------OLS-----------------------------------------------#
#--------Simple
simple <- lm(per_asset_return ~ per_ETF_return, data = UGA)
summary(simple)

#--------Dummy
model <- lm(abs(UGA$etf_asset_error) ~ abs(UGA$per_ETF_return) + UGA$`RB Day Before Roll` + UGA$`RB Day After Roll` +
              UGA$`RB Feb` + UGA$`RB Mar` + UGA$`RB April` + UGA$`RB May` + UGA$`RB June` + UGA$`RB July` +
              UGA$`RB Aug` + UGA$`RB Sept` + UGA$`RB Oct` + UGA$`RB Nov` + UGA$`RB Dec` + UGA$`RB 2013` +
              UGA$`RB 2014` + UGA$`RB 2015` + UGA$`RB 2016` + UGA$`RB 2017` + UGA$`RB 2018` + UGA$`RB 2019` +
              UGA$`RB 2020` + UGA$`RB STEO` + UGA$`RB Drilling Prod` + UGA$`RB Petro Supply/Prod` + UGA$`RB Annual Energy Outlook`)
summary(model)              
              
            
              