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
#------ETF, Asset Basket Error
qplot(SOYB$DATE, (SOYB$etf_asset_error * 100), geom = 'line') + theme_bw() +
  ggtitle("Daily Return Error: SOYB ETF - Asset Basket") + ylab("Error (%)") +
  xlab("Date")
#-----ETF and Asset Basket 
etfcolor <- "black"
assetcolor <- "darkgrey"
coeff <- SOYB$asset_basket[1]/SOYB$SOYB_MID[1]
ggplot(data = SOYB, aes(x = DATE)) +
  geom_line(aes(y = SOYB_MID), color = etfcolor) +
  geom_line(aes(y = asset_basket / coeff), color = assetcolor) +
  scale_y_continuous(
    name = 'ETF Price ($)',
    sec.axis = sec_axis(~.*coeff, name = "Asset Basket Price (cents per bushel)")
  ) + theme_bw() + ggtitle("SOYB ETF and Asset Basket Price") + xlab("Date") 
#----Premium/Discount to NAV
qplot(SOYB$DATE, ((SOYB$SOYB_MID - SOYB$SOYB_NAV)/SOYB$SOYB_NAV * 100), geom = 'line') +
  theme_bw() + ylab("Premium/Discount (%)") + xlab("Date") + ggtitle("SOYB Premium/Discount to NAV")

#-------------------OLS-----------------------------------------------#
#--------Simple
simple <- lm(per_asset_return ~ per_ETF_return, data = SOYB)
summary(simple)

#--------Dummy
model <- lm(abs(SOYB$etf_asset_error) ~ abs(SOYB$per_ETF_return) + SOYB$`S WASDE` + SOYB$`S WASDE + CP` +
              SOYB$`S Grain Stocks` + SOYB$`S Prospective Plantings` + SOYB$`S Acreage Report` + 
              SOYB$`S Cattle on Feed` + SOYB$`S Hogs & Pigs` + SOYB$`S Day Before Roll` + SOYB$`S Day After Roll` +
              SOYB$`C Feb` + SOYB$`C Mar` + SOYB$`C April` + SOYB$`C May` + SOYB$`C June` + SOYB$`C July` +
              SOYB$`C Aug` + SOYB$`C Sept` + SOYB$`C Nov` + SOYB$`C Dec` + SOYB$`C 2013` + SOYB$`C 2014` +
              SOYB$`C 2015` + SOYB$`C 2016` + SOYB$`C 2017` + SOYB$`C 2018` + SOYB$`C 2019` + SOYB$`C 2020`)
summary(model)

#---------------------GARCH----------------------------------------------#
err_garch = tseries::garch(x = SOYB$etf_asset_error, order = c(1,1))
summary(err_garch)

#_--------------------ACF and PACF Plots----------------------------------#
SOYB_Error <- SOYB$etf_asset_error
acf(SOYB_Error)
pacf(SOYB_Error)
