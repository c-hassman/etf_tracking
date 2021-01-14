########################################################
# Tracking Difference and Premium/Discount Analysis
########################################################
# Colburn Hassman
# December 7, 2020

library(tseries)
library(xts)
library(tidyverse)
library(fpp2)
library(stats)

setwd("~/Documents/etf_tracking")

#-----------------Import Data from Excel and order------------#
CORN <- read.csv("~/Documents/etf_tracking/corn_master.csv")
SOYB <- read.csv("~/Documents/etf_tracking/soyb_master.csv")
WEAT <- read.csv("~/Documents/etf_tracking/weat_master.csv")
USO <- read.csv("~/Documents/etf_tracking/uso_master.csv")
UGA <- read.csv("~/Documents/etf_tracking/uga_master.csv")



#------------------Date Manipulation and Cleaning---------------#

CORN$DATE <- as.Date(CORN$DATE, format = "%m/%d/%Y")  #set date 
CORN <- CORN[order(CORN$DATE),] #order by date
CORN$asset_basket <- (CORN$`F1..35.` * 0.35) + (CORN$`F2..3.` * 0.3) + (CORN$`F3..35.` * 0.35) #reconstruct asset basket
CORN$per_asset_return <- log(CORN$asset_basket/lag(CORN$asset_basket))* 100 # calculate percent asset basket return
CORN$per_ETF_return <- log(CORN$CORN_MID/lag(CORN$CORN_MID)) * 100#calculate percent ETF return
CORN$per_nav_return <- log(CORN$CORN_NAV/lag(CORN$CORN_NAV)) * 100 # Calculate NAV Return


SOYB$DATE <- as.Date(SOYB$DATE, format = "%m/%d/%Y") # Set date
SOYB <- SOYB[order(SOYB$DATE),] #order by date
SOYB$asset_basket <- (SOYB$`F1..35.` * 0.35) + (SOYB$`F2..3.` * 0.30) + (SOYB$`F3..35.` * 0.35)
SOYB$per_asset_return <- log(SOYB$asset_basket / lag(SOYB$asset_basket)) * 100
SOYB$per_ETF_return <- log(SOYB$SOYB_MID / lag(SOYB$SOYB_MID)) * 100
SOYB$per_nav_return <- log(SOYB$SOYB_NAV/lag(SOYB$SOYB_NAV)) * 100


WEAT$DATE <- as.Date(WEAT$DATE, format = "%m/%d/%Y") #set date 
WEAT <- WEAT[order(WEAT$DATE),] #order by date
WEAT$asset_basket <- (WEAT$`F1..35.` * 0.35) + (WEAT$`F2..3.` * 0.3) + (WEAT$`F3..35.` * 0.35) #reconstruct asset basket
WEAT$per_asset_return <- log(WEAT$asset_basket/lag(WEAT$asset_basket))* 100 # calculate percent asset basket return
WEAT$per_ETF_return <- log(WEAT$WEAT_MID/lag(WEAT$WEAT_MID)) * 100#calculate percent ETF return
WEAT$per_nav_return <- log(WEAT$WEAT_NAV/lag(WEAT$WEAT_NAV)) * 100 


USO$DATE <- as.Date(USO$DATE, format = "%m/%d/%Y")
USO <- USO[order(USO$DATE),]
USO$per_asset_return <- log(USO$Futures/lag(USO$Futures)) * 100
USO$per_ETF_return <- log(USO$USO_MID /lag(USO$USO_MID)) * 100
USO$per_nav_return <- log(USO$USO_NAV/lag(USO$USO_NAV)) * 100

UGA$DATE <- as.Date(UGA$DATE, format = "%m/%d/%Y")
UGA <- UGA[order(UGA$DATE),]
UGA$per_asset_return <- log(UGA$Futures/lag(UGA$Futures)) * 100
UGA$per_ETF_return <- log(UGA$UGA_MID/lag(UGA$UGA_MID)) * 100
UGA$per_nav_return <- log(UGA$UGA_NAV/lag(UGA$UGA_NAV)) * 100



#--- More data cleaning
# The code below handles the issue of roll dates 
CORN <- na.omit(CORN)  
CORN <- CORN[!(CORN$ROLL == 1),] #directly remove roll

SOYB <- na.omit(SOYB)
SOYB <- SOYB[!(SOYB$ROLL == 1),]

WEAT <- na.omit(WEAT) 
WEAT <- WEAT[!(WEAT$ROLL == 1),]


USO <- na.omit(USO)
USO <- USO[!(USO$ROLL == 1),]

UGA <- na.omit(UGA) 
UGA <- UGA[!(UGA$ROLL == 1),]

# Am I miss defining this? What the hell am I actually measuring with the PD 
# Column..... This I need to just look at Premium/Discount, not change in NAV return
# versus change in Price return... or

# Average Absolute return difference (AARD)
# Standard Deviation of return differences (SDRD)
# Tracking Differences (TD):
# NAV - Benchmark
# Premium/Discount:
# Price - NAV
# Total Tracking Divergence (totTD):
# Price - Benchmark

CORN$TD <- CORN$per_nav_return - CORN$per_asset_return
CORN$PD <- CORN$per_ETF_return - CORN$per_nav_return
CORN$totTD <- CORN$per_ETF_return - CORN$per_asset_return


SOYB$TD <- SOYB$per_nav_return - SOYB$per_asset_return
SOYB$PD <- SOYB$per_ETF_return - SOYB$per_nav_return
SOYB$totTD <- SOYB$per_ETF_return - SOYB$per_asset_return

WEAT$TD <- WEAT$per_nav_return - WEAT$per_asset_return
WEAT$PD <- WEAT$per_ETF_return - WEAT$per_nav_return
WEAT$totTD <- WEAT$per_ETF_return - WEAT$per_asset_return

USO$TD <- USO$per_nav_return - USO$per_asset_return
USO$PD <- USO$per_ETF_return - USO$per_nav_return
USO$totTD <- USO$per_ETF_return - USO$per_asset_return

UGA$TD <- UGA$per_nav_return - UGA$per_asset_return
UGA$PD <- UGA$per_ETF_return - UGA$per_nav_return
UGA$totTD <- UGA$per_ETF_return - UGA$per_asset_return

# AARD
AARD_TD_CORN <- mean(abs(CORN$TD))
AARD_PD_CORN <- mean(abs(CORN$PD))
AARD_totTD_CORN <- mean(abs(CORN$totTD))

AARD_TD_SOYB <- mean(abs(SOYB$TD))
AARD_PD_SOYB <- mean(abs(SOYB$PD))
AARD_totTD_SOYB <- mean(abs(SOYB$totTD))

AARD_TD_WEAT <- mean(abs(WEAT$TD))
AARD_PD_WEAT <- mean(abs(WEAT$PD))
AARD_totTD_WEAT <- mean(abs(WEAT$totTD))

AARD_TD_USO <- mean(abs(USO$TD))
AARD_PD_USO <- mean(abs(USO$PD))
AARD_totTD_USO <- mean(abs(USO$totTD))

AARD_TD_UGA <- mean(abs(UGA$TD))
AARD_PD_UGA <- mean(abs(UGA$PD))
AARD_totTD_UGA <- mean(abs(UGA$totTD))

# SDRD
SDRD_TD_CORN <- sd(CORN$TD)
SDRC_PD_CORN <- sd(CORN$PD)
SDRC_totTD_CORN <- sd(CORN$totTD)

SDRD_TD_SOYB <- sd(SOYB$TD)
SDRC_PD_SOYB <- sd(SOYB$PD)
SDRC_totTD_SOYB <- sd(SOYB$totTD)

SDRD_TD_WEAT <- sd(WEAT$TD)
SDRC_PD_WEAT <- sd(WEAT$PD)
SDRC_totTD_WEAT <- sd(WEAT$totTD)

SDRD_TD_USO <- sd(USO$TD)
SDRC_PD_USO <- sd(USO$PD)
SDRC_totTD_USO <- sd(USO$totTD)

SDRD_TD_UGA <- sd(UGA$TD)
SDRC_PD_UGA <- sd(UGA$PD)
SDRC_totTD_UGA <- sd(UGA$totTD)

plot(CORN$PD)
plot(WEAT$PD)
plot(USO$PD)
# Alright, this is a very surprising result
plot(x = CORN$TD, y = CORN$PD)
plot(x = SOYB$TD, y = SOYB$PD, 
     xlab = "Tracking Difference", 
     ylab = "% Change in Premium/Discount")
plot(x = WEAT$TD, y = WEAT$PD)
plot(x = USO$TD, y = USO$PD)
plot(x = UGA$TD, y = UGA$PD)


# !!! Change NAV to x position in premium/discount
plot(x = CORN$per_asset_return, y = CORN$per_nav_return , 
     xlab = "Benchmark Return", ylab = "NAV Return", 
     main = "CORN Tracking Difference A")
plot(x = CORN$per_nav_return, y = CORN$per_ETF_return, 
     xlab = "NAV Return", ylab =" ETF Price Return", 
     main = "CORN Tracking Difference B")

plot(x = SOYB$per_asset_return, y = SOYB$per_nav_return, 
     xlab = "Benchmark Return", ylab = "NAV Return")
plot(x = SOYB$per_nav_return, y = SOYB$per_ETF_return, 
     xlab = "NAV Return", ylab =" ETF Price Return")

plot(x = WEAT$per_asset_return, y = WEAT$per_nav_return )
plot(x = WEAT$per_ETF_return, y = WEAT$per_nav_return)

plot(x = USO$per_asset_return, y = USO$per_nav_return )
plot(x = USO$per_ETF_return, y = USO$per_nav_return)

plot(x = UGA$per_asset_return, y = UGA$per_nav_return )
plot(x = UGA$per_ETF_return, y = UGA$per_nav_return)
     