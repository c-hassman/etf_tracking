########################################################
# Tracking Difference Analysis
########################################################
# Colburn Hassman
# December 15, 2020

library(tseries)
library(xts)
library(tidyverse)
library(fpp2)
library(stats)
library(gridExtra)
library(grid)

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

##### Managerial TD

CORN$TDm <- CORN$per_nav_return - CORN$per_asset_return
SOYB$TDm <- SOYB$per_nav_return - SOYB$per_asset_return
WEAT$TDm <- WEAT$per_nav_return - WEAT$per_asset_return
USO$TDm <- USO$per_nav_return - USO$per_asset_return
UGA$TDm <- UGA$per_nav_return - UGA$per_asset_return

