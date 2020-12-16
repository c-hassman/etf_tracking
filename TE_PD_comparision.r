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
CORN <- read.csv("~/Documents/etf_tracking/corn.csv")
SOYB <- read.csv("~/Documents/etf_tracking/soyb.csv")
WEAT <- read.csv("~/Documents/etf_tracking/weat.csv")
USO <- read.csv("~/Documents/etf_tracking/uso.csv")
UGA <- read.csv("~/Documents/etf_tracking/uga.csv")



#------------------Date Manipulation and Cleaning---------------#
# When importing data from csv, for some reason empty columns are created
# This is only a temporary solution to this problem
CORN <- CORN[1:(length(CORN)-2)]
SOYB <- SOYB[1:(length(SOYB)-1)]
WEAT <- WEAT[1:(length(WEAT)-2)]
# USO Good
UGA <- UGA[1:(length(UGA)-2)]




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


UGA$DATE <- as.Date(UGA$DATE, format = "%m/%d/%Y")
UGA <- UGA[order(UGA$DATE),]
UGA$per_asset_return <- log(UGA$Futures/lag(UGA$Futures)) * 100
UGA$per_ETF_return <- log(UGA$UGA_MID/lag(UGA$UGA_MID)) * 100



#--- More data cleaning
# The code below handles the issue of roll dates 
CORN <- na.omit(CORN)  
CORN <- CORN[!(CORN$ROLL == 1),] #directly remove roll
CORN.xts <- xts(CORN[,-1], order.by = CORN$DATE)

SOYB <- na.omit(SOYB)
SOYB <- SOYB[!(SOYB$ROLL == 1),]
SOYB.xts <- xts(SOYB[,-1], order.by = SOYB$DATE) #create xts object

WEAT <- na.omit(WEAT) 
WEAT <- WEAT[!(WEAT$ROLL == 1),]
WEAT.xts <- xts(WEAT[,-1], order.by = WEAT$DATE)


USO <- na.omit(USO)
USO <- USO[!(USO$ROLL == 1),]
USO.xts <- as.xts(USO, order.by = USO$DATE)

UGA <- na.omit(UGA) 
UGA <- UGA[!(UGA$ROLL == 1),]
UGA.xts <- xts(UGA[,-1], order.by = UGA$DATE)