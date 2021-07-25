###############################################################################
#### Graphs        ############################################################
###############################################################################
# Colburn Hassman
# colburn7@vt.edu
# January 26, 2021



# source("~/Documents/etf_tracking/Analysis/preprocessing.R")
source("/home/colburn/Documents/etf_tracking/Analysis/preprocessing.R")

CORN <- data_pull_ex("CORN")
SOYB <- data_pull_ex("SOYB")
WEAT <- data_pull_ex("WEAT")
USO <- data_pull_ex("USO")
UGA <- data_pull_ex("UGA")


#### Prices ####################################################################
par(mfrow = c(5, 3), mai = c(0.3, .65, 0.2, 0.05))
# CORN
plot(CORN$DATE, CORN$Price, type = "l", main = "ETF",
     xlab = "", ylab = "CORN\n $/Share")
plot(CORN$DATE, CORN$NAV, type = "l", main = "NAV",
     xlab = "", ylab = "$/Share")
plot(CORN$DATE, CORN$asset_basket, type = "l", main = "Asset Basket",
     xlab = "", ylab = "Cents/Bushel")
# SOYB 
plot(SOYB$DATE, SOYB$Price, type = "l", main = "",
     xlab = "", ylab = "SOYB\n $/Share")
plot(SOYB$DATE, SOYB$NAV, type = "l", main = "",
     xlab = "", ylab = "$/Share")
plot(SOYB$DATE, SOYB$asset_basket, type = "l", main = "",
     xlab = "", ylab = "Cents/Bushel")
#WEAT
plot(WEAT$DATE, WEAT$Price, type = "l", main = "",
     xlab = "", ylab = "WEAT\n $/Share")
plot(WEAT$DATE, WEAT$NAV, type = "l", main = "",
     xlab = "", ylab = "$/Share")
plot(WEAT$DATE, WEAT$asset_basket, type = "l", main = "",
     xlab = "", ylab = "Cents/Bushel")
#USO
plot(USO$DATE, USO$Price, type = "l", main = "", 
     xlab = "", ylab = "USO\n $/Share") 
plot(USO$DATE, USO$NAV, type = "l", main = "", 
     xlab = "", ylab = "$/Share") 
plot(USO$DATE, USO$asset_basket , type = "l", main = "", 
     xlab = "", ylab = "$/Barrel") 
# UGA
plot(UGA$DATE, UGA$Price, type = "l", main = "",
     xlab = "", ylab = "UGA\n $/Share") 
plot(UGA$DATE, UGA$NAV, type = "l", main = "",
     xlab = "", ylab = "$/Share") 
plot(UGA$DATE, UGA$asset_basket, type = "l", main = "",
     xlab = "", ylab = "$/Gallon") 

dev.off()


#### Returns ###################################################################

par(mfrow = c(5, 3), mai = c(0.3, .55, 0.2, 0.05))
# CORN
plot(CORN$DATE, CORN$per_ETF_return, type = "l", main = "ETF Percent Return",
     xlab = "", ylab = "CORN")
plot(CORN$DATE, CORN$per_NAV_return, type = "l", main = "NAV Percent Return",
     xlab = "", ylab = "")
plot(CORN$DATE, CORN$per_asset_return, type = "l", main = "Asset Basket Percent Return",
     xlab = "", ylab = "")
# SOYB 
plot(SOYB$DATE, SOYB$per_ETF_return, type = "l", main = "",
     xlab = "", ylab = "SOYB")
plot(SOYB$DATE, SOYB$per_NAV_return, type = "l", main = "",
     xlab = "", ylab = "")
plot(SOYB$DATE, SOYB$per_asset_return, type = "l", main = "",
     xlab = "", ylab = "")
#WEAT
plot(WEAT$DATE, WEAT$per_ETF_return, type = "l", main = "",
     xlab = "", ylab = "WEAT")
plot(WEAT$DATE, WEAT$per_NAV_return, type = "l", main = "",
     xlab = "", ylab = "")
plot(WEAT$DATE, WEAT$per_asset_return, type = "l", main = "",
     xlab = "", ylab = "")
#USO
plot(USO$DATE, USO$per_ETF_return, type = "l", main = "", 
     xlab = "", ylab = "USO") 
plot(USO$DATE, USO$per_NAV_return, type = "l", main = "", 
     xlab = "", ylab = "") 
plot(USO$DATE, USO$per_asset_return , type = "l", main = "", 
     xlab = "", ylab = "") 
# UGA
plot(UGA$DATE, UGA$per_ETF_return, type = "l", main = "",
     xlab = "", ylab = "UGA") 
plot(UGA$DATE, UGA$per_NAV_return, type = "l", main = "",
     xlab = "", ylab = "") 
plot(UGA$DATE, UGA$per_asset_return, type = "l", main = "",
     xlab = "", ylab = "") 
dev.off()

#### Tracking Differences ######################################################

par(mfrow = c(5, 3), mai = c(0.3, 0.55, 0.2, 0.05))
# CORN
plot(CORN$DATE, (CORN$per_ETF_return - CORN$per_asset_return), type = "l", main = "Total Tracking Difference",
     xlab = "", ylab = "CORN")
plot(CORN$DATE, (CORN$per_NAV_return - CORN$per_asset_return), type = "l", main = "Managerial Tracking Difference",
     xlab = "", ylab = "")
plot(CORN$DATE, (CORN$per_ETF_return - CORN$per_NAV_return), type = "l", main = "Arbitrage Tracking Difference",
     xlab = "", ylab = "")
# SOYB
plot(SOYB$DATE, (SOYB$per_ETF_return - SOYB$per_asset_return), type = "l", main = "",
     xlab = "", ylab = "SOYB")
plot(SOYB$DATE, (SOYB$per_NAV_return - SOYB$per_asset_return), type = "l", main = "",
     xlab = "", ylab = "")
plot(SOYB$DATE, (SOYB$per_ETF_return - SOYB$per_NAV_return), type = "l", main = "",
     xlab = "", ylab = "")
# WEAT
plot(WEAT$DATE, (WEAT$per_ETF_return - WEAT$per_asset_return), type = "l", main = "",
     xlab = "", ylab = "WEAT")
plot(WEAT$DATE, (WEAT$per_NAV_return - WEAT$per_asset_return), type = "l", main = "",
     xlab = "", ylab = "")
plot(WEAT$DATE, (WEAT$per_ETF_return - WEAT$per_NAV_return), type = "l", main = "",
     xlab = "", ylab = "")
# USO
plot(USO$DATE, (USO$per_ETF_return - USO$per_asset_return), type = "l", main = "", 
     xlab = "", ylab = "USO") 
plot(USO$DATE, (USO$per_NAV_return - USO$per_asset_return), type = "l", main = "", 
     xlab = "", ylab = "") 
plot(USO$DATE, (USO$per_ETF_return - USO$per_NAV_return), type = "l", main = "", 
     xlab = "", ylab = "") 
# UGA
plot(UGA$DATE, (UGA$per_ETF_return - UGA$per_asset_return), type = "l", main = "",
     xlab = "", ylab = "UGA") 
plot(UGA$DATE, (UGA$per_NAV_return - UGA$per_asset_return), type = "l", main = "",
     xlab = "", ylab = "") 
plot(UGA$DATE, (UGA$per_ETF_return - UGA$per_NAV_return), type = "l", main = "",
     xlab = "", ylab = "") 
dev.off()

#### Scatter Plot ##############################################################

par(mfrow = c(5, 3), mai = c(0.5, 0.65, 0.2, 0.05))
# CORN
plot(CORN$per_ETF_return, CORN$per_asset_return, type = "p", main = "Total Tracking Difference",
     xlab = "ETF Return", ylab = "CORN\n Asset Return", pch = 19)
plot(CORN$per_NAV_return,  CORN$per_asset_return, type = "p", main = "Managerial Tracking Difference",
     xlab = "NAV Return", ylab = "Asset Return", pch = 19)
plot(CORN$per_ETF_return, CORN$per_NAV_return, type = "p", main = "Arbitrage Tracking Difference",
     xlab = "ETF Return", ylab = "NAV Return", pch = 19)
# SOYB
plot(SOYB$per_ETF_return, SOYB$per_asset_return, type = "p", main = "",
     xlab = "ETF Return", ylab = "SOYB\n Asset Return", pch = 19)
plot(SOYB$per_NAV_return, SOYB$per_asset_return, type = "p", main = "",
     xlab = "NAV Return", ylab = "Asset Return", pch = 19)
plot(SOYB$per_ETF_return, SOYB$per_NAV_return, type = "p", main = "",
     xlab = "ETF Return", ylab = "NAV Return", pch = 19)
# WEAT
plot(WEAT$per_ETF_return, WEAT$per_asset_return, type = "p", main = "",
     xlab = "ETF Return", ylab = "WEAT\n Asset Retunr", pch = 19)
plot(WEAT$per_NAV_return, WEAT$per_asset_return, type = "p", main = "",
     xlab = "NAV Return", ylab = "Asset Return", pch = 19)
plot(WEAT$per_ETF_return, WEAT$per_NAV_return, type = "p", main = "",
     xlab = "ETF Return", ylab = "NAV Return", pch = 19)
# USO
plot(USO$per_ETF_return, USO$per_asset_return, type = "p", main = "", 
     xlab = "ETF Return", ylab = "USO\n Asset Return", pch = 19) 
plot(USO$per_NAV_return, USO$per_asset_return, type = "p", main = "", 
     xlab = "NAV Return", ylab = "Asset Return", pch = 19) 
plot(USO$per_ETF_return, USO$per_NAV_return, type = "p", main = "", 
     xlab = "ETF Return", ylab = "NAV Return", pch = 19) 
# UGA
plot(UGA$per_ETF_return, UGA$per_asset_return, type = "p", main = "",
     xlab = "ETF Return", ylab = "UGA\n Asset Return", pch = 19) 
plot(UGA$per_NAV_return, UGA$per_asset_return, type = "p", main = "",
     xlab = "NAV Return", ylab = "Asset Return", pch = 19) 
plot(UGA$per_ETF_return, UGA$per_NAV_return, type = "p", main = "",
     xlab = "ETF Return", ylab = "NAV Return", pch = 19) 
dev.off()



