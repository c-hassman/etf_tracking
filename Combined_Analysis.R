rm(list = ls())
library(readxl)
library(tidyverse)
library(ggthemes)
library(xts)
library(tidyr)
library(zoo)
library(rugarch)
library(fpp2)

#---- Notes
# I know what you are asking: why am I still importing all this data seperately? The answer has to do with the external variables. 
# For each commodity, the prices during the roll period are taken out and then forward filled. In order to maintain the validity of 
# the report variables, they need to also excluded during the roll period. Because roll periods are different for each commodity, 
# the data needs to be pull seperately.


#-----------------Import Data from Excel and order------------#
CORN <- read_excel("G:/My Drive/3_Massa Research/Neff Paper/Working_Folder/Data_Update.xlsx", 
                   sheet = "CORN", col_types = c("numeric", 
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
SOYB <- read_excel("G:/My Drive/3_Massa Research/Neff Paper/Working_Folder/Data_Update.xlsx", 
                   sheet = "SOYB", col_types = c("numeric", 
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
WEAT <- read_excel("G:/My Drive/3_Massa Research/Neff Paper/Working_Folder/Data_Update.xlsx", 
                   sheet = "WEAT", col_types = c("numeric", 
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
USO <- read_excel("G:/My Drive/3_Massa Research/Neff Paper/Working_Folder/Data_Update.xlsx", 
                  sheet = "USO", col_types = c("numeric", 
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

UGA <- read_excel("G:/My Drive/3_Massa Research/Neff Paper/Working_Folder/Data_Update.xlsx", 
                  sheet = "UGA", col_types = c("numeric", 
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

#------------------Date Manipulation and Cleaning---------------#
CORN$DATE <- as.Date(CORN$DATE, origin = "1899-12-30")  #set date 
CORN <- CORN[order(CORN$DATE),] #order by date
CORN$asset_basket <- (CORN$`F1(.35)` * 0.35) + (CORN$`F2(.3)` * 0.3) + (CORN$`F3(.35)` * 0.35) #reconstruct asset basket
CORN$per_asset_return <- log(CORN$asset_basket/lag(CORN$asset_basket))* 100 # calculate percent asset basket return
CORN$per_ETF_return <- log(CORN$CORN_MID/lag(CORN$CORN_MID)) * 100#calculate percent ETF return
CORN$per_NAV_return <- log(CORN$CORN_NAV/lag(CORN$CORN_NAV)) * 100#calculate percent NAV return
CORN$etf_asset_error <- CORN$per_ETF_return - CORN$per_asset_return #calculate error between ETF and Asset

SOYB$DATE <- as.Date(SOYB$DATE, origin = "1899-12-30") # Set date
SOYB <- SOYB[order(SOYB$DATE),] #order by date
SOYB$asset_basket <- (SOYB$`F1(.35)` * 0.35) + (SOYB$`F2(.3)` * 0.30) + (SOYB$`F3(.35)` * 0.35)
SOYB$per_asset_return <- log(SOYB$asset_basket / lag(SOYB$asset_basket)) * 100
SOYB$per_ETF_return <- log(SOYB$SOYB_MID / lag(SOYB$SOYB_MID)) * 100
SOYB$per_NAV_return <- log(SOYB$SOYB_NAV / lag(SOYB$SOYB_NAV)) * 100
SOYB$etf_asset_error <- SOYB$per_ETF_return - SOYB$per_asset_return

WEAT$DATE <- as.Date(WEAT$DATE, origin = "1899-12-30")  #set date 
WEAT <- WEAT[order(WEAT$DATE),] #order by date
WEAT$asset_basket <- (WEAT$`F1(.35)` * 0.35) + (WEAT$`F2(.3)` * 0.3) + (WEAT$`F3(.35)` * 0.35) #reconstruct asset basket
WEAT$per_asset_return <- log(WEAT$asset_basket/lag(WEAT$asset_basket))* 100 # calculate percent asset basket return
WEAT$per_ETF_return <- log(WEAT$WEAT_MID/lag(WEAT$WEAT_MID)) * 100#calculate percent ETF return
WEAT$per_NAV_return <- log(WEAT$WEAT_NAV/lag(WEAT$WEAT_NAV)) * 100#calculate percent NAV return
WEAT$etf_asset_error <- WEAT$per_ETF_return - WEAT$per_asset_return #calculate error between ETF and Asset

USO$DATE <- as.Date(USO$DATE, origin = "1899-12-30") 
USO <- USO[order(USO$DATE),]
USO$per_asset_return <- log(USO$Futures/lag(USO$Futures)) * 100
USO$per_ETF_return <- log(USO$USO_MID /lag(USO$USO_MID)) * 100
USO$per_NAV_return <- log(USO$USO_NAV/lag(USO$USO_NAV)) * 100
USO$etf_asset_error <- USO$per_ETF_return - USO$per_asset_return

UGA$DATE <- as.Date(UGA$DATE, origin = "1899-12-30") 
UGA <- UGA[order(UGA$DATE),]
UGA$per_asset_return <- log(UGA$Futures/lag(UGA$Futures)) * 100
UGA$per_ETF_return <- log(UGA$UGA_MID/lag(UGA$UGA_MID)) * 100
UGA$per_NAV_return <- log(UGA$UGA_NAV/lag(UGA$UGA_NAV)) * 100
UGA$etf_asset_error <- UGA$per_ETF_return - UGA$per_asset_return

#  Volume data ------------------------------------------------------------------------







#--- More data cleaning
# The code below handles the issue of roll dates and using a continous time model
# with things which dont trade on the weekend. 
#Remove rows with NAsm which has the effect of deleting roll days
CORN <- na.omit(CORN) 
# This creates rows of NA's for all the missing days, including weekends, holidays,
# and the roll days we just deleted. Not sure why I have to create another "Date"
# column but that is the only way I could get it to work
CORN <- CORN %>% 
  mutate(Date = as.Date(DATE)) %>% 
  complete(Date = seq.Date(min(DATE), max(DATE), by="day"))
# Now to forward fill the date
CORN <- na.locf(na.locf(CORN), fromLast = TRUE)
# Remove the additional date column
CORN <- subset(CORN, select = -DATE)
#CORN <- na.omit(CORN) 
# Convert the object into a XTS object
CORN.xts <- xts(CORN[,-1], order.by = CORN$Date)

SOYB <- na.omit(SOYB)
SOYB <-SOYB %>% 
  mutate(Date = as.Date(DATE)) %>%
  complete(Date = seq.Date(min(DATE), max(DATE), by = "day"))
SOYB <- na.locf(na.locf(SOYB), fromLast = TRUE) #forward fill the date
SOYB <- subset(SOYB, select = -DATE) #remove redundant date column
SOYB.xts <- xts(SOYB[,-1], order.by = SOYB$Date) #create xts object

WEAT <- na.omit(WEAT) 
WEAT <- WEAT %>% 
  mutate(Date = as.Date(DATE)) %>% 
  complete(Date = seq.Date(min(DATE), max(DATE), by="day"))
WEAT <- na.locf(na.locf(WEAT), fromLast = TRUE)
WEAT <- subset(WEAT, select = -DATE)
WEAT.xts <- xts(WEAT[,-1], order.by = WEAT$Date)


USO <- na.omit(USO)
USO <- USO %>% 
  mutate(Date = as.Date(DATE)) %>% 
  complete(Date = seq.Date(min(DATE), max(DATE), by="day"))
USO <- na.locf(na.locf(USO), fromLast = TRUE)
USO <- subset(USO, select = -Date)
USO.xts <- as.xts(USO, order.by = USO$DATE)

UGA <- na.omit(UGA) 
UGA <- UGA %>% 
  mutate(Date = as.Date(DATE)) %>% 
  complete(Date = seq.Date(min(DATE), max(DATE), by="day"))
UGA <- na.locf(na.locf(UGA), fromLast = TRUE)
UGA <- subset(UGA, select = -DATE)
UGA.xts <- xts(UGA[,-1], order.by = UGA$Date)

#--- ETF Returns
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN$Date, CORN$per_ETF_return, type = "l", main = "CORN",
     xlab = "", ylab = "", ylim = c(-10, 10))
plot(SOYB$Date, SOYB$per_ETF_return, type = "l", main = "SOYB",
     xlab = "", ylab = "", ylim = c(-10, 10))
plot(WEAT$Date, WEAT$per_ETF_return, type = "l", main = "WEAT",
     xlab = "", ylab = "Percent Return", ylim = c(-10, 10))
plot(USO$DATE, USO$per_ETF_return, type = "l", main = "USO", 
     xlab = "", ylab = "", ylim = c(-10, 10))
plot(UGA$Date, UGA$per_ETF_return, type = "l", main = "UGA",
     xlab = "", ylab = "", ylim = c(-10, 10))

#-- Asset Returns
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN$Date, CORN$per_asset_return, type = "l", main = "CORN",
     xlab = "", ylab = "", ylim = c(-10, 10))
plot(SOYB$Date, SOYB$per_asset_return, type = "l", main = "SOYB",
     xlab = "", ylab = "", ylim = c(-10, 10))
plot(WEAT$Date, WEAT$per_asset_return, type = "l", main = "WEAT",
     xlab = "", ylab = "Percent Return", ylim = c(-10, 10))
plot(USO$DATE, USO$per_asset_return, type = "l", main = "USO", 
     xlab = "", ylab = "", ylim = c(-10, 10))
plot(UGA$Date, UGA$per_asset_return, type = "l", main = "UGA",
     xlab = "", ylab = "", ylim = c(-10, 10))

#--Tracking Error
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN$Date, CORN$etf_asset_error, type = "l", main = "CORN",
     xlab = "", ylab = "", ylim = c(-5, 5))
plot(SOYB$Date, SOYB$etf_asset_error, type = "l", main = "SOYB",
     xlab = "", ylab = "", ylim = c(-5, 5))
plot(WEAT$Date, WEAT$etf_asset_error, type = "l", main = "WEAT",
     xlab = "", ylab = "Percent Error", ylim = c(-5, 5))
plot(USO$DATE, USO$etf_asset_error, type = "l", main = "USO", 
     xlab = "", ylab = "", ylim = c(-5, 5))
plot(UGA$Date, UGA$etf_asset_error, type = "l", main = "UGA",
     xlab = "", ylab = "", ylim = c(-5, 5))


#--Squared Tracking Error
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN$Date, CORN$etf_asset_error^2, type = "l", main = "CORN",
     xlab = "", ylab = "")
plot(SOYB$Date, SOYB$etf_asset_error^2, type = "l", main = "SOYB",
     xlab = "", ylab = "")
plot(WEAT$Date, WEAT$etf_asset_error^2, type = "l", main = "WEAT",
     xlab = "", ylab = "Percent Error Squared")
plot(USO$DATE, USO$etf_asset_error^2, type = "l", main = "USO", 
     xlab = "", ylab = "")
plot(UGA$Date, UGA$etf_asset_error^2, type = "l", main = "UGA",
     xlab = "", ylab = "")


# Base Garch Models
corn.base_model_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(3,0)))
corn.base_fit <- ugarchfit(data = CORN.xts$etf_asset_error, spec = corn.base_model_spec)
corn.base_fit

soyb.base_model_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(4,3)))
soyb.base_fit <- ugarchfit(data = SOYB.xts$etf_asset_error, spec = soyb.base_model_spec)
soyb.base_fit

weat.base_model_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(4,3)))
weat.base_fit <- ugarchfit(data = WEAT.xts$etf_asset_error, spec = weat.base_model_spec)
weat.base_fit

uso.base_model_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(2,1)))
uso.base_fit <- ugarchfit(data = USO.xts$etf_asset_error, spec = uso.base_model_spec)
uso.base_fit

uga.base_model_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(3,1)))
uga.base_fit <- ugarchfit(data = UGA.xts$etf_asset_error, spec = uga.base_model_spec)
uga.base_fit


# Base GARCH Conditional Variance
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN$Date, corn.base_fit@fit[['sigma']], type = "l", main = "CORN",
     xlab = "", ylab = "")
plot(SOYB$Date, soyb.base_fit@fit[['sigma']], type = "l", main = "SOYB",
     xlab = "", ylab = "")
plot(WEAT$Date, weat.base_fit@fit[['sigma']], type = "l", main = "WEAT",
     xlab = "", ylab = "Sigma")
plot(USO$DATE, uso.base_fit@fit[['sigma']], type = "l", main = "USO", 
     xlab = "", ylab = "")
plot(UGA$Date, uga.base_fit@fit[['sigma']], type = "l", main = "UGA",
     xlab = "", ylab = "")


# Base GARCH Residuals
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN$Date, corn.base_fit@fit[['residuals']], type = "l", main = "CORN",
     xlab = "", ylab = "")
plot(SOYB$Date, soyb.base_fit@fit[['residuals']], type = "l", main = "SOYB",
     xlab = "", ylab = "")
plot(WEAT$Date, weat.base_fit@fit[['residuals']], type = "l", main = "WEAT",
     xlab = "", ylab = "Percent Error Squared")
plot(USO$DATE, uso.base_fit@fit[['residuals']], type = "l", main = "USO", 
     xlab = "", ylab = "")
plot(UGA$Date, uga.base_fit@fit[['residuals']], type = "l", main = "UGA",
     xlab = "", ylab = "")


# Base GARCH Squared Residuals 
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN$Date, corn.base_fit@fit[['residuals']]^2, type = "l", main = "CORN",
     xlab = "", ylab = "")
plot(SOYB$Date, soyb.base_fit@fit[['residuals']]^2, type = "l", main = "SOYB",
     xlab = "", ylab = "")
plot(WEAT$Date, weat.base_fit@fit[['residuals']]^2, type = "l", main = "WEAT",
     xlab = "", ylab = "Percent Error Squared")
plot(USO$DATE, uso.base_fit@fit[['residuals']]^2, type = "l", main = "USO", 
     xlab = "", ylab = "")
plot(UGA$Date, uga.base_fit@fit[['residuals']]^2, type = "l", main = "UGA",
     xlab = "", ylab = "")

# Base GARCH Residual diagnostic
corn.s <- corn.base_fit@fit[['residuals']] / corn.base_fit@fit[['sigma']]
soyb.s <- soyb.base_fit@fit[['residuals']] / soyb.base_fit@fit[['sigma']]
weat.s <- weat.base_fit@fit[['residuals']] / weat.base_fit@fit[['sigma']]
uso.s <- uso.base_fit@fit[['residuals']] / uso.base_fit@fit[['sigma']]
uga.s <- uga.base_fit@fit[['residuals']] / uga.base_fit@fit[['sigma']]


# Base Standardized GARCH Residuals
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN$Date, corn.s, type = "l", main = "CORN",
     xlab = "", ylab = "")
plot(SOYB$Date, soyb.s, type = "l", main = "SOYB",
     xlab = "", ylab = "")
plot(WEAT$Date, weat.s, type = "l", main = "WEAT",
     xlab = "", ylab = "Standardized Residuals")
plot(USO$DATE, uso.s, type = "l", main = "USO", 
     xlab = "", ylab = "")
plot(UGA$Date, uga.s, type = "l", main = "UGA",
     xlab = "", ylab = "")

# Base Standardized GARCH Squared Residuals 
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN$Date, corn.s^2, type = "l", main = "CORN",
     xlab = "", ylab = "")
plot(SOYB$Date, soyb.s^2, type = "l", main = "SOYB",
     xlab = "", ylab = "")
plot(WEAT$Date, weat.s^2, type = "l", main = "WEAT",
     xlab = "", ylab = "Standardized Residuals Squared")
plot(USO$DATE, uso.s^2, type = "l", main = "USO", 
     xlab = "", ylab = "")
plot(UGA$Date, uga.s^2, type = "l", main = "UGA",
     xlab = "", ylab = "")


# Base GARCH Residuals ACF
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
ggAcf(corn.s)
ggAcf(weat.s)
ggAcf(uso.s)
ggAcf(uga.s)
