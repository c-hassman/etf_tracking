rm(list = ls())
library(readxl)
library(tidyverse)
library(ggthemes)
library(xts)
library(tidyr)
library(zoo)
library(rugarch)
library(fpp2)
library(lubridate)
library(writexl)
library(tseries)

#devtools::install_github("ropensci/writexl")

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
#CORN$per_NAV_return <- log(CORN$CORN_NAV/lag(CORN$CORN_NAV)) * 100#calculate percent NAV return
#CORN$etf_asset_error <- CORN$per_ETF_return - CORN$per_asset_return #calculate error between ETF and Asset

SOYB$DATE <- as.Date(SOYB$DATE, origin = "1899-12-30") # Set date
SOYB <- SOYB[order(SOYB$DATE),] #order by date
SOYB$asset_basket <- (SOYB$`F1(.35)` * 0.35) + (SOYB$`F2(.3)` * 0.30) + (SOYB$`F3(.35)` * 0.35)
SOYB$per_asset_return <- log(SOYB$asset_basket / lag(SOYB$asset_basket)) * 100
SOYB$per_ETF_return <- log(SOYB$SOYB_MID / lag(SOYB$SOYB_MID)) * 100
#SOYB$per_NAV_return <- log(SOYB$SOYB_NAV / lag(SOYB$SOYB_NAV)) * 100
#SOYB$etf_asset_error <- SOYB$per_ETF_return - SOYB$per_asset_return

WEAT$DATE <- as.Date(WEAT$DATE, origin = "1899-12-30")  #set date 
WEAT <- WEAT[order(WEAT$DATE),] #order by date
WEAT$asset_basket <- (WEAT$`F1(.35)` * 0.35) + (WEAT$`F2(.3)` * 0.3) + (WEAT$`F3(.35)` * 0.35) #reconstruct asset basket
WEAT$per_asset_return <- log(WEAT$asset_basket/lag(WEAT$asset_basket))* 100 # calculate percent asset basket return
WEAT$per_ETF_return <- log(WEAT$WEAT_MID/lag(WEAT$WEAT_MID)) * 100#calculate percent ETF return
#WEAT$per_NAV_return <- log(WEAT$WEAT_NAV/lag(WEAT$WEAT_NAV)) * 100#calculate percent NAV return
#WEAT$etf_asset_error <- WEAT$per_ETF_return - WEAT$per_asset_return #calculate error between ETF and Asset

USO$DATE <- as.Date(USO$DATE, origin = "1899-12-30") 
USO <- USO[order(USO$DATE),]
USO$per_asset_return <- log(USO$Futures/lag(USO$Futures)) * 100
USO$per_ETF_return <- log(USO$USO_MID /lag(USO$USO_MID)) * 100
#USO$per_NAV_return <- log(USO$USO_NAV/lag(USO$USO_NAV)) * 100
#USO$etf_asset_error <- USO$per_ETF_return - USO$per_asset_return

UGA$DATE <- as.Date(UGA$DATE, origin = "1899-12-30") 
UGA <- UGA[order(UGA$DATE),]
UGA$per_asset_return <- log(UGA$Futures/lag(UGA$Futures)) * 100
UGA$per_ETF_return <- log(UGA$UGA_MID/lag(UGA$UGA_MID)) * 100
#UGA$per_NAV_return <- log(UGA$UGA_NAV/lag(UGA$UGA_NAV)) * 100
#UGA$etf_asset_error <- UGA$per_ETF_return - UGA$per_asset_return

#  Volume data ------------------------------------------------------------------------
volume <- read.csv("G:/My Drive/3_Massa Research/Neff Paper/Working_Folder/Volume.csv")
# subset the dataframe to only the relevant columes
corn.volume <- data.frame(as.Date(volume$DATE), volume$CORN.Volume)
#rename the columns
colnames(corn.volume) <- c("DATE", "Volume")
#Merge the Volume data with the other data
CORN <- merge(CORN, corn.volume, by = "DATE")
# calculate percent change in volume
CORN$volume_return <-log(CORN$Volume/lag(CORN$Volume)) * 100

soyb.volume <- data.frame(as.Date(volume$DATE), volume$SOYB.Volume)
colnames(soyb.volume) <- c("DATE", "Volume")
SOYB <- merge(SOYB, soyb.volume, by = "DATE")
SOYB$volume_return <-log(SOYB$Volume/lag(SOYB$Volume)) * 100

weat.volume <- data.frame(as.Date(volume$DATE), volume$WEAT.Volume)
colnames(weat.volume) <- c("DATE", "Volume")
WEAT <- merge(WEAT, weat.volume, by = "DATE")
WEAT$volume_return <-log(WEAT$Volume/lag(WEAT$Volume)) * 100

uso.volume <- data.frame(as.Date(volume$DATE), volume$USO.Volume)
colnames(uso.volume) <- c("DATE", "Volume")
USO <- merge(USO, uso.volume, by = "DATE")
USO$volume_return <-log(USO$Volume/lag(USO$Volume)) * 100

uga.volume <- data.frame(as.Date(volume$DATE), volume$UGA.Volume)
colnames(uga.volume) <- c("DATE", "Volume")
UGA <- merge(UGA, uga.volume, by = "DATE")
UGA$volume_return <-log(UGA$Volume/lag(UGA$Volume)) * 100


#--- More data cleaning
# The code below handles the issue of roll dates and using a continous time model
# with things which dont trade on the weekend. 
#Remove rows with NAsm 
CORN <- na.omit(CORN)  
CORN <- CORN[!(CORN$ROLL == 1),] #directly remove roll
# This creates rows of NA's for all the missing days, including weekends, holidays,
# and the roll days we just deleted. Not sure why I have to create another "Date"
# column but that is the only way I could get it to work
# CORN <- CORN %>% 
#   mutate(Date = as.Date(DATE)) %>% 
#   complete(Date = seq.Date(min(DATE), max(DATE), by="day"))
# # Now to forward fill the date
# CORN <- na.locf(na.locf(CORN), fromLast = TRUE)
#CORN <- subset(CORN, select = -DATE)
#CORN <- na.omit(CORN) 
# Convert the object into a XTS object
CORN.xts <- xts(CORN[,-1], order.by = CORN$DATE)

SOYB <- na.omit(SOYB)
SOYB <- SOYB[!(SOYB$ROLL == 1),]
#SOYB <-SOYB %>% 
#  mutate(Date = as.Date(DATE)) %>%
#  complete(Date = seq.Date(min(DATE), max(DATE), by = "day"))
#SOYB <- na.locf(na.locf(SOYB), fromLast = TRUE) #forward fill the date
# SOYB <- subset(SOYB, select = -DATE) #remove redundant date column
SOYB.xts <- xts(SOYB[,-1], order.by = SOYB$Date) #create xts object

WEAT <- na.omit(WEAT) 
WEAT <- WEAT[!(WEAT$ROLL == 1),]
# WEAT <- WEAT %>% 
#   mutate(Date = as.Date(DATE)) %>% 
#   complete(Date = seq.Date(min(DATE), max(DATE), by="day"))
# WEAT <- na.locf(na.locf(WEAT), fromLast = TRUE)
# WEAT <- subset(WEAT, select = -DATE)
WEAT.xts <- xts(WEAT[,-1], order.by = WEAT$DATE)


USO <- na.omit(USO)
USO <- USO[!(USO$ROLL == 1),]

# USO <- USO %>% 
#   mutate(Date = as.Date(DATE)) %>% 
#   complete(Date = seq.Date(min(DATE), max(DATE), by="day"))
# USO <- na.locf(na.locf(USO), fromLast = TRUE)
# USO <- subset(USO, select = -DATE)
USO.xts <- as.xts(USO, order.by = USO$DATE)

UGA <- na.omit(UGA) 
UGA <- UGA[!(UGA$ROLL == 1),]
# UGA <- UGA %>% 
#   mutate(Date = as.Date(DATE)) %>% 
#   complete(Date = seq.Date(min(DATE), max(DATE), by="day"))
# UGA <- na.locf(na.locf(UGA), fromLast = TRUE)
# UGA <- subset(UGA, select = -DATE)
UGA.xts <- xts(UGA[,-1], order.by = UGA$DATE)

#---- LM OLS Models
corn.ols <- lm(CORN$per_ETF_return ~ CORN$per_asset_return)
CORN$etf_asset_error <- corn.ols$residuals  # The residuals of the model are the tracking error
summary(corn.ols)
adf.test(corn.ols$residuals)

soyb.ols <- lm(SOYB$per_ETF_return ~ SOYB$per_asset_return)
SOYB$etf_asset_error <- soyb.ols$residuals 
summary(soyb.ols)
adf.test(soyb.ols$residuals)

weat.ols <- lm(WEAT$per_ETF_return ~ WEAT$per_asset_return)
WEAT$etf_asset_error <- weat.ols$residuals
summary(weat.ols)
adf.test(soyb.ols$residuals)

uso.ols <- lm(USO$per_ETF_return ~USO$per_asset_return)
USO$etf_asset_error <- uso.ols$residuals
summary(uso.ols)
adf.test(uso.ols$residuals)

uga.ols <- lm(UGA$per_ETF_return ~ UGA$per_asset_return)
UGA$etf_asset_error <- uga.ols$residuals
summary(uga.ols)
adf.test(uga.ols$residuals)

#--- ETF Returns

par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN$DATE, CORN$per_ETF_return, type = "l", main = "CORN",
     xlab = "", ylab = "", ylim = c(-10, 10))
plot(SOYB$DATE, SOYB$per_ETF_return, type = "l", main = "SOYB",
     xlab = "", ylab = "", ylim = c(-10, 10))
plot(WEAT$DATE, WEAT$per_ETF_return, type = "l", main = "WEAT",
     xlab = "", ylab = "Percent Return", ylim = c(-10, 10))
plot(USO$DATE, USO$per_ETF_return, type = "l", main = "USO", 
     xlab = "", ylab = "", ylim = c(-10, 10))
plot(UGA$DATE, UGA$per_ETF_return, type = "l", main = "UGA",
     xlab = "", ylab = "", ylim = c(-10, 10))

#-- Asset Returns
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN$DATE, CORN$per_asset_return, type = "l", main = "CORN",
     xlab = "", ylab = "", ylim = c(-10, 10))
plot(SOYB$DATE, SOYB$per_asset_return, type = "l", main = "SOYB",
     xlab = "", ylab = "", ylim = c(-10, 10))
plot(WEAT$DATE, WEAT$per_asset_return, type = "l", main = "WEAT",
     xlab = "", ylab = "Percent Return", ylim = c(-10, 10))
plot(USO$DATE, USO$per_asset_return, type = "l", main = "USO", 
     xlab = "", ylab = "", ylim = c(-10, 10))
plot(UGA$DATE, UGA$per_asset_return, type = "l", main = "UGA",
     xlab = "", ylab = "", ylim = c(-10, 10))

#--Tracking Error
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN$DATE, CORN$etf_asset_error, type = "l", main = "CORN",
     xlab = "", ylab = "", ylim = c(-5, 5))
plot(SOYB$DATE, SOYB$etf_asset_error, type = "l", main = "SOYB",
     xlab = "", ylab = "", ylim = c(-5, 5))
plot(WEAT$DATE, WEAT$etf_asset_error, type = "l", main = "WEAT",
     xlab = "", ylab = "Percent Error", ylim = c(-5, 5))
plot(USO$DATE, USO$etf_asset_error, type = "l", main = "USO", 
     xlab = "", ylab = "", ylim = c(-5, 5))
plot(UGA$DATE, UGA$etf_asset_error, type = "l", main = "UGA",
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

#-------- FIT ARIMA Model to Tracking Error
arima100 <- arima(UGA$etf_asset_error, order = c(2,0,2))
summary(arima100)
checkresiduals(arima100)
Box.test(arima100$residuals^2, type = 'Ljung-Box')
#auto.arima(WEAT$etf_asset_error)


# Base Garch Models
corn.base_model_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(1,2)))
corn.base_fit <- ugarchfit(data = CORN.xts$etf_asset_error, spec = corn.base_model_spec)
corn.base_fit

robust_coef <- as.data.frame(corn.base_fit@fit[['robust.matcoef']])
write_xlsx(robust_coef, "data.xlsx")

soyb.base_model_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(4,3)))
soyb.base_fit <- ugarchfit(data = SOYB.xts$etf_asset_error, spec = soyb.base_model_spec)
soyb.base_fit

weat.base_model_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(4,3)))
weat.base_fit <- ugarchfit(data = WEAT.xts$etf_asset_error, spec = weat.base_model_spec)
weat.base_fit

uso.base_model_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(2,2)))
uso.base_fit <- ugarchfit(data = USO.xts$etf_asset_error, spec = uso.base_model_spec)
uso.base_fit

uga.base_model_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(2,2)))
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



# GARCH with External Variables
## As mentioned in the header, it is necessary to load all these in seperately, 
## For each commodity, I have two external variable xts objects, one for the 
## mean, and one for the variance equation

ols <- lm( CORN$per_ETF_return ~ CORN$per_asset_return)
summary(ols)
plot(CORN$Date, ols$residuals, type = 'l')

tsdisplay(ols$residuals)
ggAcf(ols$residuals)

CORN.mean.ext_reg <- CORN.xts[, c('per_asset_return', 'volume_return')]


# Now I want to add two dummy variables: one if $ asset is positive and one if negative
CORN.mean.ext_reg$pos_ind <-  0
CORN.mean.ext_reg$neg_ind <- 0

for(i in 1:nrow(CORN.mean.ext_reg)){
  if(CORN.mean.ext_reg$per_asset_return[i] > 0){
    CORN.mean.ext_reg$pos_ind[i] <- 1
  }
  else if(CORN.mean.ext_reg$per_asset_return[i] < 0){
    CORN.mean.ext_reg$neg_ind[i] <- 1
  }
}

CORN.mean.ext_reg$backward <- 0

for(i in 1:nrow(CORN.mean.ext_reg)){
  if(CORN$`F2(.3)`[i] - CORN$`F1(.35)`[i] < 1){
    CORN.mean.ext_reg$backward[i] <- 1
  }
}
  
  
CORN.mean.ext_reg <- as.matrix(CORN.mean.ext_reg)

# The variance equation has so many variables, I find it best to simply copy all 
# and then remove the unnecessary ones
CORN.var.ext_reg <- CORN.xts

CORN.var.ext_reg$CORN_MID  <- NULL
CORN.var.ext_reg$`F1(.35)`  <- NULL
CORN.var.ext_reg$`F2(.3)`  <- NULL
CORN.var.ext_reg$`F3(.35)`  <- NULL
CORN.var.ext_reg$CORN_NAV  <- NULL
CORN.var.ext_reg$ROLL  <- NULL
CORN.var.ext_reg$`C Jan`  <- NULL
CORN.var.ext_reg$`C 2012`  <- NULL
CORN.var.ext_reg$etf_asset_error  <- NULL
CORN.var.ext_reg$per_NAV_return  <- NULL
CORN.var.ext_reg$asset_basket  <- NULL
CORN.var.ext_reg$per_asset_return  <- NULL
CORN.var.ext_reg$Volume  <- NULL
CORN.var.ext_reg$per_ETF_return <- NULL
CORN.var.ext_reg$volume_return <- NULL

corn.full_model_spec <- ugarchspec(mean.model = list(armaOrder = c(3,0),include.mean = TRUE,
                                                    external.regressors = CORN.mean.ext_reg, archm = TRUE), 
                              variance.model = list(garchOrder = c(1,1),
                                                    external.regressors = CORN.var.ext_reg))


setbounds(corn.full_model_spec) <- list(vxreg1 = c(-100,100), vxreg2 = c(-100,100), vxreg3 = c(-100,100), vxreg4 = c(-100,100),
                                   vxreg5 = c(-100,100), vxreg6 = c(-100,100), vxreg7 = c(-100,100), vxreg8 = c(-100,100),
                                   vxreg9 = c(-100,100), vxreg10 = c(-100,100), vxreg11 = c(-100,100), vxreg12 = c(-100,100), 
                                   vxreg13 = c(-100,100), vxreg14 = c(-100,100), vxreg15 = c(-100,100), vxreg16 = c(-100,100),
                                   vxreg17 = c(-100,100), vxreg18 = c(-100,100), vxreg19 = c(-100,100), vxreg20 = c(-100,100), 
                                   vxreg21 = c(-100,100), vxreg22 = c(-100,100), vxreg23 = c(-100,100), vxreg24 = c(-100,100), 
                                   vxreg25 = c(-100,100), vxreg26 = c(-100,100), vxreg28 = c(-100,100), vxreg29 = c(-100,100),
                                   vxreg30 = c(-100,100), mxreg1 = c(-100,100), mxreg2 = c(-100,100), mxreg3 = c(-100,100),
                                   mxreg4 = c(-100,100))

corn.full_fit <- ugarchfit(data = CORN.xts$etf_asset_error, spec = corn.full_model_spec)

corn.full_fit
robust_coef <- as.data.frame(corn.full_fit@fit[['robust.matcoef']])
write_xlsx(robust_coef, "data.xlsx")


SOYB
SOYB.mean.ext_reg <- SOYB.xts[, c('per_asset_return')]
SOYB.mean.ext_reg <- as.matrix(SOYB.mean.ext_reg)

SOYB.var.ext_reg <- SOYB.xts[, c('S WASDE', 'S WASDE + CP', 'S Grain Stocks', 'S Prospective Plantings', 'S Acreage Report',
                                 'S Cattle on Feed', 'S Hogs & Pigs', 'S Day Before Roll', 'S Day After Roll', 'per_asset_return',
                                 'volume_return')]

soyb.full_model_spec <- ugarchspec(mean.model = list(armaOrder = c(4,3),include.mean = TRUE),
                                   variance.model = list(garchOrder = c(1,1),
                                                         external.regressors = SOYB.mean.ext_reg))


setbounds(soyb.full_model_spec) <- list(vxreg1 = c(-100,100), vxreg2 = c(-100,100), vxreg3 = c(-100,100), vxreg4 = c(-100,100),
                                        vxreg5 = c(-100,100), vxreg6 = c(-100,100), vxreg7 = c(-100,100), vxreg8 = c(-100,100),
                                        vxreg9 = c(-100,100), vxreg10 = c(-100,100), mxreg1 = c(-100,100), mxreg2 = c(-100,100), mxreg3 = c(-100,100),
                                        mxreg4 = c(-100,100))

soyb.full_fit <- ugarchfit(data = SOYB.xts$etf_asset_error, spec = soyb.full_model_spec)
soyb.full_fit


# WEAT
# , 'volume_return'
WEAT.mean.ext_reg <- WEAT.xts[, c('per_asset_return', 'volume_return')]
WEAT.mean.ext_reg$pos_ind <-  0
WEAT.mean.ext_reg$neg_ind <- 0

for(i in 1:nrow(WEAT.mean.ext_reg)){
  if(WEAT.mean.ext_reg$per_asset_return[i] > 0){
    WEAT.mean.ext_reg$pos_ind[i] <- 1
  }
  else if(WEAT.mean.ext_reg$per_asset_return[i] < 0){
    WEAT.mean.ext_reg$neg_ind[i] <- 1
  }
}

WEAT.mean.ext_reg$backward <- 0

for(i in 1:nrow(WEAT.mean.ext_reg)){
  if(WEAT$`F2(.3)`[i] - WEAT$`F1(.35)`[i] < 1){
    WEAT.mean.ext_reg$backward[i] <- 1
  }
}



WEAT.mean.ext_reg <- as.matrix(WEAT.mean.ext_reg)

WEAT.var.ext_reg <- WEAT.xts
WEAT.var.ext_reg$WEAT_MID  <- NULL
WEAT.var.ext_reg$`F1(.35)`  <- NULL
WEAT.var.ext_reg$`F2(.3)`  <- NULL
WEAT.var.ext_reg$`F3(.35)`  <- NULL
WEAT.var.ext_reg$WEAT_NAV  <- NULL
WEAT.var.ext_reg$ROLL  <- NULL
WEAT.var.ext_reg$`W Jan`  <- NULL
WEAT.var.ext_reg$`W 2012`  <- NULL
WEAT.var.ext_reg$etf_asset_error  <- NULL
WEAT.var.ext_reg$per_NAV_return  <- NULL
WEAT.var.ext_reg$asset_basket  <- NULL
WEAT.var.ext_reg$per_asset_return  <- NULL
WEAT.var.ext_reg$Volume  <- NULL
WEAT.var.ext_reg$per_ETF_return <- NULL
WEAT.var.ext_reg$volume_return <- NULL

weat.full_model_spec <- ugarchspec(mean.model = list(armaOrder = c(4,3),include.mean = TRUE,
                                                     external.regressors = WEAT.mean.ext_reg, archm = TRUE), 
                                   variance.model = list(garchOrder = c(1,1),
                                                         external.regressors = WEAT.var.ext_reg))


setbounds(weat.full_model_spec) <- list(vxreg1 = c(-100,100), vxreg2 = c(-100,100), vxreg3 = c(-100,100), vxreg4 = c(-100,100),
                                        vxreg5 = c(-100,100), vxreg6 = c(-100,100), vxreg7 = c(-100,100), vxreg8 = c(-100,100),
                                        vxreg9 = c(-100,100), vxreg10 = c(-100,100), vxreg11 = c(-100,100), vxreg12 = c(-100,100), 
                                        vxreg13 = c(-100,100), vxreg14 = c(-100,100), vxreg15 = c(-100,100), vxreg16 = c(-100,100),
                                        vxreg17 = c(-100,100), vxreg18 = c(-100,100), vxreg19 = c(-100,100), vxreg20 = c(-100,100), 
                                        vxreg21 = c(-100,100), vxreg22 = c(-100,100), vxreg23 = c(-100,100), vxreg24 = c(-100,100), 
                                        vxreg25 = c(-100,100), vxreg26 = c(-100,100), vxreg28 = c(-100,100), vxreg29 = c(-100,100),
                                        vxreg30 = c(-100,100), mxreg1 = c(-100,100), mxreg2 = c(-100,100), mxreg3 = c(-100,100),
                                        mxreg4 = c(-100,100))

weat.full_fit <- ugarchfit(data = WEAT.xts$etf_asset_error, spec = weat.full_model_spec)
weat.full_fit

robust_coef <- as.data.frame(weat.full_fit@fit[['robust.matcoef']])
write_xlsx(robust_coef, "data.xlsx")

# USO

USO.mean.ext_reg <- USO.xts[,c('per_asset_return')]

USO.mean.ext_reg$pos_ind <- 0
USO.mean.ext_reg$neg_ind <- 0

for(i in 1:nrow(USO.mean.ext_reg)){
  if(USO.mean.ext_reg$per_asset_return[i] > 0){
    USO.mean.ext_reg$pos_ind[i] <- 1
  }
  else if(USO.mean.ext_reg$per_asset_return[i] < 0){
    USO.mean.ext_reg$neg_ind[i] <- 1
  }
}

USO.mean.ext_reg <- as.matrix(USO.mean.ext_reg)

USO.var.ext_reg <- USO.xts
USO.var.ext_reg$Date <- NULL
USO.var.ext_reg$USO_MID <- NULL
USO.var.ext_reg$Futures <- NULL
USO.var.ext_reg$USO_NAV <- NULL
USO.var.ext_reg$ROLL <- NULL
USO.var.ext_reg$`CL Jan` <- NULL
USO.var.ext_reg$`CL 2013` <- NULL
USO.var.ext_reg$`CL 2014` <- NULL
USO.var.ext_reg$`CL 2020` <- NULL
USO.var.ext_reg$etf_asset_error<- NULL
USO.var.ext_reg$per_NAV_return <- NULL
USO.var.ext_reg$per_ETF_return <- NULL
USO.var.ext_reg$Volume <- NULL
USO.var.ext_reg$DATE <- NULL
#USO.var.ext_reg$volume_return <- NULL
USO.var.ext_reg$per_asset_return <- NULL

uso.full_model_spec <- ugarchspec(mean.model = list(armaOrder = c(2,1), include.mean = TRUE, archm = TRUE,
                                                    external.regressors = USO.mean.ext_reg),
                                  variance.model = list(garchOrder = c(1,1), 
                                                        external.regressors = USO.var.ext_reg))


setbounds(uso.full_model_spec) <- list(vxreg1 = c(-100,100), vxreg2 = c(-100,100), vxreg3 = c(-100,100), vxreg4 = c(-100,100),
                                        vxreg5 = c(-100,100), vxreg6 = c(-100,100), vxreg7 = c(-100,100), vxreg8 = c(-100,100),
                                        vxreg9 = c(-100,100), vxreg10 = c(-100,100), vxreg11 = c(-100,100), vxreg12 = c(-100,100), 
                                        vxreg13 = c(-100,100), vxreg14 = c(-100,100), vxreg15 = c(-100,100), vxreg16 = c(-100,100),
                                        vxreg17 = c(-100,100), vxreg18 = c(-100,100), vxreg19 = c(-100,100), vxreg20 = c(-100,100), 
                                        vxreg21 = c(-100,100), vxreg22 = c(-100,100), vxreg23 = c(-100,100), vxreg24 = c(-100,100), 
                                        vxreg25 = c(-100,100), vxreg26 = c(-100,100), vxreg28 = c(-100,100), vxreg29 = c(-100,100),
                                        vxreg30 = c(-100,100), mxreg1 = c(-100,100), mxreg2 = c(-100,100), mxreg3 = c(-100,100),
                                        mxreg4 = c(-100,100))


uso.full_fit <- ugarchfit(data = USO.xts$etf_asset_error, spec = uso.full_model_spec)
uso.full_fit

robust_coef <- as.data.frame(uso.full_fit@fit[['robust.matcoef']])
write_xlsx(robust_coef, "data.xlsx")







# UGA


UGA.mean.ext_reg <- UGA.xts[,c('per_asset_return', 'volume_return')]

UGA.mean.ext_reg$pos_ind <- 0
UGA.mean.ext_reg$neg_ind <- 0

for(i in 1:nrow(UGA.mean.ext_reg)){
  if(UGA.mean.ext_reg$per_asset_return[i] > 0){
    UGA.mean.ext_reg$pos_ind[i] <- 1
  }
  else if(UGA.mean.ext_reg$per_asset_return[i] < 0){
    UGA.mean.ext_reg$neg_ind[i] <- 1
  }
}

UGA.mean.ext_reg <- as.matrix(UGA.mean.ext_reg)

UGA.var.ext_reg <- UGA.xts
UGA.var.ext_reg$Date <- NULL
UGA.var.ext_reg$UGA_MID <- NULL
UGA.var.ext_reg$Futures <- NULL
UGA.var.ext_reg$UGA_NAV <- NULL
UGA.var.ext_reg$ROLL <- NULL
UGA.var.ext_reg$`RB Jan` <- NULL
UGA.var.ext_reg$`RB 2012` <- NULL
UGA.var.ext_reg$etf_asset_error<- NULL
UGA.var.ext_reg$per_NAV_return <- NULL
UGA.var.ext_reg$per_ETF_return <- NULL
UGA.var.ext_reg$Volume <- NULL
UGA.var.ext_reg$DATE <- NULL
UGA.var.ext_reg$volume_return <- NULL
UGA.var.ext_reg$per_asset_return <- NULL

#UGA.var.ext_reg <- as.matrix(UGA.var.ext_reg)

uga.full_model_spec <- ugarchspec(mean.model = list(armaOrder = c(3,1), include.mean = TRUE, external.regressors = UGA.mean.ext_reg, archm = TRUE),
                                  variance.model = list(garchOrder = c(1,1), external.regressors = UGA.var.ext_reg))

setbounds(uga.full_model_spec) <- list(vxreg1 = c(-100,100), vxreg2 = c(-100,100), vxreg3 = c(-100,100), vxreg4 = c(-100,100),
                                       vxreg5 = c(-100,100), vxreg6 = c(-100,100), vxreg7 = c(-100,100), vxreg8 = c(-100,100),
                                       vxreg9 = c(-100,100), vxreg10 = c(-100,100), vxreg11 = c(-100,100), vxreg12 = c(-100,100),
                                       vxreg13 = c(-100,100), vxreg14 = c(-100,100), vxreg15 = c(-100,100), vxreg16 = c(-100,100),
                                       vxreg17 = c(-100,100), vxreg18 = c(-100,100), vxreg19 = c(-100,100), vxreg20 = c(-100,100),
                                       vxreg21 = c(-100,100), vxreg22 = c(-100,100), vxreg23 = c(-100,100), vxreg24 = c(-100,100),
                                       vxreg25 = c(-100,100), vxreg26 = c(-100,100), vxreg28 = c(-100,100), vxreg29 = c(-100,100),
                                       vxreg30 = c(-100,100), mxreg1 = c(-100,100), mxreg2 = c(-100,100), mxreg3 = c(-100,100),
                                       mxreg4 = c(-100,100))


# Fit the model and display results
uga.full_fit <- ugarchfit(data = UGA.xts$etf_asset_error, spec = uga.full_model_spec)
uga.full_fit


robust_coef <- as.data.frame(uga.full_fit@fit[['robust.matcoef']])
write_xlsx(robust_coef, "data.xlsx")



