rm(list = ls())

library(readxl)
# library(tidyverse)
# library(ggthemes)# library(writexl)# library(parallel)
library(xts)
# library(tidyr)
library(zoo)
# library(rugarch)
# library(fpp2)
library(lubridate)
library(tseries)

## Set working direction to this R file's location
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path)) 
getwd()

## Needed to move 1 level back from wd (to get to Data)
(path <- dirname(getwd())) 

#---- Notes
# I know what you are asking: why am I still importing all this data seperately? The answer has to do with the external variables. 
# For each commodity, the prices during the roll period are taken out and then forward filled. In order to maintain the validity of 
# the report variables, they need to also excluded during the roll period. Because roll periods are different for each commodity, 
# the data needs to be pull seperately.


#-----------------Import Data from Excel and order------------#
mydat <- paste0(path,"/Data/Data_Update.xlsx")
CORN <- read_xlsx(mydat, sheet = "CORN")
SOYB <- read_excel(mydat, sheet = "SOYB")
WEAT <- read_excel(mydat, sheet = "WEAT")
USO <- read_excel(mydat, sheet = "USO")
UGA <- read_excel(mydat, sheet = "UGA")

#------------------Date Manipulation and Cleaning---------------#
ws <- rbind(0.35,0.3,0.35)
fs <- c("F1(.35)","F2(.3)","F3(.35)")

### CORN

# Reverse the chronological order
CORN <- CORN[nrow(CORN):1,]  
# Create the Asset Basket
CORN$asset_basket <- as.matrix(CORN[,fs]) %*% ws

# Calculate Asset and ETF Return                     
CORN$per_asset_return <- log(CORN$asset_basket/lag(CORN$asset_basket))* 100
CORN$per_ETF_return <- log(CORN$CORN_MID/lag(CORN$CORN_MID)) * 100

### SOYBEANS

# Reverse chronological order
SOYB <- SOYB[nrow(SOYB):1,]  
# Create the Asset Basket
SOYB$asset_basket <- as.matrix(SOYB[,fs]) %*% ws

# Calculate Asset and ETF Return  
SOYB$per_asset_return <- log(SOYB$asset_basket / lag(SOYB$asset_basket)) * 100
SOYB$per_ETF_return <- log(SOYB$SOYB_MID / lag(SOYB$SOYB_MID)) * 100

### WHEAT

# Reverse chronological order
WEAT <- WEAT[nrow(WEAT):1,]  

# Create the Asset Basket
WEAT$asset_basket <- as.matrix(WEAT[,fs]) %*% ws

# Calculate Asset and ETF Return  
WEAT$per_asset_return <- log(WEAT$asset_basket/lag(WEAT$asset_basket))*100 
WEAT$per_ETF_return <- log(WEAT$WEAT_MID/lag(WEAT$WEAT_MID))*100

### USO

# Reverse chronological order
USO <- USO[nrow(USO):1,]  

# Calculate Asset and ETF Return  
USO$per_asset_return <- log(USO$Futures/lag(USO$Futures)) * 100
USO$per_ETF_return <- log(USO$USO_MID /lag(USO$USO_MID)) * 100

### UGA

# Reverse chronological order
UGA <- UGA[nrow(UGA):1,]  

# Calculate Asset and ETF Return  
UGA$per_asset_return <- log(UGA$Futures/lag(UGA$Futures)) * 100
UGA$per_ETF_return <- log(UGA$UGA_MID/lag(UGA$UGA_MID)) * 100

###---------------------------------------------------------------
## Volume data ---------------------------------------------------
###---------------------------------------------------------------
volume <- read.csv(paste0(path,"/Data/Volume.csv"))
volume[,3:7] <- volume[,3:7] + 1

### CORN
# subset the dataframe to relevant columns
corn.volume <- data.frame("DATE" = as.POSIXct(volume$DATE,
               tz = "UTC"), "Volume" = volume$CORN.Volume)

# Merge the Volume data with the other data
CORN <- merge(CORN, corn.volume, by = "DATE")

# calculate percent change in volume
CORN$volume_return <-log(CORN$Volume /lag(CORN$Volume)) * 100

### SOYB
# subset the dataframe to relevant columns
soyb.volume <- data.frame("DATE" = as.POSIXct(volume$DATE,
              tz = "UTC"), "Volume" = volume$SOYB.Volume)

# Merge the Volume data with the other data
SOYB <- merge(SOYB, soyb.volume, by = "DATE")

# Note: SOme Volume were 0: which(SOYB$Volume==0) yields INf
# calculate percent change in volume
SOYB$volume_return <-log((SOYB$Volume)/lag(SOYB$Volume)) * 100

### WEAT
# subset the dataframe to relevant columns
weat.volume <- data.frame("DATE" = as.POSIXct(volume$DATE,
              tz = "UTC"), "Volume" = volume$WEAT.Volume)

# Merge the Volume data with the other data
WEAT <- merge(WEAT, weat.volume, by = "DATE")

# calculate percent change in volume
WEAT$volume_return <-log(WEAT$Volume/lag(WEAT$Volume)) * 100

### USO
# subset the dataframe to relevant columns
uso.volume <- data.frame("DATE" = as.POSIXct(volume$DATE,
              tz = "UTC"), "Volume" = volume$USO.Volume)

# Merge the Volume data with the other data
USO <- merge(USO, uso.volume, by = "DATE")

# calculate percent change in volume
USO$volume_return <-log(USO$Volume/lag(USO$Volume)) * 100

### UGA
# subset the dataframe to relevant columns
uga.volume <-data.frame("DATE" = as.POSIXct(volume$DATE,
             tz = "UTC"), "Volume" = volume$UGA.Volume)

# Merge the Volume data with the other data
UGA <- merge(UGA, uga.volume, by = "DATE")

# calculate percent change in volume
UGA$volume_return <-log(UGA$Volume/lag(UGA$Volume)) * 100

###---------------------------------------------------------------
## More Data Cleaning --------------------------------------------
###---------------------------------------------------------------

# The code below handles the issue of roll dates 

CORN <- na.omit(CORN)  
CORN <- CORN[!(CORN$ROLL == 1),] # directly remove roll
CORN.xts <- xts(CORN[,-1], order.by = CORN$DATE)

SOYB <- na.omit(SOYB)
SOYB <- SOYB[!(SOYB$ROLL == 1),]
SOYB.xts <- xts(SOYB[,-1], order.by = SOYB$DATE) 

WEAT <- na.omit(WEAT) 
WEAT <- WEAT[!(WEAT$ROLL == 1),]
WEAT.xts <- xts(WEAT[,-1], order.by = WEAT$DATE)

USO <- na.omit(USO)
USO <- USO[!(USO$ROLL == 1),]
USO.xts <- as.xts(USO, order.by = USO$DATE)

UGA <- na.omit(UGA) 
UGA <- UGA[!(UGA$ROLL == 1),]
UGA.xts <- xts(UGA[,-1], order.by = UGA$DATE)

###---------------------------------------------------------------
##  -------------- LM OLS   --------------------------------------
###---------------------------------------------------------------

corn.ols <- lm(per_ETF_return ~ per_asset_return, data = CORN)
CORN$etf_asset_error <- corn.ols$residuals  
summary(corn.ols)
adf_corn <- adf.test(corn.ols$residuals)

soyb.ols <- lm(per_ETF_return ~ per_asset_return, data = SOYB)
SOYB$etf_asset_error <- soyb.ols$residuals 
summary(soyb.ols)
adf_soyb <-adf.test(soyb.ols$residuals)

weat.ols <- lm(per_ETF_return ~ per_asset_return, data = WEAT)
WEAT$etf_asset_error <- weat.ols$residuals
summary(weat.ols)
adf_weat <- adf.test(weat.ols$residuals)

uso.ols <- lm(per_ETF_return ~ per_asset_return, data = USO)
USO$etf_asset_error <- uso.ols$residuals
summary(uso.ols)
adf_uso <- adf.test(uso.ols$residuals)

uga.ols <- lm(per_ETF_return ~ per_asset_return, data = UGA)
UGA$etf_asset_error <- uga.ols$residuals
summary(uga.ols)
adf_uga <- adf.test(uga.ols$residuals)

c("corn" = adf_corn$p.value, "soyb" = adf_soyb$p.value,
  "weat" = adf_weat$p.value, "uso" = adf_uso$p.value, 
  "uga" = adf_uga$p.value)

#--- ETF Returns

plot(CORN$per_asset_return, CORN$per_ETF_return)
abline(corn.ols, col = "red")

pdf(paste0(path,"/Figures/ETF_Return.pdf"),height = 8,
    width = 10)
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN$DATE, CORN$per_ETF_return, type = "l", main = "CORN",
     xlab = "", ylab = "")
plot(SOYB$DATE, SOYB$per_ETF_return, type = "l", main = "SOYB",
     xlab = "", ylab = "")
plot(WEAT$DATE, WEAT$per_ETF_return, type = "l", main = "WEAT",
     xlab = "", ylab = "Percent Return")
plot(USO$DATE, USO$per_ETF_return, type = "l", main = "USO", 
     xlab = "", ylab = "") 
plot(UGA$DATE, UGA$per_ETF_return, type = "l", main = "UGA",
     xlab = "", ylab = "") 
dev.off()

#-- Asset Returns
pdf(paste0(path,"/Figures/Asset_Return.pdf"),height = 8,
    width = 10)
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN$DATE, CORN$per_asset_return, type = "l", main = "CORN",
     xlab = "", ylab = "")
plot(SOYB$DATE, SOYB$per_asset_return, type = "l", main = "SOYB",
     xlab = "", ylab = "")
plot(WEAT$DATE, WEAT$per_asset_return, type = "l", main = "WEAT",
     xlab = "", ylab = "Percent Return")
plot(USO$DATE, USO$per_asset_return, type = "l", main = "USO", 
     xlab = "", ylab = "")
plot(UGA$DATE, UGA$per_asset_return, type = "l", main = "UGA",
     xlab = "", ylab = "")
dev.off()

#--Tracking Error
pdf(paste0(path,"/Figures/Tracking_Error.pdf"),height = 8,
    width = 10)
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN$DATE, CORN$etf_asset_error, type = "l", main = "CORN",
     xlab = "", ylab = "")
plot(SOYB$DATE, SOYB$etf_asset_error, type = "l", main = "SOYB",
     xlab = "", ylab = "")
plot(WEAT$DATE, WEAT$etf_asset_error, type = "l", main = "WEAT",
     xlab = "", ylab = "Percent Error")
plot(USO$DATE, USO$etf_asset_error, type = "l", main = "USO", 
     xlab = "", ylab = "")
plot(UGA$DATE, UGA$etf_asset_error, type = "l", main = "UGA",
     xlab = "", ylab = "")
dev.off()

#--Squared Tracking Error

pdf(paste0(path,"/Figures/Squared_Tracking_Error.pdf"),height = 8,
    width = 10)
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN$DATE, CORN$etf_asset_error^2, type = "l", main = "CORN",
     xlab = "", ylab = "")
plot(SOYB$DATE, SOYB$etf_asset_error^2, type = "l", main = "SOYB",
     xlab = "", ylab = "")
plot(WEAT$DATE, WEAT$etf_asset_error^2, type = "l", main = "WEAT",
     xlab = "", ylab = "Percent Error Squared")
plot(USO$DATE, USO$etf_asset_error^2, type = "l", main = "USO", 
     xlab = "", ylab = "")
plot(UGA$DATE, UGA$etf_asset_error^2, type = "l", main = "UGA",
     xlab = "", ylab = "")
dev.off()

ggtsdisplay(SOYB$etf_asset_error)


###---------------------------------------------------------------
##  -------------- Base Garch Models -----------------------------
###---------------------------------------------------------------

corn.base_model_spec <- ugarchspec(variance.model = list( garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(2,1)))
corn.base_fit <- ugarchfit(data = CORN$etf_asset_error, spec = corn.base_model_spec, solver = 'hybrid')
corn.base_fit

soyb.base_model_spec <- ugarchspec(variance.model = list( garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(0,1)))
soyb.base_fit <- ugarchfit(data = SOYB$etf_asset_error, spec = soyb.base_model_spec,
                           solver = 'hybrid')
soyb.base_fit

weat.base_model_spec <- ugarchspec(variance.model = list( garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(1,5)))
weat.base_fit <- ugarchfit(data = WEAT$etf_asset_error, spec = weat.base_model_spec, 
                           solver = 'hybrid')
weat.base_fit

uso.base_model_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(2,1)))
uso.base_fit <- ugarchfit(data = USO$etf_asset_error, spec = uso.base_model_spec, 
                          solver = 'hybrid')
uso.base_fit


uga.base_model_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(1,2)))
uga.base_fit <- ugarchfit(data = UGA$etf_asset_error, spec = uga.base_model_spec,
                          solver = 'hybrid')
uga.base_fit

?ugarchfit
#--------------------------------------------------------------------------------
# Base GARCH Conditional Variance
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN$DATE, corn.base_fit@fit[['sigma']], type = "l", main = "CORN",
     xlab = "", ylab = "")
plot(SOYB$DATE, soyb.base_fit@fit[['sigma']], type = "l", main = "SOYB",
     xlab = "", ylab = "")
plot(WEAT$DATE, weat.base_fit@fit[['sigma']], type = "l", main = "WEAT",
     xlab = "", ylab = "Sigma")
plot(USO$DATE, uso.base_fit@fit[['sigma']], type = "l", main = "USO", 
     xlab = "", ylab = "")
plot(UGA$DATE, uga.base_fit@fit[['sigma']], type = "l", main = "UGA",
     xlab = "", ylab = "")

# Squared Tracking error vs Base GARCH Conditional Variance
# par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
# plot(CORN$etf_asset_error^2, corn.base_fit@fit[['sigma']]^2, main = "CORN",
#      xlab = "", ylab = "")
# plot(SOYB$etf_asset_error^2, soyb.base_fit@fit[['sigma']]^2, main = "SOYB",
#      xlab = "", ylab = "")
# plot(WEAT$etf_asset_error^2, weat.base_fit@fit[['sigma']]^2 , main = "WEAT",
#      xlab = "", ylab = "Sigma")
# plot(USO$etf_asset_error^2, uso.base_fit@fit[['sigma']]^2, main = "USO", 
#      xlab = "", ylab = "")
# plot(UGA$etf_asset_error^2, uga.base_fit@fit[['sigma']]^2, main = "UGA",
#      xlab = "", ylab = "")


# Base GARCH Residuals
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN$DATE, corn.base_fit@fit[['residuals']], type = "l", main = "CORN",
     xlab = "", ylab = "")
plot(SOYB$DATE, soyb.base_fit@fit[['residuals']], type = "l", main = "SOYB",
     xlab = "", ylab = "")
plot(WEAT$DATE, weat.base_fit@fit[['residuals']], type = "l", main = "WEAT",
     xlab = "", ylab = "Model Residuals")
plot(USO$DATE, uso.base_fit@fit[['residuals']], type = "l", main = "USO", 
     xlab = "", ylab = "")
plot(UGA$DATE, uga.base_fit@fit[['residuals']], type = "l", main = "UGA",
     xlab = "", ylab = "")


# Base GARCH Squared Residuals 
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN$DATE, corn.base_fit@fit[['residuals']]^2, type = "l", main = "CORN",
     xlab = "", ylab = "")
plot(SOYB$DATE, soyb.base_fit@fit[['residuals']]^2, type = "l", main = "SOYB",
     xlab = "", ylab = "")
plot(WEAT$DATE, weat.base_fit@fit[['residuals']]^2, type = "l", main = "WEAT",
     xlab = "", ylab = "Model Residuals Squared")
plot(USO$DATE, uso.base_fit@fit[['residuals']]^2, type = "l", main = "USO", 
     xlab = "", ylab = "")
plot(UGA$DATE, uga.base_fit@fit[['residuals']]^2, type = "l", main = "UGA",
     xlab = "", ylab = "")

# Base GARCH Residual diagnostic
corn.s <- corn.base_fit@fit[['residuals']] / corn.base_fit@fit[['sigma']]
soyb.s <- soyb.base_fit@fit[['residuals']] / soyb.base_fit@fit[['sigma']]
weat.s <- weat.base_fit@fit[['residuals']] / weat.base_fit@fit[['sigma']]
uso.s <- uso.base_fit@fit[['residuals']] / uso.base_fit@fit[['sigma']]
uga.s <- uga.base_fit@fit[['residuals']] / uga.base_fit@fit[['sigma']]


# Base Standardized GARCH Residuals
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN$DATE, corn.s, type = "l", main = "CORN",
     xlab = "", ylab = "")
plot(SOYB$DATE, soyb.s, type = "l", main = "SOYB",
     xlab = "", ylab = "")
plot(WEAT$DATE, weat.s, type = "l", main = "WEAT",
     xlab = "", ylab = "Standardized Residuals")
plot(USO$DATE, uso.s, type = "l", main = "USO", 
     xlab = "", ylab = "")
plot(UGA$DATE, uga.s, type = "l", main = "UGA",
     xlab = "", ylab = "")

# Base Standardized GARCH Squared Residuals 
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN$DATE, corn.s^2, type = "l", main = "CORN",
     xlab = "", ylab = "")
plot(SOYB$DATE, soyb.s^2, type = "l", main = "SOYB",
     xlab = "", ylab = "")
plot(WEAT$DATE, weat.s^2, type = "l", main = "WEAT",
     xlab = "", ylab = "Standardized Residuals Squared")
plot(USO$DATE, uso.s^2, type = "l", main = "USO", 
     xlab = "", ylab = "")
plot(UGA$DATE, uga.s^2, type = "l", main = "UGA",
     xlab = "", ylab = "")



# GARCH with External Variables
## As mentioned in the header, it is necessary to load all these in seperately, 
## For each commodity, I have two external variable xts objects, one for the 
## mean, and one for the variance equation


CORN.mean.ext_reg <- CORN.xts[, c('per_asset_return')]
CORN.mean.ext_reg$pos_ind <-  0

for(i in 1:nrow(CORN.mean.ext_reg)){
  if(CORN.mean.ext_reg$per_asset_return[i] > 0){
    CORN.mean.ext_reg$pos_ind[i] <- 1
  }
}

CORN.mean.ext_reg$backward <- 0

for(i in 1:nrow(CORN.mean.ext_reg)){
  if(CORN$`F2(.3)`[i] - CORN$`F1(.35)`[i] < 1){
    CORN.mean.ext_reg$backward[i] <- 1
  }
}
CORN.mean.ext_reg <- as.matrix(CORN.mean.ext_reg)


CORN.var.ext_reg <- CORN.xts
# Include months and years:  c(1,2,3,4,5,6,9,21,37,38,39,40,42)
# No months and years: 
# c(1,2,3,4,5,6,9,10,11,12,13,14,15,16,17,
#   18,19,20,21,22,23,24,25,26,27,28,29,37,38,39,40,42)

colnames(CORN.xts)
cnames_corn <- colnames(CORN.xts)[-c(1,2,3,4,5,6,9,21,37,38,39,40,42)]
CORN.var.ext_reg <- CORN.xts[,cnames_corn]

corn.full_model_spec <- ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = TRUE,
                                                     external.regressors = CORN.mean.ext_reg),
                                   variance.model = list(garchOrder = c(1,1),
                                                    external.regressors = CORN.var.ext_reg))

#cl = makePSOCKcluster(2)
corn.full_fit <- ugarchfit(data = CORN$etf_asset_error, spec = corn.full_model_spec, 
                           solver = 'hybrid', cluster = cl)
#stopCluster(cl)
corn.full_fit

robust_coef <- as.data.frame(corn.full_fit@fit[['robust.matcoef']])
write_xlsx(robust_coef, "data.xlsx")


#------------------------------------------

#SOYB

summary(data.frame(SOYB$per_asset_return, SOYB$per_ETF_return, SOYB$asset_basket, SOYB$SOYB_MID, SOYB$etf_asset_error))

summary(CORN$etf_asset_error)
dev.off()
hist(CORN$etf_asset_error, breaks = seq(-1.5,2,.1))
var(CORN$etf_asset_error)
var(SOYB$etf_asset_error)

SOYB.mean.ext_reg <- SOYB.xts[, c('per_asset_return')]

SOYB.mean.ext_reg$backward <- 0

for(i in 1:nrow(SOYB.mean.ext_reg)){
  if(SOYB$`F2(.3)`[i] - SOYB$`F1(.35)`[i] < 1){
    SOYB.mean.ext_reg$backward[i] <- 1
  }
}

SOYB.mean.ext_reg <- as.matrix(SOYB.mean.ext_reg)

SOYB.var.ext_reg <- SOYB.xts[, c('S WASDE', 'S WASDE + CP', 'S Grain Stocks', 'S Prospective Plantings', 'S Acreage Report',
                                 'S Cattle on Feed', 'S Hogs & Pigs', 'S Day Before Roll', 'S Day After Roll', 'per_asset_return',
                                 'volume_return')]
SOYB.var.ext_reg <- as.matrix(SOYB.var.ext_reg)

# mean.model = list(armaOrder = c(0,0),include.mean = TRUE),
# 
soyb.full_model_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)))#, external.regressors = as.matrix(SOYB.var.ext_reg[,(10:11)])))                                                

soyb.full_fit <- ugarchfit(data = SOYB$etf_asset_error, spec = soyb.full_model_spec, solver = 'hybrid')
                           
soyb.full_fit

#-----------------------------------------------
# WEAT

WEAT.mean.ext_reg <- WEAT.xts[, c('per_asset_return')]
WEAT.mean.ext_reg$pos_ind <-  0

 for(i in 1:nrow(WEAT.mean.ext_reg)){
   if(WEAT.mean.ext_reg$per_asset_return[i] > 0){
     WEAT.mean.ext_reg$pos_ind[i] <- 1
   }
}

WEAT.mean.ext_reg$backward <- 0

for(i in 1:nrow(WEAT.mean.ext_reg)){
  if(WEAT$`F2(.3)`[i] - WEAT$`F1(.35)`[i] < 1){
    WEAT.mean.ext_reg$backward[i] <- 1
  }
}

WEAT.mean.ext_reg <- as.matrix(WEAT.mean.ext_reg)

# With Months and Years: -c(1,2,3,4,5,6,9,21,37,38,40)
# Without Months and Years:
# 
# c(1,2,3,4,5,6,9,10,11,12,13,14,15,16,17,18,19,20,21,
#   22,23,24,25,26,27,28,29,37,38,40)

colnames(WEAT.xts)
cnames_weat <- colnames(WEAT.xts)[-c(1,2,3,4,5,6,9,21,37,38,39,40)]
cnames_weat

WEAT.var.ext_reg <- WEAT.xts[,cnames_weat]

weat.full_model_spec <- ugarchspec(mean.model = list(armaOrder = c(0,0),external.regressors = WEAT.mean.ext_reg, include.mean = TRUE), 
                                   variance.model = list(garchOrder = c(1,1),
                                                         external.regressors = WEAT.var.ext_reg))

weat.full_fit <- ugarchfit(data = WEAT$etf_asset_error, spec = weat.full_model_spec, 
                           solver = 'hybrid')
weat.full_fit

robust_coef <- as.data.frame(weat.full_fit@fit[['robust.matcoef']])
write_xlsx(robust_coef, "data.xlsx")

#---------------------------------------------------------
# USO

USO.mean.ext_reg <- USO.xts[,c('per_asset_return')]

USO.mean.ext_reg$pos_ind <- 0
for(i in 1:nrow(USO.mean.ext_reg)){
  if(USO.mean.ext_reg$per_asset_return[i] > 0){
    USO.mean.ext_reg$pos_ind[i] <- 1
  }
}

USO.mean.ext_reg <- as.matrix(USO.mean.ext_reg)

#With Months and Years: c(1,2,3,4,5,8,20,32,33,34)
#Without Months and Years: 

cnames_uso <- colnames(USO.xts)[-c(1,2,3,4,5,8,20,32,33,34)]
cnames_uso

USO.var.ext_reg <- USO.xts[,cnames_uso]

uso.full_model_spec <- ugarchspec(mean.model = list(armaOrder = c(0,0),external.regressors = USO.mean.ext_reg ),
                                  variance.model = list(garchOrder = c(1,1), 
                                                        external.regressors = USO.var.ext_reg))

uso.full_fit <- ugarchfit(data = USO$etf_asset_error, spec = uso.full_model_spec, 
                          solver = 'hybrid')
uso.full_fit

robust_coef <- as.data.frame(uso.full_fit@fit[['robust.matcoef']])
write_xlsx(robust_coef, "data.xlsx")

#--------------------------------------------------------
# UGA

UGA.mean.ext_reg <- UGA.xts[,c('per_asset_return')]

UGA.mean.ext_reg$pos_ind <- 0
 
for(i in 1:nrow(UGA.mean.ext_reg)){
 if(UGA.mean.ext_reg$per_asset_return[i] > 0){
   UGA.mean.ext_reg$pos_ind[i] <- 1
 }
}
UGA.mean.ext_reg <- as.matrix(UGA.mean.ext_reg)

cnames_uga <- colnames(UGA.xts)[-c(1,2,3,4,7,19,32,33,34)]
cnames_uga

UGA.var.ext_reg <- UGA.xts[,cnames_uga]

UGA.var.ext_reg <- as.matrix(UGA.var.ext_reg)


# , archm = TRUE , include.mean = TRUE
uga.full_model_spec <- ugarchspec(mean.model = list(armaOrder = c(0,0), external.regressors = UGA.mean.ext_reg),
                                  variance.model = list(garchOrder = c(1,1), external.regressors = UGA.var.ext_reg))



# Fit the model and display results
uga.full_fit <- ugarchfit(data = UGA$etf_asset_error, spec = uga.full_model_spec,
                          solver = 'hybrid')
uga.full_fit


robust_coef <- as.data.frame(uga.full_fit@fit[['robust.matcoef']])
write_xlsx(robust_coef, "data.xlsx")


#----------------------------------------------------------
# Playing Around with Multivariate GARCH

# first put all the tracking errors in one dataframe

#dat <- cbind(USO$etf_asset_error, UGA$etf_asset_error)

#install.packages('rmgarch')

library(rmgarch)



