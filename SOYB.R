rm(list = ls())
library(readxl)
library(tidyverse)
library(ggthemes)
library(xts)
library(tidyr)
library(zoo)
library(rugarch)
library(fpp2)


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

#-------------------Date Manipulation and Cleaning----------------------#
SOYB$DATE <- as.Date(SOYB$DATE, origin = "1899-12-30") # Set date
SOYB <- SOYB[order(SOYB$DATE),] #order by date

#-------------------------Calculate Returns and Errors------------------------------#
SOYB$asset_basket <- (SOYB$`F1(.35)` * 0.35) + (SOYB$`F2(.3)` * 0.30) + (SOYB$`F3(.35)` * 0.35)
SOYB$per_asset_return <- log(SOYB$asset_basket / lag(SOYB$asset_basket)) * 100
SOYB$per_ETF_return <- log(SOYB$SOYB_MID / lag(SOYB$SOYB_MID)) * 100
SOYB$per_NAV_return <- log(SOYB$SOYB_NAV / lag(SOYB$SOYB_NAV)) * 100
SOYB$etf_asset_error <- SOYB$per_ETF_return - SOYB$per_asset_return

#-----------------------Add ETF Volume Data-----------------------#
# Import volume data from csv
volume <- read.csv("G:/My Drive/3_Massa Research/Neff Paper/Working_Folder/Volume.csv")
# subset the dataframe to only the relevant columes
volume <- data.frame(as.Date(volume$DATE), volume$SOYB.Volume)
#rename the columns
colnames(volume) <- c("DATE", "Volume")
#Merge the Volume data with the other data
SOYB <- merge(SOYB, volume, by = "DATE")
# calculate percent change in volume 
SOYB$volume_return <- log(SOYB$Volume)

#--------More data Manipulation
SOYB <- na.omit(SOYB)
SOYB <-SOYB %>% 
  mutate(Date = as.Date(DATE)) %>%
  complete(Date = seq.Date(min(DATE), max(DATE), by = "day"))
SOYB <- na.locf(na.locf(SOYB), fromLast = TRUE) #forward fill the date
SOYB <- subset(SOYB, select = -DATE) #remove redundant date column
SOYB.xts <- xts(SOYB[,-1], order.by = SOYB$Date) #create xts object

#---------------------Descriptive Statistics of Tracking Error------#

mean(SOYB$etf_asset_error, na.rm = TRUE)
sd(SOYB$etf_asset_error, na.rm = TRUE)
e1071::skewness(SOYB$etf_asset_error, na.rm = TRUE)
e1071::kurtosis(SOYB$etf_asset_error, na.rm = TRUE)
max(SOYB$etf_asset_error, na.rm = TRUE)
min(SOYB$etf_asset_error, na.rm = TRUE)

#---------------------Exploratory Plots--------------------------------------#
#------ETF, Asset Basket Error
qplot(SOYB$Date, (SOYB$etf_asset_error * 100), geom = 'line') + theme_bw() +
  ggtitle("Daily Return Error: SOYB ETF - Asset Basket") + ylab("Error (%)") +
  xlab("Date")
#-----ETF and Asset Basket 
etfcolor <- "black"
assetcolor <- "darkgrey"
coeff <- SOYB$asset_basket[1]/SOYB$SOYB_MID[1]
ggplot(data = SOYB, aes(x = Date)) +
  geom_line(aes(y = SOYB_MID), color = etfcolor) +
  geom_line(aes(y = asset_basket / coeff), color = assetcolor) +
  scale_y_continuous(
    name = 'ETF Price ($)',
    sec.axis = sec_axis(~.*coeff, name = "Asset Basket Price (cents per bushel)")
  ) + theme_bw() + ggtitle("SOYB ETF and Asset Basket Price") + xlab("Date") 
#----Premium/Discount to NAV
qplot(SOYB$Date, ((SOYB$SOYB_MID - SOYB$SOYB_NAV)/SOYB$SOYB_NAV * 100), geom = 'line') +
  theme_bw() + ylab("Premium/Discount (%)") + xlab("Date") + ggtitle("SOYB Premium/Discount to NAV")


#==========Box Jenkins Procedure===================#
#- Investigate Stationarity of Tracking Error
ggAcf(SOYB$etf_asset_error, lag.max = 650)
ggPacf(SOYB$etf_asset_error, lag.max = 650)

Box.test(SOYB$etf_asset_error)
tseries::kpss.test(SOYB$etf_asset_error) 
tseries::adf.test(SOYB$etf_asset_error)


# Fit ARIMA Models
arima100 <- arima(SOYB$etf_asset_error, order = c(1,0,0))
summary(arima100)
checkresiduals(arima100)
Box.test(arima100$residuals^2, type = 'Ljung-Box')

arima200 <- arima(SOYB$etf_asset_error, order = c(2,0,0))
summary(arima200)
checkresiduals(arima200)
Box.test(arima200$residuals^2, type = 'Ljung-Box')

arima300 <- arima(SOYB$etf_asset_error, order = c(3,0,0))
summary(arima300)
checkresiduals(arima300)
Box.test(arima300$residuals^2, lag = 1, type = 'Ljung-Box')

arima400 <- arima(SOYB$etf_asset_error, order = c(4,0,0))
summary(arima400)
checkresiduals(arima400)
Box.test(arima400$residuals^2, lag = 1, type = 'Ljung-Box')

arima101 <- arima(SOYB$etf_asset_error, order = c(1,0,1))
summary(arima101)
checkresiduals(arima101)
Box.test(arima101$residuals^2, lag = 1, type = 'Ljung-Box')

#==== GARCH Models

# Base
base_model_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(3,0)))
base_fit <- ugarchfit(data = SOYB.xts$etf_asset_error, spec = base_model_spec)
base_fit

# Full
ext_reg <- SOYB.xts # creates a new xts object to hold external regressors

# The code below removes all the columns which are not external regressors. 
# There must be a better way to do this
#ext_reg$Date <- NULL
ext_reg$SOYB_MID <- NULL
ext_reg$`F1(.35)` <- NULL
ext_reg$`F2(.3)` <- NULL
ext_reg$`F3(.35)` <- NULL
ext_reg$SOYB_NAV <- NULL
ext_reg$ROLL <- NULL
ext_reg$`S Jan` <- NULL
ext_reg$`S 2012` <- NULL
ext_reg$etf_asset_error<- NULL
ext_reg$per_NAV_return <- NULL
ext_reg$per_ETF_return <- NULL
ext_reg$asset_basket <- NULL
ext_reg$Volume <- NULL


# Define the model
full_model_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1),
                                                    external.regressors = ext_reg),
                              mean.model = list(armaOrder = c(3,0)))

setbounds(full_model_spec) <- list(vxreg1 = c(-100,100), vxreg2 = c(-100,100), vxreg3 = c(-100,100), vxreg4 = c(-100,100),
                                   vxreg5 = c(-100,100), vxreg6 = c(-100,100), vxreg7 = c(-100,100), vxreg8 = c(-100,100),
                                   vxreg9 = c(-100,100), vxreg10 = c(-100,100), vxreg11 = c(-100,100), vxreg12 = c(-100,100), 
                                   vxreg13 = c(-100,100), vxreg14 = c(-100,100), vxreg15 = c(-100,100), vxreg16 = c(-100,100),
                                   vxreg17 = c(-100,100), vxreg18 = c(-100,100), vxreg19 = c(-100,100), vxreg20 = c(-100,100), 
                                   vxreg21 = c(-100,100), vxreg22 = c(-100,100), vxreg23 = c(-100,100), vxreg24 = c(-100,100), 
                                   vxreg25 = c(-100,100), vxreg26 = c(-100,100), vxreg28 = c(-100,100), vxreg29 = c(-100,100),
                                   vxreg30 = c(-100,100))


# Fit the model and display results
full_fit <- ugarchfit(data = SOYB.xts$etf_asset_error, spec = full_model_spec)
full_fit

