rm(list = ls())
library(readxl)
library(tidyverse)
library(ggthemes)
library(xts)
library(tidyr)
library(zoo)
library(rugarch)
library(fpp2)


#-----------------Import Data from Excel and order------------#
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
#------------------Date Manipulation and Cleaning---------------#

WEAT$DATE <- as.Date(WEAT$DATE, origin = "1899-12-30")  #set date 
WEAT <- WEAT[order(WEAT$DATE),] #order by date
WEAT$asset_basket <- (WEAT$`F1(.35)` * 0.35) + (WEAT$`F2(.3)` * 0.3) + (WEAT$`F3(.35)` * 0.35) #reconstruct asset basket
WEAT$per_asset_return <- log(WEAT$asset_basket/lag(WEAT$asset_basket))* 100 # calculate percent asset basket return
WEAT$per_ETF_return <- log(WEAT$WEAT_MID/lag(WEAT$WEAT_MID)) * 100#calculate percent ETF return
WEAT$per_NAV_return <- log(WEAT$WEAT_NAV/lag(WEAT$WEAT_NAV)) * 100#calculate percent NAV return
WEAT$etf_asset_error <- WEAT$per_ETF_return - WEAT$per_asset_return #calculate error between ETF and Asset

#-----------------------Add ETF Volume Data-----------------------#
# Import volume data from csv
volume <- read.csv("G:/My Drive/3_Massa Research/Neff Paper/Working_Folder/Volume.csv")
# subset the dataframe to only the relevant columes
volume <- data.frame(as.Date(volume$DATE), volume$WEAT.Volume)
#rename the columns
colnames(volume) <- c("DATE", "Volume")
#Merge the Volume data with the other data
WEAT <- merge(WEAT, volume, by = "DATE")
# calculate percent change in volume
WEAT$volume_return <-log(WEAT$Volume/lag(WEAT$Volume)) * 100

#----------------------More Date Manipulation----------------#
# The code below handles the issue of roll dates and using a continous time model
# with things which dont trade on the weekend. 

#Remove rows with NAsm which has the effect of deleting roll days
WEAT <- na.omit(WEAT) 
# This creates rows of NA's for all the missing days, including weekends, holidays,
# and the roll days we just deleted. Not sure why I have to create another "Date"
# column but that is the only way I could get it to work
WEAT <- WEAT %>% 
  mutate(Date = as.Date(DATE)) %>% 
  complete(Date = seq.Date(min(DATE), max(DATE), by="day"))

# Now to forward fill the date
WEAT <- na.locf(na.locf(WEAT), fromLast = TRUE)

# Remove the additional date column
WEAT <- subset(WEAT, select = -DATE)

# Convert the object into a XTS object
WEAT.xts <- xts(WEAT[,-1], order.by = WEAT$Date)

#---------------------Descriptive Statistics of Tracking Error------#

mean(WEAT$etf_asset_error, na.rm = TRUE)
sd(WEAT$etf_asset_error, na.rm = TRUE)
e1071::skewness(WEAT$etf_asset_error, na.rm = TRUE)
e1071::kurtosis(WEAT$etf_asset_error, na.rm = TRUE)
max(WEAT$etf_asset_error, na.rm = TRUE)
min(WEAT$etf_asset_error, na.rm = TRUE)

# Graphs
#-----ETF and Asset Basket Prices
etfcolor <- "black"
assetcolor <- "darkgrey"
coeff <- WEAT$asset_basket[1] / WEAT$WEAT_MID[1]
ggplot(data = WEAT, aes(x = Date)) +
  geom_line(aes(y = WEAT_MID), color = etfcolor) +
  geom_line(aes(y = asset_basket / coeff), color = assetcolor) +
  scale_y_continuous(
    name = "ETF Price ($)", 
    sec.axis = sec_axis(~.*coeff, name = "Asset Basket Price (cents per bushel)")
  ) + theme_bw() + ggtitle("WEAT ETF and Asset Basket Price") + xlab("Date") 

#-----ETF Returns
qplot(WEAT$Date, WEAT$per_ETF_return, geom = 'line') + ggtitle("WEAT: ETF % Return") + 
  ylab('Log Percent Return') + xlab('Date') + theme_bw()

#-----Asset Returns
qplot(WEAT$Date, WEAT$per_asset_return, geom = 'line') + ggtitle("WEAT: Asset Basket % Return") + 
  ylab('Log Percent Return') + xlab('Date') + theme_bw()

# Plot the ETF_ASSET_ERRORS
qplot(WEAT$Date, WEAT$etf_asset_error, geom = 'line') + ggtitle("WEAT: Tracking Error") + 
  ylab('Error') + xlab('Date') + theme_bw()
qplot(WEAT$Date, WEAT$etf_asset_error^2, geom = 'line') + ggtitle('WEAT: Squared Tracking Error') +
  ylab('Squared Error') + xlab('Date') + theme_bw()


#==========Box Jenkins Procedure===================#
#- Investigate Stationarity of Tracking Error
ggAcf(WEAT$etf_asset_error, lag.max = 650)
ggPacf(WEAT$etf_asset_error, lag.max = 650)

Box.test(WEAT$etf_asset_error)
tseries::kpss.test(WEAT$etf_asset_error) 
tseries::adf.test(WEAT$etf_asset_error)


# Fit ARIMA Models
arima100 <- arima(WEAT$etf_asset_error, order = c(1,0,0))
summary(arima100)
checkresiduals(arima100)
Box.test(arima100$residuals^2, type = 'Ljung-Box')

arima200 <- arima(WEAT$etf_asset_error, order = c(2,0,0))
summary(arima200)
checkresiduals(arima200)
Box.test(arima200$residuals^2, type = 'Ljung-Box')

arima300 <- arima(WEAT$etf_asset_error, order = c(3,0,0))
summary(arima300)
checkresiduals(arima300)
Box.test(arima300$residuals^2, lag = 1, type = 'Ljung-Box')


#==== GARCH Models

# Base
base_model_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(3,0)))
base_fit <- ugarchfit(data = WEAT.xts$etf_asset_error, spec = base_model_spec)
base_fit

# Full
ext_reg <- WEAT.xts # creates a new xts object to hold external regressors

# The code below removes all the columns which are not external regressors. 
# There must be a better way to do this
#ext_reg$Date <- NULL
ext_reg$WEAT_MID <- NULL
ext_reg$`F1(.35)` <- NULL
ext_reg$`F2(.3)` <- NULL
ext_reg$`F3(.35)` <- NULL
ext_reg$CORN_NAV <- NULL
ext_reg$ROLL <- NULL
ext_reg$`W Jan` <- NULL
ext_reg$`W 2012` <- NULL
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
full_fit <- ugarchfit(data = WEAT.xts$etf_asset_error, spec = full_model_spec)
full_fit
