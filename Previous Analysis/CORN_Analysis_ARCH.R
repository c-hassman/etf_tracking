rm(list = ls())
library(readxl)
library(tidyverse)
library(ggthemes)
library(xts)
library(tidyr)
library(zoo)
library(rugarch)
library(skedastic)
library(fpp2)



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

#------------------Date Manipulation and Cleaning---------------#
# This is a very messy section. I need to come back and clean it up, but I can somewhat attest
# to it's effectiveness. No data loss

CORN$DATE <- as.Date(CORN$DATE, origin = "1899-12-30") 
# Note: If I import the Date column as a "Date" using readxl, it includes a timezone character
# which creates issue when I merge this df and the volume df. 
# The data is imported in excel date numbers, which is why I need to set the origin to the proper amount
CORN <- CORN[order(CORN$DATE),] # order by date

#-----------------------Calculate Returns and Errors--------------#
CORN$asset_basket <- (CORN$`F1(.35)` * 0.35) + (CORN$`F2(.3)` * 0.3) + (CORN$`F3(.35)` * 0.35) #reconstruct asset basket
CORN$per_asset_return <- log(CORN$asset_basket/lag(CORN$asset_basket))* 100 # calculate percent asset basket return
CORN$per_ETF_return <- log(CORN$CORN_MID/lag(CORN$CORN_MID)) * 100#calculate percent ETF return
CORN$per_NAV_return <- log(CORN$CORN_NAV/lag(CORN$CORN_NAV)) * 100#calculate percent NAV return
CORN$etf_asset_error <- CORN$per_ETF_return - CORN$per_asset_return #calculate error between ETF and Asset


#-----------------------Add ETF Volume Data-----------------------#
# Import volume data from csv
volume <- read.csv("G:/My Drive/3_Massa Research/Neff Paper/Working_Folder/Volume.csv")
# subset the dataframe to only the relevant columes
volume <- data.frame(as.Date(volume$DATE), volume$CORN.Volume)
#rename the columns
colnames(volume) <- c("DATE", "Volume")
 #Merge the Volume data with the other data
CORN <- merge(CORN, volume, by = "DATE")
# calculate percent change in volume
CORN$volume_return <-log(CORN$Volume/lag(CORN$Volume)) * 100
#----------------------More Date Manipulation----------------#
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

#=====ARIMA===============#
# Stationarity
ggAcf(CORN$etf_asset_error, lag.max = 650)
ggPacf(CORN$etf_asset_error, lag.max = 650)

Box.test(CORN$etf_asset_error)
tseries::kpss.test(CORN$etf_asset_error) 
tseries::adf.test(CORN$etf_asset_error)

#=====AR(1)==============#
arima100 <- arima(CORN$etf_asset_error, order = c(1,0,0))
summary(arima100)
checkresiduals(arima100)
Box.test(arima100$residuals^2, type = 'Ljung-Box')

arima200 <- arima(CORN$etf_asset_error, order = c(2,0,0))
summary(arima200)
checkresiduals(arima200)
Box.test(arima200$residuals^2, type = 'Ljung-Box')

arima300 <- arima(CORN$etf_asset_error, order = c(3,0,0))
summary(arima300)
checkresiduals(arima300)
Box.test(arima300$residuals^2, lag = 1, type = 'Ljung-Box')

arima400 <- arima(CORN$etf_asset_error, order = c(4,0,0))
summary(arima400)
checkresiduals(arima400)
Box.test(arima400$residuals^2, type = 'Ljung-Box')

arima500 <- arima(CORN$etf_asset_error, order = c(4,0,0))
summary(arima500)
checkresiduals(arima500)
Box.test(arima500$residuals^2, type = 'Ljung-Box')


arima101 <- arima(CORN$etf_asset_error, order = c(1,0,1))
summary(arima101)
checkresiduals(arima101)
Box.test(arima101$residuals^2, type = 'Ljung-Box')


#===Testing for ARCH/GARCH Effects
# Engles LM ARCH Test
FinTS::ArchTest(CORN$etf_asset_error)
# Find strong evidence of ARCH effects
FinTS::ArchTest(CORN$etf_asset_error^2)

#=====Testing different ARCH specifications

arch.order = 1:5
arch.names = paste('arch', arch.order, sep = '')
arch.list = list()
for (p in arch.order) {
  arch.spec = ugarchspec(variance.model = list(garchOrder = c(p, 0)),
                         mean.model = list(armaOrder = c(0, 0)))
  arch.fit = ugarchfit(spec = arch.spec, data = CORN$etf_asset_error,
                       solver.control = list(trace = 0))
  arch.list[[p]] = arch.fit
}
names(arch.list) = arch.names

# Add GARCH 
garch11.spec = ugarchspec (variance.model = list(garchOrder = c(1,1)),
                           mean.model = list(armaOrder = c(0,0)))
garch31.spec = ugarchspec (variance.model = list(garchOrder = c(3,1)),
                         mean.model = list(armaOrder = c(0,0)))

garch11.fit = ugarchfit(spec = garch11.spec, data = CORN$etf_asset_error,
                        solver.control = list(trace = 1))
garch31.fit = ugarchfit(spec = garch31.spec, data = CORN$etf_asset_error,
                        solver.control = list(trace = 1))

arch.list$garch11 = garch11.fit
arch.list$garch31 = garch31.fit

# Compute Information Criteria
info.mat = sapply(arch.list, infocriteria)
rownames(info.mat) = rownames(infocriteria(arch.list[[1]]))
info.mat

garch11.fit

arch3.spec = ugarchspec (variance.model = list(garchOrder = c(3,0)),
                           mean.model = list(armaOrder = c(0,0)))
arch3.fit = ugarchfit(spec = arch3.spec, data = CORN$etf_asset_error,
                        solver.control = list(trace = 1))

arch3.fit

#=========Residual Disgnostics========#
# ARCH(3)
par(mfrow = c(2,2))
plot(CORN$Date, arch3.fit@fit[['residuals']], type= 'line') + ylab('Residuals') +
  xlab('Date') + theme_bw() 
hist(arch3.fit@fit[['residuals']]) + theme_bw() 
acf(arch3.fit@fit[['residuals']]) 
qqnorm(arch3.fit@fit[['residuals']]) 
qqline(arch3.fit@fit[['residuals']])

#GARCH(1,1)
par(mfrow = c(2,2))
plot(CORN$Date, garch11.fit@fit[['residuals']], type= 'line') + ylab('Residuals') +
  xlab('Date') + theme_bw() 
hist(garch11.fit@fit[['residuals']]) + theme_bw() 
acf(garch11.fit@fit[['residuals']]) 
qqnorm(garch11.fit@fit[['residuals']]) 
qqline(garch11.fit@fit[['residuals']])


#========External Regressors==================#

ext_reg <- CORN.xts # creates a new xts object to hold external regressors

# The code below removes all the columns which are not external regressors. 
# There must be a better way to do this
#ext_reg$Date <- NULL
ext_reg$CORN_MID <- NULL
ext_reg$`F1(.35)` <- NULL
ext_reg$`F2(.3)` <- NULL
ext_reg$`F3(.35)` <- NULL
ext_reg$CORN_NAV <- NULL
ext_reg$ROLL <- NULL
ext_reg$`C Jan` <- NULL
ext_reg$`C 2012` <- NULL
ext_reg$etf_asset_error<- NULL
ext_reg$per_NAV_return <- NULL
ext_reg$per_ETF_return <- NULL
ext_reg$asset_basket <- NULL
ext_reg$Volume <- NULL


# Define the model
full_model_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1), 
                                               external.regressors = ext_reg),
                        mean.model = list(armaOrder = c(0,0)))
setbounds(model_spec) <- list(vxreg1 = c(-100,100), vxreg2 = c(-100,100), vxreg3 = c(-100,100), vxreg4 = c(-100,100),
                              vxreg5 = c(-100,100), vxreg6 = c(-100,100), vxreg7 = c(-100,100), vxreg8 = c(-100,100),
                              vxreg9 = c(-100,100), vxreg10 = c(-100,100), vxreg11 = c(-100,100), vxreg12 = c(-100,100), 
                              vxreg13 = c(-100,100), vxreg14 = c(-100,100), vxreg15 = c(-100,100), vxreg16 = c(-100,100),
                              vxreg17 = c(-100,100), vxreg18 = c(-100,100), vxreg19 = c(-100,100), vxreg20 = c(-100,100), 
                              vxreg21 = c(-100,100), vxreg22 = c(-100,100), vxreg23 = c(-100,100), vxreg24 = c(-100,100), 
                              vxreg25 = c(-100,100), vxreg26 = c(-100,100), vxreg28 = c(-100,100), vxreg29 = c(-100,100),
                              vxreg30 = c(-100,100))


# Fit the model and display results
full_fit <- ugarchfit(data = CORN.xts$etf_asset_error, spec = model_spec)
full_fit

#---------------------Descriptive Statistics of Asset Error------#

mean(CORN$etf_asset_error, na.rm = TRUE)
sd(CORN$etf_asset_error, na.rm = TRUE)
e1071::skewness(CORN$etf_asset_error, na.rm = TRUE)
e1071::kurtosis(CORN$etf_asset_error, na.rm = TRUE)
max(CORN$etf_asset_error, na.rm = TRUE)
min(CORN$etf_asset_error, na.rm = TRUE)

#===================================================================#
#-----ETF and Asset Basket Prices
etfcolor <- "black"
assetcolor <- "darkgrey"
coeff <- CORN$asset_basket[1] / CORN$CORN_MID[1]
ggplot(data = CORN, aes(x = Date)) +
  geom_line(aes(y = CORN_MID), color = etfcolor) +
  geom_line(aes(y = asset_basket / coeff), color = assetcolor) +
  scale_y_continuous(
    name = "ETF Price ($)", 
    sec.axis = sec_axis(~.*coeff, name = "Asset Basket Price (cents per bushel)")
  ) + theme_bw() + ggtitle("CORN ETF and Asset Basket Price") + xlab("Date") 

#-----ETF Returns
qplot(CORN$Date, CORN$per_ETF_return, geom = 'line') + ggtitle("CORN: ETF % Return") + 
  ylab('Log Percent Return') + xlab('Date') + theme_bw()

#-----Asset Returns
qplot(CORN$Date, CORN$per_asset_return, geom = 'line') + ggtitle("CORN: Asset Basket % Return") + 
  ylab('Log Percent Return') + xlab('Date') + theme_bw()

# Plot the ETF_ASSET_ERRORS
qplot(CORN$Date, CORN$etf_asset_error, geom = 'line') + ggtitle("CORN: Tracking Error") + 
  ylab('Error') + xlab('Date') + theme_bw()
qplot(CORN$Date, CORN$etf_asset_error^2, geom = 'line') + ggtitle('CORN: Squared Tracking Error') +
  ylab('Squared Error') + xlab('Date') + theme_bw()


#==============================================================
#--- Description of Returns



#----Augmented Dickey Fuller Test on Returns
tseries::adf.test(CORN$per_ETF_return)
tseries::adf.test(CORN$per_NAV_return)

#==================================================================
#--ARMA Model for Tracking Error

auto.arima(CORN$etf_asset_error)
acf(CORN$etf_asset_error)
pacf(CORN$etf_asset_error)
arima(CORN$etf_asset_error, order = c(3,0,3))




#--------------GARCH--------------------#
ext_reg <- CORN.xts # creates a new xts object to hold external regressors

# The code below removes all the columns which are not external regressors. 
# There must be a better way to do this
#ext_reg$Date <- NULL
ext_reg$CORN_MID <- NULL
ext_reg$`F1(.35)` <- NULL
ext_reg$`F2(.3)` <- NULL
ext_reg$`F3(.35)` <- NULL
ext_reg$CORN_NAV <- NULL
ext_reg$ROLL <- NULL
ext_reg$`C Jan` <- NULL
ext_reg$`C 2012` <- NULL
ext_reg$etf_asset_error<- NULL
ext_reg$per_NAV_return <- NULL
ext_reg$per_ETF_return <- NULL
ext_reg$asset_basket <- NULL
ext_reg$Volume <- NULL

typeof(ext_reg$`C WASDE`)
#
# Define the model
model_spec <- ugarchspec(variance.model = list(model = 'gjrGARCH', garchOrder = c(3,1), 
                                               external.regressors = ext_reg))
#model_spec <- ugarchspec(mean.model = list(armaOrder = c(3,0)))
# Fit the model and display results
fit <- ugarchfit(data = CORN.xts$etf_asset_error, spec = model_spec)

fit




#=========Residual Disgnostics========#
par(mfrow = c(2,2))
plot(CORN$Date, fit@fit[['residuals']], type= 'line') + ylab('Residuals') +
  xlab('Date') + theme_bw() 
hist(fit@fit[['residuals']]) + theme_bw() 
acf(fit@fit[['residuals']]) 
qqnorm(fit@fit[['residuals']]) 
qqline(fit@fit[['residuals']])


#=========Residuals and Conditional Variance======#
qplot(CORN$Date, fit@fit[['residuals']], geom = 'line') + ggtitle('apARCH(1,1) Model Residuals') + ylab('Residuals') +
  xlab('Date') + theme_bw()
qplot(CORN$Date, fit@fit[['residuals']]^2, geom = 'line') + ggtitle('apARCH(1,1) Model Residuals^2') + ylab('Squared Residuals') + 
  xlab('Date') + theme_bw()
qplot(CORN$Date, fit@fit[['sigma']], geom = 'line') + ggtitle('apARCH(1,1) Conditional Variance') + ylab('h') + xlab('Date') +
  theme_bw()





