rm(list = ls())
library(readxl)
library(tidyverse)
library(ggthemes)
library(xts)
library(tidyr)
library(zoo)
library(rugarch)
library(skedastic)




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

# Remove after roll days 
CORN<- CORN[! (CORN$`C Day After Roll` == '1'),]






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

# Convert the object into a XTS object
CORN.xts <- as.xts(CORN, order.by = CORN$Date)

#---------------------Descriptive Statistics of Asset Error------#

mean(CORN$etf_asset_error, na.rm = TRUE)
sd(CORN$etf_asset_error, na.rm = TRUE)
e1071::skewness(CORN$etf_asset_error, na.rm = TRUE)
e1071::kurtosis(CORN$etf_asset_error, na.rm = TRUE)
max(CORN$etf_asset_error, na.rm = TRUE)
min(CORN$etf_asset_error, na.rm = TRUE)
nrow(CORN$etf_asset_error, na.rm = TRUE)
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



# Simple beta model
beta_ols = lm(per_asset_return ~ per_ETF_return, data = CORN)
summary(beta_ols)
lmtest::bptest(beta_ols)
qplot(CORN$Date, beta_ols$residuals, geom = 'line') + ggtitle("CORN: Residuals from Beta Model") + 
  ylab("Residuals") + xlab('Date') + theme_bw()
qplot(CORN$Date, beta_ols$residuals^2, geom = 'line') + ggtitle('CORN: Squared Residuals from Beta Model') +
  ylab('Squared Residuals') + xlab('Date') + theme_bw()



# Simple OlS Model
simple_ols = lm(abs(etf_asset_error) ~ abs(per_asset_return), data = CORN)
summary(simple_ols)
lmtest::bptest(simple_ols)
qplot(CORN$Date, simple_ols$residuals, geom = 'line') + ggtitle("CORN: Residuals from Simple OLS Model") + 
  ylab("Residuals") + xlab('Date') + theme_bw()
qplot(CORN$Date, simple_ols$residuals^2, geom = 'line') + ggtitle('CORN: Squared Residuals from Simple OLS Model') +
  ylab('Squared Residuals') + xlab('Date') + theme_bw()



# Dummy Model
model <- lm(abs(CORN$etf_asset_error) ~ abs(CORN$per_asset_return) + CORN$`C WASDE` + CORN$`C WASDE + CP` +
              CORN$`C Grain Stocks` + CORN$`C Prospective Plantings` + CORN$`C Acreage Report` + 
              CORN$`C Cattle on Feed` + CORN$`C Hogs & Pigs` + CORN$`C Day Before Roll` + CORN$`C Day After Roll`+
              CORN$`C Feb` + CORN$`C Mar` + CORN$`C April` + CORN$`C May` + CORN$`C June` + CORN$`C July` +
              CORN$`C Aug` + CORN$`C Sept` + CORN$`C Oct` + CORN$`C Nov` + CORN$`C Dec` + CORN$`C 2013` +
              CORN$`C 2014` + CORN$`C 2015` + CORN$`C 2016` + CORN$`C 2017` + CORN$`C 2018` + CORN$`C 2019` +
              CORN$`C 2020`)
summary(model)
lmtest::bptest(model)
qplot(CORN$Date, model$residuals, geom = 'line') + ggtitle("CORN: Residuals from Dummy Model") + 
  ylab("Residuals") + xlab('Date') + theme_bw()
qplot(CORN$Date, model$residuals^2, geom = 'line') + ggtitle('CORN: Squared Residuals from Dummy Model') +
  ylab('Squared Residuals') + xlab('Date') + theme_bw()



#--------------GARCH--------------------#

ext_reg <- CORN.xts # creates a new xts object to hold external regressors

# The code below removes all the columns which are not external regressors. 
# There must be a better way to do this
ext_reg$Date <- NULL
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

ext_reg$per_asset_return <- abs(as.numeric(ext_reg$per_asset_return))

# Define the model
model_spec <- ugarchspec(variance.model = list(model = 'apARCH', garchOrder = c(1,1)))

# Fit the model and display results
fit <- ugarchfit(data = CORN.xts$etf_asset_error, spec = model_spec)
fit


qplot(CORN$Date, fit@fit[['residuals']], geom = 'line') + ggtitle('apARCH(1,1) Model Residuals') + ylab('Residuals') +
  xlab('Date') + theme_bw()
qplot(CORN$Date, fit@fit[['residuals']]^2, geom = 'line') + ggtitle('apARCH(1,1) Model Residuals^2') + ylab('Squared Residuals') + 
  xlab('Date') + theme_bw()
qplot(CORN$Date, fit@fit[['sigma']], geom = 'line') + ggtitle('apARCH(1,1) Conditional Variance') + ylab('h') + xlab('Date') +
  theme_bw()





