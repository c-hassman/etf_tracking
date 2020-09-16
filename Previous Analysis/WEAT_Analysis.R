rm(list = ls())
library(readxl)
library(tidyverse)
library(xts)
library(tidyr)
library(zoo)
library(rugarch)
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
#-------------------Date Manipulation and Cleaning----------------------#
WEAT$DATE <- as.Date(WEAT$DATE, origin = "1899-12-30") 
WEAT <- WEAT[order(WEAT$DATE),] #order by date

#-----------------------Calculate Returns and Errors--------------#
WEAT$asset_basket <- (WEAT$`F1(.35)` * 0.35) + (WEAT$`F2(.3)` * 0.3) + (WEAT$`F3(.35)` * 0.35) #construct asset baskets
WEAT$per_asset_return <- log(WEAT$asset_basket/lag(WEAT$asset_basket)) * 100 #calculate percent asset return 
WEAT$per_ETF_return <- log(WEAT$WEAT_MID/lag(WEAT$WEAT_MID)) * 100#calculate percent ETF return
WEAT$per_NAV_return <- log(WEAT$WEAT_NAV/lag(WEAT$WEAT_NAV)) * 100 #calculate percent NAV return
WEAT$etf_asset_error <- WEAT$per_ETF_return - WEAT$per_asset_return #calculate percent ETF return

#-----------------------Add ETF Volume Data-----------------------#
# Import volume data from csv
volume <- read.csv("G:/My Drive/3_Massa Research/Neff Paper/Working_Folder/Volume.csv")
# subset the dataframe to only the relevant columes
volume <- data.frame(as.Date(volume$DATE), volume$WEAT.Volume)
#rename the columns
colnames(volume) <- c("DATE", "Volume")
#Merge the Volume data with the other data
WEAT <- merge(WEAT, volume, by = "DATE")

#----------------------More Date Manipulation----------------#
#Remove rows with NA
WEAT <- na.omit(WEAT) #Omit Rows with NAs

WEAT <- WEAT %>% 
  mutate(Date = as.Date(DATE)) %>% 
  complete(Date = seq.Date(min(DATE), max(DATE), by="day"))

# Now to forward fill the date
WEAT <- na.locf(na.locf(WEAT), fromLast = TRUE)

# Remove the additional date column
WEAT <- subset(WEAT, select = -Date)

# Create an XTS object
WEAT.xts <- as.xts(WEAT, order.by = WEAT$DATE)

#===============================================#
# Plot ETF_ASSET_ERRORS
qplot(WEAT$DATE, WEAT$etf_asset_error, geom = 'line') + ggtitle('WEAT: ETF % Return - Asset % Return') + 
  ylab('Error') + xlab('Date') + theme_bw()
qplot(WEAT$DATE, WEAT$etf_asset_error^2, geom = 'line') + ggtitle('WEAT: (ETF % Return - Asset % Return)^2') + 
  ylab('Squared Error') + xlab('Date') + theme_bw()

# Simple Beta Model
beta_ols = lm(per_asset_return ~ per_ETF_return, data = WEAT)
summary(beta_ols)
lmtest::bptest(beta_ols)
qplot(WEAT$DATE, beta_ols$residuals, geom= 'line') + ggtitle('WEAT: Residuals from Beta Model') +
  ylab('Residuals') + xlab('Date') + theme_bw()
qplot(WEAT$DATE, beta_ols$residuals^2, geom= 'line') + ggtitle('WEAT: Squared Residuals from Beta Model') +
  ylab('Residuals') + xlab('Date') + theme_bw()

# Simple OLS Model
simple_ols = lm(abs(etf_asset_error) ~ abs(per_asset_return), data = WEAT)
summary(simple_ols)
lmtest::bptest(simple_ols)
qplot(WEAT$DATE, simple_ols$residuals, geom = 'line') + ggtitle('WEAT: Residuals from Simple OLS Model') +
  ylab('Residuals') + xlab('Date') + theme_bw()
qplot(WEAT$DATE, simple_ols$residuals^2, geom = 'line') + ggtitle('WEAT: Squared Residuals from Simple OLS Model') +
  ylab('Squared Residuals') + xlab('Date') + theme_bw()

# Dummy Model
model <- lm(abs(WEAT$etf_asset_error) ~ abs(WEAT$per_ETF_return) + WEAT$`W WASDE` + WEAT$`W WASDE + CP` +
              WEAT$`W Grain Stocks` + WEAT$`W Prospective Plantings` + WEAT$`W Acreage Report` +
              WEAT$`W Cattle on Feed` + WEAT$`W Hogs & Pigs` + WEAT$`W Day Before Roll` + WEAT$`W Day After Roll` +
              WEAT$`W Feb` + WEAT$`W Mar` + WEAT$`W April` + WEAT$`W May` + WEAT$`W June` + 
              WEAT$`W July` + WEAT$`W Aug` + WEAT$`W Sept` + WEAT$`W Oct` + WEAT$`W Nov` + WEAT$`W Dec` +
              WEAT$`W 2013` + WEAT$`W 2014` + WEAT$`W 2015` + WEAT$`W 2016` + WEAT$`W 2017` +
              WEAT$`W 2018` + WEAT$W2019 + WEAT$W2020)
summary(model)
lmtest::bptest(model)
qplot(WEAT$DATE, model$residuals, geom= 'line') + ggtitle('WEAT: Residuals from Dummy Model') + 
  ylab('Residuals') + xlab('Date') + theme_bw()
qplot(WEAT$DATE, model$residuals^2, geom= 'line') + ggtitle('WEAT: Squared Residuals from Dummy Model') + 
  ylab('Squared Residuals') + xlab('Date') + theme_bw()

#---------------GARCH----------------#
# Define the model
model_spec <- ugarchspec(variance.model = list(model = 'apARCH', garchOrder = c(1,1)))

# Fit the model and display results
fit <- ugarchfit(data = WEAT.xts$etf_asset_error, spec = model_spec)
fit


qplot(WEAT$DATE, fit@fit[['residuals']], geom = 'line') + ggtitle('WEAT: apARCH(1,1) Model Residuals') + ylab('Residuals') +
  xlab('Date') + theme_bw()
qplot(WEAT$DATE, fit@fit[['residuals']]^2, geom = 'line') + ggtitle('WEAT: apARCH(1,1) Model Residuals^2') + ylab('Squared Residuals') + 
  xlab('Date') + theme_bw()
qplot(WEAT$DATE, fit@fit[['sigma']], geom = 'line') + ggtitle('WEAT: apARCH(1,1) Conditional Variance') + ylab('h') + xlab('Date') +
  theme_bw()




