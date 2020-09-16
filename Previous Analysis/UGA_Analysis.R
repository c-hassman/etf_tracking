rm(list = ls())
library(readxl)
library(tidyverse)
library(xts)
library(tidyr)
library(zoo)
library(rugarch)
#-----------------Import Data from Excel and order------------#
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

#-------------------Date Manipulation and Cleaning----------------------#
UGA$DATE <- as.Date(UGA$DATE, origin = "1899-12-30") 
UGA <- UGA[order(UGA$DATE),]

#-----------------------Calculate Returns and Errors--------------#
UGA$per_asset_return <- log(UGA$Futures/lag(UGA$Futures)) * 100
UGA$per_ETF_return <- log(UGA$UGA_MID/lag(UGA$UGA_MID)) * 100
UGA$per_NAV_return <- log(UGA$UGA_NAV/lag(UGA$UGA_NAV)) * 100
UGA$etf_asset_error <- UGA$per_ETF_return - UGA$per_asset_return

#-----------------------Add ETF Volume Data-----------------------#

# Import volume data from csv
volume <- read.csv("G:/My Drive/3_Massa Research/Neff Paper/Working_Folder/Volume.csv")
# subset the dataframe to only the relevant columes
volume <- data.frame(as.Date(volume$DATE), volume$UGA.Volume)
#rename the columns
colnames(volume) <- c("DATE", "Volume")
#Merge the Volume data with the other data
UGA <- merge(UGA, volume, by = "DATE")

#----------------------More Date Manipulation----------------#
#Remove rows with NA
UGA <- na.omit(UGA)

UGA <- UGA %>% 
  mutate(Date = as.Date(DATE)) %>% 
  complete(Date = seq.Date(min(DATE), max(DATE), by="day"))

# Now to forward fill the date
UGA <- na.locf(na.locf(UGA), fromLast = TRUE)

# Remove the additional date column
UGA <- subset(UGA, select = -Date)

# Create an XTS object
UGA.xts <- as.xts(UGA, order.by = UGA$DATE)

#===================================================================#
# pl,ot the ETF_ASSET_Errors
qplot(UGA$DATE, UGA$etf_asset_error, geom ='line') + ggtitle('UGA: ETF% Return - Asset % Return') +
  ylab('Error') + xlab('Date') + theme_bw()
qplot(UGA$DATE, UGA$etf_asset_error^2, geom ='line') + ggtitle('UGA: (ETF% Return - Asset % Return)^2') +
  ylab('Squared Error') + xlab('Date') + theme_bw()

# Simple Beta model
beta_ols = lm(per_asset_return ~ per_ETF_return, data = UGA)
summary(beta_ols)
lmtest::bptest(beta_ols)
qplot(UGA$DATE, beta_ols$residuals, geom = 'line') + ggtitle('UGA: Residuals from Beta Model') +
  ylab('Residuals') + xlab('Date') + theme_bw()
qplot(UGA$DATE, beta_ols$residuals^2, geom = 'line') + ggtitle('UGA: Squared Residuals from Beta Model') +
  ylab('Squared Residuals') + xlab('Date') + theme_bw()

# simple OLS
simple_ols = lm(abs(etf_asset_error) ~ abs(per_asset_return), data = UGA)
summary(simple_ols)
lmtest::bptest(simple_ols)
qplot(UGA$DATE, simple_ols$residuals, geom = 'line') + ggtitle('UGA: Residuals from Simple OLS Model') +
  xlab('Date') + ylab('Residuals') + theme_bw()
qplot(UGA$DATE, simple_ols$residuals^2, geom = 'line') + ggtitle('UGA: Squared Residuals from Simple OLS Model') +
  xlab('Date') + ylab('Squared Residuals') + theme_bw()

# Dummy Model
model <- lm(abs(UGA$etf_asset_error) ~ abs(UGA$per_ETF_return) + UGA$`RB Day Before Roll` + UGA$`RB Day After Roll` +
              UGA$`RB Feb` + UGA$`RB Mar` + UGA$`RB April` + UGA$`RB May` + UGA$`RB June` + UGA$`RB July` +
              UGA$`RB Aug` + UGA$`RB Sept` + UGA$`RB Oct` + UGA$`RB Nov` + UGA$`RB Dec` + UGA$`RB 2013` +
              UGA$`RB 2014` + UGA$`RB 2015` + UGA$`RB 2016` + UGA$`RB 2017` + UGA$`RB 2018` + UGA$`RB 2019` +
              UGA$`RB 2020` + UGA$`RB STEO` + UGA$`RB Drilling Prod` + UGA$`RB Petro Supply/Prod` + UGA$`RB Annual Energy Outlook`)
summary(model)            
lmtest::bptest(model)
qplot(UGA$DATE, model$residuals, geom = 'line') + ggtitle('UGA: Residuals from Dummy Model') + 
  ylab('Residuals') + xlab('Date') + theme_bw()
qplot(UGA$DATE, model$residuals^2, geom = 'line') + ggtitle('UGA: Squared Residuals from Dummy Model') + 
  ylab('Squared Residuals') + xlab('Date') + theme_bw()

#------------GARCH---------#

# Define the model
model_spec <- ugarchspec(variance.model = list(model = 'apARCH', garchOrder = c(1,1)))

# Fit the model and display results
fit <- ugarchfit(data = UGA.xts$etf_asset_error, spec = model_spec)
fit


qplot(UGA$DATE, fit@fit[['residuals']], geom = 'line') + ggtitle('UGA: apARCH(1,1) Model Residuals') + ylab('Residuals') +
  xlab('Date') + theme_bw()
qplot(UGA$DATE, fit@fit[['residuals']]^2, geom = 'line') + ggtitle('UGA: apARCH(1,1) Model Residuals^2') + ylab('Squared Residuals') + 
  xlab('Date') + theme_bw()
qplot(UGA$DATE, fit@fit[['sigma']], geom = 'line') + ggtitle('apARCH(1,1) Conditional Variance') + ylab('h') + xlab('Date') +
  theme_bw()






