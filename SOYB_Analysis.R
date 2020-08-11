rm(list=ls())
library(readxl)
library(tidyverse)
library(ggthemes)
library(xts)
library(tidyr)
library(zoo)
library(rugarch)

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
SOYB$DATE <- as.Date(SOYB$DATE, origin = "1899-12-30") 
SOYB <- SOYB[order(SOYB$DATE),]

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

#----------------------More Date Manipulation----------------#
#Remove rows with NA
SOYB <- na.omit(SOYB)

SOYB <- SOYB %>% 
  mutate(Date = as.Date(DATE)) %>% 
  complete(Date = seq.Date(min(DATE), max(DATE), by="day"))

# Now to forward fill the date
SOYB <- na.locf(na.locf(SOYB), fromLast = TRUE)

# Remove the additional date column
SOYB <- subset(SOYB, select = -Date)
SOYB$DATE <- as.Date(SOYB$DATE)
# Convert the object into a XTS object
SOYB.xts <- as.xts(SOYB, order.by = SOYB$DATE)

#========================================================#
# Plot ETF-Asset Errors
qplot(SOYB$DATE, SOYB$etf_asset_error, geom = 'line') + ggtitle('SOYB: ETF % Reutrn - Asset % Return') +
  ylab('Error') + xlab('Date') + theme_bw()
qplot(SOYB$DATE, SOYB$etf_asset_error^2, geom = 'line') + ggtitle('SOYB: (ETF % Return - Asset % Return)^2') +
  ylab('Squared Error') + xlab('Date') + theme_bw()

# Simple Beta Mdoel
beta_ols = lm(per_asset_return ~ per_ETF_return, data = SOYB)
summary(beta_ols)
lmtest::bptest(beta_ols)
qplot(SOYB$DATE, beta_ols$residuals, geom = 'line') + ggtitle('SOYB: Residuals from Beta Model') + 
  ylab('Residuals') + xlab('Date') + theme_bw()
qplot(SOYB$DATE, beta_ols$residuals^2, geom = 'line') + ggtitle('SOYB: Squared Residuals from Beta Model') + 
  ylab('Squared Residuals') + xlab('Date') + theme_bw()

#Simple OLS Model
simple_ols = lm(abs(etf_asset_error) ~ abs(per_asset_return), data = SOYB)
summary(simple_ols)
lmtest::bptest(simple_ols)
qplot(SOYB$DATE, simple_ols$residuals, geom = 'line') + ggtitle('SOYB: Residuals from Simple OLS Model') +
  ylab('Residuals') + xlab('Date') + theme_bw()
qplot(SOYB$DATE, simple_ols$residuals^2, geom = 'line') + ggtitle('SOYB: Squared Residuals from Simple OLS Model') +
  ylab('SquaredResiduals') + xlab('Date') + theme_bw()


# Dummy Model
model <- lm(abs(SOYB$etf_asset_error) ~ abs(SOYB$per_ETF_return) + SOYB$`S WASDE` + SOYB$`S WASDE + CP` +
              SOYB$`S Grain Stocks` + SOYB$`S Prospective Plantings` + SOYB$`S Acreage Report` + 
              SOYB$`S Cattle on Feed` + SOYB$`S Hogs & Pigs` + SOYB$`S Day Before Roll` + SOYB$`S Day After Roll` +
              SOYB$`C Feb` + SOYB$`C Mar` + SOYB$`C April` + SOYB$`C May` + SOYB$`C June` + SOYB$`C July` +
              SOYB$`C Aug` + SOYB$`C Sept` + SOYB$`C Nov` + SOYB$`C Dec` + SOYB$`C 2013` + SOYB$`C 2014` +
              SOYB$`C 2015` + SOYB$`C 2016` + SOYB$`C 2017` + SOYB$`C 2018` + SOYB$`C 2019` + SOYB$`C 2020`)
summary(model)
lmtest::bptest(model)
qplot(SOYB$DATE, model$residuals, geom = 'line') + ggtitle("SOYB: Residuals from Dummy Model") + 
  ylab("Residuals") + xlab('Date') + theme_bw()
qplot(SOYB$DATE, model$residuals^2, geom = 'line') + ggtitle('SOYB: Squared Residuals from Dummy Model') +
  ylab('Squared Residuals') + xlab('Date') + theme_bw()


#---------------------GARCH----------------------------------------------#

# Define the model
model_spec <- ugarchspec(variance.model = list(model = 'apARCH', garchOrder = c(1,1)))

# Fit the model and display results
fit <- ugarchfit(data = SOYB.xts$etf_asset_error, spec = model_spec)
fit


qplot(SOYB$DATE, fit@fit[['residuals']], geom = 'line') + ggtitle('SOYB: apARCH(1,1) Model Residuals') + ylab('Residuals') +
  xlab('Date') + theme_bw()
qplot(SOYB$DATE, fit@fit[['residuals']]^2, geom = 'line') + ggtitle('SOYB: apARCH(1,1) Model Residuals^2') + ylab('Squared Residuals') + 
  xlab('Date') + theme_bw()
qplot(SOYB$DATE, fit@fit[['sigma']], geom = 'line') + ggtitle('SOYB: apARCH(1,1) Conditional Variance') + ylab('h') + xlab('Date') +
  theme_bw()

