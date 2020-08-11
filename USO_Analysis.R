rm(list=ls())
library(readxl)
library(tidyverse)
library(ggthemes)
library(xts)
library(tidyr)
library(zoo)
library(rugarch)
#------------------------Load in Data from Excel------------------------------#
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
#-------------------Date Manipulation and Cleaning----------------------#
USO$DATE <- as.Date(USO$DATE, origin = "1899-12-30") 
USO <- USO[order(USO$DATE),]

#--------------------Calculate Returns and Errors ---------------------------#
USO$per_asset_return <- log(USO$Futures/lag(USO$Futures)) * 100
USO$per_ETF_return <- log(USO$USO_MID /lag(USO$USO_MID)) * 100
USO$per_NAV_return <- log(USO$USO_NAV/lag(USO$USO_NAV)) * 100
USO$etf_asset_error <- USO$per_ETF_return - USO$per_asset_return
USO

#-----------------------Add ETF Volume Data-----------------------#

# Import volume data from csv
volume <- read.csv("G:/My Drive/3_Massa Research/Neff Paper/Working_Folder/Volume.csv")
# subset the dataframe to only the relevant columes
volume <- data.frame(as.Date(volume$DATE), volume$USO.Volume)
#rename the columns
colnames(volume) <- c("DATE", "Volume")
#Merge the Volume data with the other data
USO <- merge(USO, volume, by = "DATE")

#----------------------More Date Manipulation----------------#
#Remove rows with NA
USO <- na.omit(USO)

USO <- USO %>% 
  mutate(Date = as.Date(DATE)) %>% 
  complete(Date = seq.Date(min(DATE), max(DATE), by="day"))

# Now to forward fill the date
USO <- na.locf(na.locf(USO), fromLast = TRUE)

# Remove the additional date column
USO <- subset(USO, select = -Date)

# Create an XTS object
USO.xts <- as.xts(USO, order.by = USO$DATE)

#=====================================================#
# plot the ETF_ASSET_ERRORS
qplot(USO$DATE, USO$etf_asset_error, geom = 'line') + ggtitle('USO: ETF % Return = Asset % Return') + 
  ylab('Error') + xlab('Date') + theme_bw()
qplot(USO$DATE, USO$etf_asset_error^2, geom = 'line') + ggtitle('USO: (ETF % Return = Asset % Return)^2') + 
  ylab('Error') + xlab('Date') + theme_bw()

# Simple Beta Model
beta_ols = lm(per_asset_return ~ per_ETF_return, data = USO)
summary(beta_ols)
lmtest::bptest(beta_ols)
qplot(USO$DATE, beta_ols$residuals, geom = 'line') + ggtitle('USO: Residuals from Beta Model') +
  ylab('Residuals') + xlab('Date') + theme_bw()
qplot(USO$DATE, beta_ols$residuals^2, geom = 'line') + ggtitle('USO: Squared Residuals from Beta Model') +
  ylab('Squared Residuals') + xlab('Date') + theme_bw()

# Simple OLS Model
simple_ols = lm(abs(etf_asset_error) ~ abs(per_asset_return), data = USO)
summary(simple_ols)
lmtest::bptest(simple_ols)
qplot(USO$DATE, simple_ols$residuals, geom = 'line') + ggtitle('USO: Residuals from Simple OLS Model') + 
  ylab('Residuals') + xlab('Date') + theme_bw()
qplot(USO$DATE, simple_ols$residuals^2, geom = 'line') + ggtitle('USO: Squared Residuals from Simple OLS Model') + 
  ylab('Squared Residuals') + xlab('Date') + theme_bw()

# Dummy Model
model <- lm(abs(USO$etf_asset_error) ~ abs(USO$per_ETF_return) + USO$`CL Day Before Roll` +
              USO$`CL Day After Roll` + USO$`CL Feb` + USO$`CL Mar` + USO$`CL April` + USO$`CL May` +
              USO$`CL June` + USO$`CL July` + USO$`CL Aug` + USO$`CL Sept` + USO$`CL Oct` +
              USO$`CL Nov` + USO$`CL Dec` + USO$`CL 2014` + USO$`CL 2015` + USO$`CL 2016` +
              USO$`CL 2017` + USO$`CL 2018` + USO$`CL 2019` + USO$`CL 2020` + USO$`CL STEO` +
              USO$`CL Drilling Prod` + USO$`CL Petro Supply/Prod` + USO$`CL Annual Energy Outlook`)
summary(model)  
lmtest::bptest(model)
qplot(USO$DATE, model$residuals, geom = 'line') + ggtitle('USO: Residuals from Dummy Model') +
  ylab('Residuals') + xlab('Date') + theme_bw()
qplot(USO$DATE, model$residuals^2, geom = 'line') + ggtitle('USO: Squared Residuals from Dummy Model') +
  ylab('Squared Residuals') + xlab('Date') + theme_bw()

#----------GARCH----------#
# Define the model
model_spec <- ugarchspec(variance.model = list(model = 'apARCH', garchOrder = c(1,1)))

# Fit the model and display results
fit <- ugarchfit(data = USO.xts$etf_asset_error, spec = model_spec)
fit


qplot(USO$DATE, fit@fit[['residuals']], geom = 'line') + ggtitle('USO: apARCH(1,1) Model Residuals') + ylab('Residuals') +
  xlab('Date') + theme_bw()
qplot(USO$DATE, fit@fit[['residuals']]^2, geom = 'line') + ggtitle('USO: apARCH(1,1) Model Residuals^2') + ylab('Squared Residuals') + 
  xlab('Date') + theme_bw()
qplot(USO$DATE, fit@fit[['sigma']], geom = 'line') + ggtitle('USO: apARCH(1,1) Conditional Variance') + ylab('h') + xlab('Date') +
  theme_bw()




