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


#-----------------------Add ETF Volume Data-----------------------#

# Import volume data from csv
volume <- read.csv("G:/My Drive/3_Massa Research/Neff Paper/Working_Folder/Volume.csv")
# subset the dataframe to only the relevant columes
volume <- data.frame(as.Date(volume$DATE), volume$USO.Volume)
#rename the columns
colnames(volume) <- c("DATE", "Volume")
#Merge the Volume data with the other data
USO <- merge(USO, volume, by = "DATE")
USO$volume_return <-log(USO$Volume/lag(USO$Volume)) * 100

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





#--- GARCH Model
base_model_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(2,1)))
base_fit <- ugarchfit(data = USO.xts$etf_asset_error, spec = base_model_spec)
base_fit




# Full model
ext_reg <- USO.xts # creates a new xts object to hold external regressors

# The code below removes all the columns which are not external regressors. 
# There must be a better way to do this
ext_reg$Date <- NULL
ext_reg$USO_MID <- NULL
ext_reg$Futures <- NULL
ext_reg$USO_NAV <- NULL
ext_reg$ROLL <- NULL
ext_reg$`CL Jan` <- NULL
ext_reg$`C 2012` <- NULL
ext_reg$`C 2020` <- NULL
ext_reg$etf_asset_error<- NULL
ext_reg$per_NAV_return <- NULL
ext_reg$per_ETF_return <- NULL
ext_reg$Volume <- NULL
ext_reg$DATE <- NULL


full_model_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1),
                                                    external.regressors = ext_reg),
                              mean.model = list(armaOrder = c(2,1)))

setbounds(full_model_spec) <- list(vxreg1 = c(-100,100), vxreg2 = c(-100,100), vxreg3 = c(-100,100), vxreg4 = c(-100,100),
                                   vxreg5 = c(-100,100), vxreg6 = c(-100,100), vxreg7 = c(-100,100), vxreg8 = c(-100,100),
                                   vxreg9 = c(-100,100), vxreg10 = c(-100,100), vxreg11 = c(-100,100), vxreg12 = c(-100,100), 
                                   vxreg13 = c(-100,100), vxreg14 = c(-100,100), vxreg15 = c(-100,100), vxreg16 = c(-100,100),
                                   vxreg17 = c(-100,100), vxreg18 = c(-100,100), vxreg19 = c(-100,100), vxreg20 = c(-100,100), 
                                   vxreg21 = c(-100,100), vxreg22 = c(-100,100), vxreg23 = c(-100,100), vxreg24 = c(-100,100), 
                                   vxreg25 = c(-100,100), vxreg26 = c(-100,100), vxreg28 = c(-100,100), vxreg29 = c(-100,100),
                                   vxreg30 = c(-100,100))


# Fit the model and display results
full_fit <- ugarchfit(data = USO.xts$etf_asset_error, spec = full_model_spec)
full_fit




