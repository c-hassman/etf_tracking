rm(list=ls())
library(readxl)
library(tidyverse)
library(ggthemes)
library(xts)
library(tidyr)
library(zoo)

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

#---------------------GARCH----------------------------------------------#
err_garch = tseries::garch(x = USO$etf_asset_error, order = c(1,1))
summary(err_garch)

#--GARCH Volatility Graph
# This graphs the Volatility from the GARCH model versus the market returns
vol = err_garch$fitted.values # assign the fitted values to a variable
vol = data.frame(vol) # convert to a dataframe
vol$Volatility = vol$sigt # Create a new column of sigt squared
vol$Date = USO$DATE # Assign the date column from corn to vol
vol$Error = USO$etf_asset_error^2 # add the per asset returns
# Convert the data to a long format
vol_long <- vol %>%
  select(Date, Volatility, Error) %>%
  gather(key = 'variable', value = 'value', -Date)

# Make Graph
ggplot(vol_long, aes(x = Date, y = value)) + 
  geom_line(aes(color = variable)) + 
  scale_color_manual(values = c("darkred", "steelblue")) +
  facet_grid(rows = vars(variable), scales = "free") +
  theme_bw() + theme(legend.position = "none") +
  ylab("Percent (%)") + ggtitle("USO Spread^2 Volatility Plot")

#_--------------------ACF and PACF Plots----------------------------------#
USO_Error <- USO$etf_asset_error
acf(USO_Error)
pacf(USO_Error)
