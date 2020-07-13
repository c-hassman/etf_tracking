rm(list = ls())
library(readxl)
library(tidyverse)
library(ggthemes)
library(xts)
library(tidyr)
library(zoo)


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
CORN <- subset(CORN, select = -Date)

#---------------------GARCH----------------------------------------------#
err_garch = tseries::garch(x = CORN$etf_asset_error, order = c(1,1))
summary(err_garch)

#--GARCH Volatility Graph
# This graphs the Volatility from the GARCH model versus the market returns
vol = err_garch$fitted.values # assign the fitted values to a variable
vol = data.frame(vol) # convert to a dataframe
vol$Volatility = vol$sigt # Create a new column of sigt squared
vol$Date = CORN$DATE # Assign the date column from corn to vol
vol$Error = CORN$etf_asset_error^2 # add the per asset returns
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
  ylab("Percent (%)") + ggtitle("CORN Spread^2 and Volatility Plot")

#_--------------------ACF and PACF Plots----------------------------------#
CORN_Error <- CORN$etf_asset_error
acf(CORN_Error)
pacf(CORN_Error)
