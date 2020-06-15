rm(list = ls())
library(readxl)
library(tidyverse)
library(ggthemes)
library(xts)


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

CORN$DATE <- as.Date(CORN$DATE, origin = "1899-12-30") 
# Note: If I import the Date column as a "Date" using readxl, it includes a timezone character
# which creates issue when I merge this df and the volume df. also need to store the data in 
# excel as a 
CORN <- CORN[order(CORN$DATE),] # order by date
CORN$asset_basket <- (CORN$`F1(.35)` * 0.35) + (CORN$`F2(.3)` * 0.3) + (CORN$`F3(.35)` * 0.35) #reconstruct asset basket


#-----------------------Calculate Returns and Errors--------------#
CORN$per_asset_return <- log(CORN$asset_basket/lag(CORN$asset_basket)) # calculate percent asset basket return
CORN$per_ETF_return <- log(CORN$CORN_MID/lag(CORN$CORN_MID)) #calculate percent ETF return
CORN$per_NAV_return <- log(CORN$CORN_NAV/lag(CORN$CORN_NAV)) #calculate percent NAV return
CORN$etf_asset_error <- CORN$per_ETF_return - CORN$per_asset_return #calculate error between ETF and Asset

#-----------------------Add ETF Volume Data-----------------------#

CORN_df <- CORN  #reassign the data so it is not deleted
start_date <- "2012-01-04"
end_date <- '2020-01-30'
symbols <- "CORN"

#Pull Data from Yahoo Finance
quantmod::getSymbols(Symbols = symbols, 
           src = "yahoo", 
           index.class = "POSIXct",
           from = start_date, 
           to = end_date, 
           adjust = FALSE)
CORN <- data.frame(DATE = as.Date(index(CORN)), CORN$CORN.Volume)
CORN <- merge(CORN_df, CORN, by = "DATE")

rm(CORN_df)

#this is such sloppy code. in final version I will really clean it up
# use tmp files if needed



CORN <- na.omit(CORN) #Omit the rows with NAs, which are the final roll days

#-----------------------Exploratory Plots-----------------------#
#------ETF, Asset Basket Error
qplot(CORN$DATE, (CORN$etf_asset_error * 100), geom = 'line') + theme_bw() +
  ggtitle('Daily Return Error: CORN ETF - Asset Basket') +
  ylab('Error (%)') + xlab("Date")
#-----ETF and Asset Basket 
etfcolor <- "black"
assetcolor <- "darkgrey"
coeff <- CORN$asset_basket[1] / CORN$CORN_MID[1]
ggplot(data = CORN, aes(x = DATE)) +
  geom_line(aes(y = CORN_MID), color = etfcolor) +
  geom_line(aes(y = asset_basket / coeff), color = assetcolor) +
  scale_y_continuous(
    name = "ETF Price ($)", 
    sec.axis = sec_axis(~.*coeff, name = "Asset Basket Price (cents per bushel")
  ) + theme_bw() + ggtitle("CORN ETF and Asset Basket Price") + xlab("Date") 
#----Premium/Discount to NAV
qplot(CORN$DATE, ((CORN$CORN_MID - CORN$CORN_NAV)/CORN$CORN_NAV * 100), geom = 'line') +
  theme_bw() + ylab("Premium/Discount (%)") + xlab("Date") + ggtitle("CORN Premium/Discount to NAV")
#-------------------OLS-----------------------------------------------#
#--------Simple
simple <- lm(per_asset_return ~ per_ETF_return, data = CORN)
summary(simple)

#--------Dummy
model <- lm(abs(CORN$etf_asset_error) ~ abs(CORN$per_ETF_return) + CORN$`C WASDE` + CORN$`C WASDE + CP` +
              CORN$`C Grain Stocks` + CORN$`C Prospective Plantings` + CORN$`C Acreage Report` + 
              CORN$`C Cattle on Feed` + CORN$`C Hogs & Pigs` + CORN$`C Day Before Roll` + CORN$`C Day After Roll`+
              CORN$`C Feb` + CORN$`C Mar` + CORN$`C April` + CORN$`C May` + CORN$`C June` + CORN$`C July` +
              CORN$`C Aug` + CORN$`C Sept` + CORN$`C Oct` + CORN$`C Nov` + CORN$`C Dec` + CORN$`C 2013` +
              CORN$`C 2014` + CORN$`C 2015` + CORN$`C 2016` + CORN$`C 2017` + CORN$`C 2018` + CORN$`C 2019` +
              CORN$`C 2020`)
summary(model)
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
