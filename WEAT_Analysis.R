rm(list = ls())
library(readxl)
library(tidyverse)

#-----------------Import Data from Excel and order------------#
WEAT <- read_excel("G:/My Drive/3_Massa Research/Neff Paper/Working_Folder/Data_Update.xlsx", 
                   sheet = "WEAT", col_types = c("date", 
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
WEAT <- WEAT[order(WEAT$DATE),] #order by date
WEAT$asset_basket <- (WEAT$`F1(.35)` * 0.35) + (WEAT$`F2(.3)` * 0.3) + (WEAT$`F3(.35)` * 0.35) #construct asset baskets

#-----------------------Calculate Returns and Errors--------------#
WEAT$per_asset_return <- log(WEAT$asset_basket/lag(WEAT$asset_basket)) #calculate percent asset return 
WEAT$per_ETF_return <- log(WEAT$WEAT_MID/lag(WEAT$WEAT_MID)) #calculate percent ETF return
WEAT$per_NAV_return <- log(WEAT$WEAT_NAV/lag(WEAT$WEAT_NAV)) #calculate percent NAV return
WEAT$etf_asset_error <- WEAT$per_ETF_return - WEAT$per_asset_return #calculate percent ETF return
WEAT <- na.omit(WEAT) #Omit Rows with NAs

#---------------------Exploratory Plots--------------------------#
#------ETF, Asset Basket Error
qplot(WEAT$DATE, (WEAT$etf_asset_error * 100), geom = 'line') + theme_bw() +
  ggtitle("Daily Return Error: WEAT ETF - Asset Basket") + ylab("Error (%)") +
  xlab("Date")
#-----ETF and Asset Basket 
etfcolor <- "black"
assetcolor <- "darkgrey"
coeff <- WEAT$asset_basket[1] / WEAT$WEAT_MID[1]
ggplot(data = WEAT, aes(x = DATE)) +
  geom_line(aes(y = WEAT_MID), color = etfcolor) +
  geom_line(aes(y = asset_basket / coeff), color = assetcolor) +
  scale_y_continuous(
    name = 'ETF Price ($)', 
    sec.axis = sec_axis(~.*coeff, name = "Asset Basket Price (cents per bushel)")
  ) + theme_bw() + ggtitle('WEAT ETF and Asset Basket Price')
#----Premium/Discount to NAV
qplot(WEAT$DATE, ((WEAT$WEAT_MID - WEAT$WEAT_NAV)/WEAT$WEAT_NAV * 100), geom = 'line') +
  theme_bw() + ylab("Premium/Discount (%)") + xlab("Date") + ggtitle("WEAT Premium/Discount to NAV")

#-------------------OLS-----------------------------------------------#
#--------Simple
simple <- lm(per_asset_return ~ per_ETF_return, data = WEAT)
summary(simple)

#--------Dummy
model <- lm(abs(WEAT$etf_asset_error) ~ abs(WEAT$per_ETF_return) + WEAT$`W WASDE` + WEAT$`W WASDE + CP` +
              WEAT$`W Grain Stocks` + WEAT$`W Prospective Plantings` + WEAT$`W Acreage Report` +
              WEAT$`W Cattle on Feed` + WEAT$`W Hogs & Pigs` + WEAT$`W Day Before Roll` + WEAT$`W Day After Roll` +
              WEAT$`W Feb` + WEAT$`W Mar` + WEAT$`W April` + WEAT$`W May` + WEAT$`W June` + 
              WEAT$`W July` + WEAT$`W Aug` + WEAT$`W Sept` + WEAT$`W Oct` + WEAT$`W Nov` + WEAT$`W Dec` +
              WEAT$`W 2013` + WEAT$`W 2014` + WEAT$`W 2015` + WEAT$`W 2016` + WEAT$`W 2017` +
              WEAT$`W 2018` + WEAT$W2019 + WEAT$W2020)
summary(model)

#---------------------GARCH----------------------------------------------#
err_garch = tseries::garch(x = WEAT$etf_asset_error, order = c(1,1))
summary(err_garch)

#--GARCH Volatility Graph
# This graphs the Volatility from the GARCH model versus the market returns
vol = err_garch$fitted.values # assign the fitted values to a variable
vol = data.frame(vol) # convert to a dataframe
vol$Volatility = vol$sigt # Create a new column of sigt squared
vol$Date = WEAT$DATE # Assign the date column from corn to vol
vol$'Asset Return ^2' = WEAT$per_asset_return ^2 # add the per asset returns
# Convert the data to a long format
vol_long <- vol %>%
  select(Date, Volatility, 'Asset Return ^2') %>%
  gather(key = 'variable', value = 'value', -Date)

# Make Graph
ggplot(vol_long, aes(x = Date, y = value)) + 
  geom_line(aes(color = variable)) + 
  scale_color_manual(values = c("darkred", "steelblue")) +
  facet_grid(rows = vars(variable), scales = "free") +
  theme_bw() + theme(legend.position = "none") +
  ylab("Percent (%)") + ggtitle("WEAT Asset Basket Return and Volatility Plot")


#_--------------------ACF and PACF Plots----------------------------------#
WEAT_Error <- WEAT$etf_asset_error
acf(WEAT_Error)
pacf(WEAT_Error)
