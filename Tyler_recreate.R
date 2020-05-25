rm(list=ls())
library(readxl)
library(tidyverse)
library(ggthemes)


#----------------Load Data, order, and subset--------------#
# Import data from excel sheet
data <- read_excel("Data_Update.xlsx", 
                   col_types = c("date", "numeric", "numeric", 
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
                                 "numeric"))

data <- data[order(data$DATE),] #order by date
data <- subset(data, DATE < as.Date("2017-10-31")) #subset the data to only include those tyler used
data$asset_basket <- (data$`F1(.35)` * 0.35) + (data$`F2(.3)` * 0.3) + (data$`F3(.35)` * 0.35) # reconstruct asset basket

#--------------Calculate returns and Errors-----------------#

data$per_nav_return <- log(data$CORN_NAV/lag(data$CORN_NAV)) #calculate percent daily return in nav
data$per_etf_return <- log(data$CORN_MID/lag(data$CORN_MID)) #calculate percent daily return in etf
data$per_asset_return <- log(data$asset_basket/lag(data$asset_basket)) #calculate daily return in asset basket
data$etf_asset_error <- data$per_etf_return - data$per_asset_return #calculate error (%) between etf and asset
data <- na.omit(data) # remove all rows with NA, which are the last day of roll rows

#-------------Exploratory Plots----------------------------#

qplot(data$DATE, (data$etf_asset_error * 100), geom='line') + theme_bw() + 
  ggtitle('Daily Return Error: CORN ETF - Asset Basket') +
  ylab('Error (%)') + xlab('') + ylim(-7,5)

TE1 <- sum(abs(data$etf_asset_error)) / nrow(data)
