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
# calculate percent change in volume 
SOYB$volume_return <- log(SOYB$Volume)

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
#SOYB$DATE <- as.Date(SOYB$DATE)
# Convert the object into a XTS object
SOYB.xts <- as.xts(SOYB, order.by = SOYB$DATE)

