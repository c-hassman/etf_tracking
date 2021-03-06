################################################################################
##### SOYB Roll Issue Scratchpad ###############################################
###############################################################################

# Colburn Hassman
# January 20,2021

# Purpose: This script investigates different ways of dealing with the issue of 
# early managerial tracking errors in SOYB on day after rolls. This is likely 
# a data issue, versus a managerial one, so care must be taken to fix it. 

library(dplyr)

# Import SOYB Data
source("~/Documents/etf_tracking/Analysis/preprocessing.R")
SOYB_ex <- data_pull_ex("SOYB")
SOYB_ex <- na.omit(SOYB_ex)

# Import Dummy Data
setwd("~/Documents/etf_tracking/Analysis/Aux/SOYB_Roll_Issue")
dum <- read.csv("soyb_dummies.csv")
names(dum)[names(dum) == "X"] <- "DATE" # Rename col
dum$DATE = as.Date(dum$DATE) # set col as date 


#Merge the dataframes
SOYB <- left_join(SOYB_ex, dum, by = "DATE")

# Plot of TDa overtime
plot(SOYB$DATE, SOYB$TDm, type = "l")

# Original MZ
base_model <- lm(SOYB$per_NAV_return ~ SOYB$per_asset_return)
summary(base_model)

# Solution 1: Single dummy

single_model <- lm(SOYB$per_NAV_return ~ SOYB$per_asset_return + SOYB$Single)
summary(single_model)
plot(SOYB$DATE, single_model$residuals, type = "l")

unique_model <- lm(SOYB$per_NAV_return ~ SOYB$per_asset_return + SOYB$Single +
                     SOYB$u0 + SOYB$u1 + SOYB$u2 + SOYB$u3 + SOYB$u4 + SOYB$u5 + 
                     SOYB$u6 + SOYB$u7 + SOYB$u8 + SOYB$u9 + SOYB$u10 + SOYB$u11 + 
                     SOYB$u12 + SOYB$u13)
summary(unique_model)
plot(SOYB$DATE, unique_model$residuals, type = "l")
