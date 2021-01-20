################################################################################
##### SOYB Roll Issue Scratchpad ###############################################
###############################################################################

# Colburn Hassman
# January 20,2021

# Purpose: This script investigates different ways of dealing with the issue of 
# early managerial tracking errors in SOYB on day after rolls. This is likely 
# a data issue, versus a managerial one, so care must be taken to fix it. 


source("~/Documents/etf_tracking/Analysis/preprocessing.R")
SOYB_ex <- data_pull_ex("SOYB")

# Plot of TDa overtime
plot(SOYB_ex$DATE, SOYB_ex$TDm, type = "l")

# Original MZ
base_model <- lm(SOYB_ex$per_NAV_return ~ SOYB_ex$per_asset_return)
summary(base_model)

# Solution 1: Single dummy

