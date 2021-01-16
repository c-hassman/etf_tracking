
###############################################################################
#### Summary stats ############################################################
###############################################################################
# Colburn Hassman
# colburn7@vt.edu
# January 15, 2021

library("stats")


source("~/Documents/etf_tracking/Analysis/preprocessing.R")
CORN_ex <- data_pull_ex("CORN")
SOYB_ex <- data_pull_ex("SOYB")
WEAT_ex <- data_pull_ex("WEAT")
USO_ex <- data_pull_ex("USO")
UGA_ex <- data_pull_ex("UGA")

CORN_in <- data_pull_in("CORN")
SOYB_in <- data_pull_in("SOYB")
WEAT_in <- data_pull_in("WEAT")
USO_in <- data_pull_in("USO")
UGA_in <- data_pull_in("UGA")



#############################
## Define Functions
#############################

### Do Calculations:
calc <- function(data, var){
  # Checks to see if you are looking at return based data: if so, omit na row
  if(var == "per_asset_return" || var == "per_NAV_return" || var == "per_ETF_return" || var == "TD" || var == "TDa" || var == "TDm"){
    data <- na.omit(data)
  }
  # do calculations
  ETF = c(min(data), median(data), mean(data), max(data), sd(data), length(data)) 
  return(ETF)
}



### Table make 
# Tried to make this a bit more functionalized but hit some road blocks... works
# even if it isnt pretty
table_make_ex <- function(var){
  values <- c("Min", "Median", "Mean", "Max", "STD DEV", "N")
  data_table <- data.frame(values, calc(CORN_ex[, var], var), calc(SOYB_ex[,var], var), 
                           calc(WEAT_ex[,var], var), calc(USO_ex[,var], var), 
                           calc(UGA_ex[,var], var))
  colnames(data_table) <- c("Value", "CORN", "SOYB", "WEAT", "USO", "UGA")
  return(data_table)
}

table_make_in <- function(var){
  values <- c("Min", "Median", "Mean", "Max", "STD DEV", "N")
  data_table <- data.frame(values, calc(CORN_in[, var], var), calc(SOYB_in[,var], var), 
                           calc(WEAT_in[,var], var), calc(USO_in[,var],var), 
                           calc(UGA_in[,var], var))
  colnames(data_table) <- c("Value", "CORN", "SOYB", "WEAT", "USO", "UGA")
  return(data_table)
}

#############################
### Create Tables
#############################

# In the paper, the tables with the most values will be used (default is in).
# but in the case of TDm, TD, Asset, etc, ex is used

Price_in <- table_make_in("Price")
NAV_in <- table_make_in("NAV")


NAV_r_in <- table_make_in("per_NAV_return")
ETF_r_in <- table_make_in("per_ETF_return")


TD_ex <- table_make_ex("TD")
TDa_in <- table_make_in("TDa")
TDm_ex <- table_make_ex("TDm")



# Clean up my removing all non-table elements
rm(CORN_ex, CORN_in, SOYB_ex, SOYB_in, WEAT_ex, WEAT_in, USO_ex, USO_in,
            UGA_ex, UGA_in)

