
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

# Ex is used by Default

#### Prices
Price_ex <- table_make_ex("Price")
NAV_ex <- table_make_ex("NAV")
Asset_ex <- table_make_ex("asset_basket")

prices_df <- rbind(Price_ex, NAV_ex, Asset_ex)
write_csv(prices_df, "~/Documents/etf_tracking/Analysis/Summary_Statistics/prices_summ.csv")

### Returns
ETF_r <- table_make_ex("per_ETF_return")
NAV_r <- table_make_ex("per_NAV_return")
Asset_r <- table_make_ex("per_asset_return")

returns_df <- rbind(ETF_r, NAV_r, Asset_r)
write_csv(prices_df, "~/Documents/etf_tracking/Analysis/Summary_Statistics/returns_summ.csv")


### TD
TD <- table_make_ex("TD")
TDm <- table_make_ex("TDm")
TDa <- table_make_ex("TDa")

TD_df <- rbind(TD, TDm, TDa)
write_csv(prices_df, "~/Documents/etf_tracking/Analysis/Summary_Statistics/TD_summ.csv")


### PD

PD <- table_make_ex("PD")
write_csv(PD, "~/Documents/etf_tracking/Analysis/Summary_Statistics/PD_summ.csv")



# Clean up my removing all non-table elements
rm(CORN_ex, CORN_in, SOYB_ex, SOYB_in, WEAT_ex, WEAT_in, USO_ex, USO_in,
            UGA_ex, UGA_in)

#### Inclusive Summary tables
# Prices
Prices_in <- table_make_in("Price")
NAV_in <- table_make_in("NAV")

prices_in <- rbind(Prices_in, NAV_in)
write_csv(prices_in, "~/Documents/etf_tracking/Analysis/Summary_Statistics/prices_in_summ.csv")

# returns
ETF_r_in <- table_make_in("per_ETF_return")
NAV_r_in <- table_make_in("per_NAV_return")

returns_in <- rbind(ETF_r_in, NAV_r_in)
write_csv(returns_in, "~/Documents/etf_tracking/Analysis/Summary_Statistics/returnss_in_summ.csv")

