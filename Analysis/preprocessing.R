
###############################################################################
#### Data Preprocessing #######################################################
###############################################################################
# Colburn Hassman
# colburn7@vt.edu
# January 25, 2021

# This code defines two data_pull functions, which then called in by other 
# R scripts to import, clean, and do some preprocessing of the ETF data

# data_pull_in <- data pull inclusive of roll dates (same as file org)
# data_pull_ex <- exclusive of roll dates (includes benchmark)

# The user inputs the ticker as a string and the function returns a cleaned
# dataframe for that ETF including returns and TD calculations

### Import Packages
library(tidyverse)


# Data pull inclusive of roll dates (full set, only Price and NAV)
data_pull_in <- function(TICKER){
  if(TICKER == "CORN"){
    CORN <- read.csv("~/Documents/etf_tracking/Data/DB_in/CORN_in.csv")
    
    # Set date as date and ensure it is ordered
    CORN$Date <- as.Date(CORN$Date, format = "%Y-%m-%d")
    CORN <- CORN[order(CORN$Date),]
    
    # Calculate log Returns
    CORN$per_ETF_return <- log(CORN$Price/lag(CORN$Price)) * 100
    CORN$per_NAV_return <- log(CORN$NAV/lag(CORN$NAV)) * 100
    
    # Calculate Tracking Differences
    CORN$TDa <- CORN$per_ETF_return - CORN$per_NAV_return
    
    # Calculate Premiums/Discounts
    CORN$PD <- (CORN$Price-CORN$NAV)/CORN$NAV
    
    # Return the cleaned and processed Dataframe
    return(CORN)
    
  }else if (TICKER == "SOYB"){
    SOYB <- read.csv("~/Documents/etf_tracking/Data/DB_in/SOYB_in.csv")
    SOYB$Date <- as.Date(SOYB$Date, format = "%Y-%m-%d")
    SOYB <- SOYB[order(SOYB$Date),]
    SOYB$per_ETF_return = log(SOYB$Price/lag(SOYB$Price)) * 100
    SOYB$per_NAV_return <- log(SOYB$NAV/lag(SOYB$NAV)) * 100
    SOYB$TDa <- SOYB$per_ETF_return - SOYB$per_NAV_return
    SOYB$PD <- (SOYB$Price - SOYB$NAV)/SOYB$NAV
    return(SOYB)
  }else if (TICKER == "WEAT"){
    WEAT <- read.csv("~/Documents/etf_tracking/Data/DB_in/WEAT_in.csv")
    WEAT$Date <- as.Date(WEAT$Date, format = "%Y-%m-%d")
    WEAT <- WEAT[order(WEAT$Date),]
    WEAT$per_ETF_return = log(WEAT$Price/lag(WEAT$Price)) * 100
    WEAT$per_NAV_return <- log(WEAT$NAV/lag(WEAT$NAV)) * 100
    WEAT$TDa <- WEAT$per_ETF_return - WEAT$per_NAV_return
    WEAT$PD <- (WEAT$Price - WEAT$NAV)/WEAT$NAV
    return(WEAT)
  }else if (TICKER == "USO"){
    USO <- read.csv("~/Documents/etf_tracking/Data/DB_in/USO_in.csv")
    USO$Date <- as.Date(USO$Date, format = "%Y-%m-%d")
    USO <- USO[order(USO$Date),]
    USO$per_ETF_return = log(USO$Price/lag(USO$Price)) * 100
    USO$per_NAV_return <- log(USO$NAV/lag(USO$NAV)) * 100
    USO$TDa <- USO$per_ETF_return - USO$per_NAV_return
    USO$PD <- (USO$Price - USO$NAV)/USO$NAV
    return(USO)
  }else if (TICKER == "UGA"){
    UGA <- read.csv("~/Documents/etf_tracking/Data/DB_in/UGA_in.csv")
    UGA$Date <- as.Date(UGA$Date, format = "%Y-%m-%d")
    UGA <- UGA[order(UGA$Date),]
    UGA$per_ETF_return = log(UGA$Price/lag(UGA$Price)) * 100
    UGA$per_NAV_return <- log(UGA$NAV/lag(UGA$NAV)) * 100
    UGA$TDa <- UGA$per_ETF_return - UGA$per_NAV_return
    UGA$PD <- (UGA$Price - UGA$NAV)/UGA$NAV
    return(UGA)
  }else{
    print("Error: Ticker not recognized")
  }
}
  
  
  


# Data pull exclusive of roll dates (full set, only Price and NAV)
data_pull_ex <- function(TICKER){
  if(TICKER == "CORN"){
    
    # Pull in data from CSV
    ETF <- read.csv("~/Documents/etf_tracking/Data/DB_ex/CORN_ex.csv")
    
    #Set Date as Date type and order by date
    ETF$DATE <- as.Date(ETF$DATE, format = "%m/%d/%Y")  
    ETF <- ETF[order(ETF$DATE),] 
    
    # Reconstruct Asset Basket
    ETF$asset_basket <- (ETF$`F1..35.` * 0.35) + (ETF$`F2..3.` * 0.3) + (ETF$`F3..35.` * 0.35)
    
    # Calculate Returns
    ETF$per_asset_return <- log(ETF$asset_basket/lag(ETF$asset_basket))* 100
    ETF$per_ETF_return <- log(ETF$Price/lag(ETF$Price)) * 100
    ETF$per_NAV_return <- log(ETF$NAV/lag(ETF$NAV)) * 100 
    # Calculate Tracking Differences
    ETF$TD <- ETF$per_ETF_return - ETF$per_asset_return
    ETF$TDa <- ETF$per_ETF_return - ETF$per_NAV_return
    ETF$TDm <- ETF$per_NAV_return - ETF$per_asset_return
    
    # Calculate Premiums/Discounts
    ETF$PD <- (ETF$Price-ETF$NAV)/ETF$NAV
    
    # omit Roll Dates
    ETF <- ETF[!(ETF$ROLL == 1),]
    
    # Delete Roll indicator column
    ETF$ROLL <- NULL
    
    # Return data
    return(ETF)

  }else if (TICKER == "SOYB"){
    ETF <- read.csv("~/Documents/etf_tracking/Data/DB_ex/SOYB_ex.csv")
    ETF$DATE <- as.Date(ETF$DATE, format = "%m/%d/%Y")  
    ETF <- ETF[order(ETF$DATE),]
    ETF$asset_basket <- (ETF$`F1..35.` * 0.35) + (ETF$`F2..3.` * 0.3) + (ETF$`F3..35.` * 0.35)
    ETF$per_asset_return <- log(ETF$asset_basket/lag(ETF$asset_basket))* 100 
    ETF$per_ETF_return <- log(ETF$Price/lag(ETF$Price)) * 100
    ETF$per_NAV_return <- log(ETF$NAV/lag(ETF$NAV)) * 100 
    ETF$TD <- ETF$per_ETF_return - ETF$per_asset_return
    ETF$TDa <- ETF$per_ETF_return - ETF$per_NAV_return
    ETF$TDm <- ETF$per_NAV_return - ETF$per_asset_return
    ETF$PD <- (ETF$Price-ETF$NAV)/ETF$NAV
    ETF <- ETF[!(ETF$ROLL == 1),]
    ETF$ROLL <- NULL
    return(ETF)
  }else if (TICKER == "WEAT"){
    ETF <- read.csv("~/Documents/etf_tracking/Data/DB_ex/WEAT_ex.csv")
    ETF$DATE <- as.Date(ETF$DATE, format = "%m/%d/%Y")  
    ETF <- ETF[order(ETF$DATE),]
    ETF$asset_basket <- (ETF$`F1..35.` * 0.35) + (ETF$`F2..3.` * 0.3) + (ETF$`F3..35.` * 0.35)
    ETF$per_asset_return <- log(ETF$asset_basket/lag(ETF$asset_basket))* 100 
    ETF$per_ETF_return <- log(ETF$Price/lag(ETF$Price)) * 100
    ETF$per_NAV_return <- log(ETF$NAV/lag(ETF$NAV)) * 100 
    ETF$TD <- ETF$per_ETF_return - ETF$per_asset_return
    ETF$TDa <- ETF$per_ETF_return - ETF$per_NAV_return
    ETF$TDm <- ETF$per_NAV_return - ETF$per_asset_return
    ETF$PD <- (ETF$Price-ETF$NAV)/ETF$NAV
    ETF <- ETF[!(ETF$ROLL == 1),]
    ETF$ROLL <- NULL
    return(ETF)
  }else if (TICKER == "USO"){
    ETF <- read.csv("~/Documents/etf_tracking/Data/DB_ex/USO_ex.csv")
    ETF$DATE <- as.Date(ETF$DATE, format = "%m/%d/%Y")  
    ETF <- ETF[order(ETF$DATE),]
    # For USO & UGA, no need to reconstruct asset basket
    ETF$per_asset_return <- log(ETF$Futures/lag(ETF$Futures))* 100 
    ETF$per_ETF_return <- log(ETF$Price/lag(ETF$Price)) * 100
    ETF$per_NAV_return <- log(ETF$NAV/lag(ETF$NAV)) * 100 
    ETF$TD <- ETF$per_ETF_return - ETF$per_asset_return
    ETF$TDa <- ETF$per_ETF_return - ETF$per_NAV_return
    ETF$TDm <- ETF$per_NAV_return - ETF$per_asset_return
    ETF$PD <- (ETF$Price-ETF$NAV)/ETF$NAV
    # For USO& UGA, want to rename "Futures" to "asset_basket" to be uniform
    colnames(ETF)[colnames(ETF)== "Futures"] <- "asset_basket"
    ETF <- ETF[!(ETF$ROLL == 1),]
    ETF$ROLL <- NULL
    return(ETF)
  }else if (TICKER == "UGA"){
    ETF <- read.csv("~/Documents/etf_tracking/Data/DB_ex/UGA_ex.csv")
    ETF$DATE <- as.Date(ETF$DATE, format = "%m/%d/%Y")  
    ETF <- ETF[order(ETF$DATE),]
    ETF$per_asset_return <- log(ETF$Futures/lag(ETF$Futures))* 100 
    ETF$per_ETF_return <- log(ETF$Price/lag(ETF$Price)) * 100
    ETF$per_NAV_return <- log(ETF$NAV/lag(ETF$NAV)) * 100 
    ETF$TD <- ETF$per_ETF_return - ETF$per_asset_return
    ETF$TDa <- ETF$per_ETF_return - ETF$per_NAV_return
    ETF$TDm <- ETF$per_NAV_return - ETF$per_asset_return
    ETF$PD <- (ETF$Price-ETF$NAV)/ETF$NAV
    colnames(ETF)[colnames(ETF)== "Futures"] <- "asset_basket"
    ETF <- ETF[!(ETF$ROLL == 1),]
    ETF$ROLL <- NULL
    return(ETF)
  } else{
      print("Error: Ticker not recognized")
  }
}


