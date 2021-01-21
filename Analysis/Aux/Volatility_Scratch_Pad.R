# Volatility Scratch Pad
# Colburn Hassman
# January 21, 2021

# Meant to explore volatility data and make sure specification is good


# IMPORT DATA
# Drop first row (containing NAs from return calc)
source("~/Documents/etf_tracking/Analysis/preprocessing.R")
CORN <- na.omit(data_pull_in("CORN"))
SOYB <- na.omit(data_pull_in("SOYB"))
WEAT <- na.omit(data_pull_in("WEAT"))
USO <- na.omit(data_pull_in("USO"))
UGA <- na.omit(data_pull_in("UGA"))


add_vol <- function(data){
  data$vol <- (data$High - data$Low) / data$Price
  return(data)
}

CORN <- add_vol(CORN)
SOYB <- add_vol(SOYB)
WEAT <- add_vol(WEAT)
USO <- add_vol(USO)
UGA <- add_vol(UGA)


plot_vol <- function(data){
  plot(data$Date, data$vol, type = 'l')
}

plot_vol(CORN)
plot_vol(SOYB)
plot_vol(WEAT)
plot_vol(USO)
plot_vol(UGA)

vol_test <- function(data){
  print(tseries::adf.test(data$vol))
  print(urca::ur.kpss(data$vol))
}

vol_test(CORN)
