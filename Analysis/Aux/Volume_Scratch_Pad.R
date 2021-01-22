# Volume Scratch Pad
# Colburn Hassman
# January 21, 2021

# The purpose of this script is to do the data work necessary to figure out
# which metric of volume is appropriate. 

library("tidyverse")

# IMPORT DATA
# Drop first row (containing NAs from return calc)
source("~/Documents/etf_tracking/Analysis/preprocessing.R")
CORN <- na.omit(data_pull_in("CORN"))
SOYB <- na.omit(data_pull_in("SOYB"))
WEAT <- na.omit(data_pull_in("WEAT"))
USO <- na.omit(data_pull_in("USO"))
UGA <- na.omit(data_pull_in("UGA"))


CORN$Turnover <- CORN$Price * CORN$Volume
SOYB$Turnover <- SOYB$Price * SOYB$Volume
WEAT$Turnover <- WEAT$Price * WEAT$Volume
USO$Turnover <- USO$Price * USO$Volume
UGA$Turnover <- UGA$Price * UGA$Volume


add_metric <- function(data){
  data$metric <- abs(data$per_ETF_return) / data$Turnover
  return(data)
}

CORN <- add_metric(CORN)
SOYB <- add_metric(SOYB)
WEAT <- add_metric(WEAT)
USO <- add_metric(USO)
UGA <- add_metric(UGA)


plot_vol <- function(data, title){
  plot(data$Date, data$metric, type = 'l', main = title)
}

par(mfrow = c(3, 2))
plot_vol(CORN, "Amihud Illiquidity: CORN")
plot_vol(SOYB, "SOYB")
plot_vol(WEAT, "WEAT")
plot_vol(USO, "USO")
plot_vol(UGA, "UGA")

dev.off()


par(mfrow = c(3, 1))
plot(CORN_in$Date, CORN_in$Volume, type = "l")
plot(CORN_in$Date, CORN_in$logVolume, type = "l")
plot(CORN_in$Date, CORN_in$logTurnover, type = "l")
dev.off()

tseries::adf.test(CORN_in$logVolume)
tseries::adf.test(CORN_in$logTurnover)

stats::Box.test(CORN_in$logVolume, type = "Ljung-Box", lag = 20)

corn_acf <- forecast::Acf(CORN_in$logVolume, plot = FALSE)
plot(corn_acf[2:21], main = "CORN")

plot(USO_in$Date, USO_in$Volume, type = "l")
plot(USO_in$Date, log(USO_in$Volume/lag(USO_in$Volume)), type = "l")

tseries::adf.test(na.omit(log(USO_in$Volume/lag(USO_in$Volume))))
stats::Box.test(na.omit(log(USO_in$Volume/lag(USO_in$Volume))), type = "Ljung-Box", lag = 20)

library(urca)
test <- ur.kpss(log(UGA_in$Volume/lag(UGA_in$Volume))) 
summary(test)

tseries::adf.test(log(SOYB_in$Volume))
