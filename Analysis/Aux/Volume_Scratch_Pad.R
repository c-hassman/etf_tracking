# Volume Scratch Pad
# Colburn Hassman
# January 21, 2021

# The purpose of this script is to do the data work necessary to figure out
# which metric of volume is appropriate. 

library("tidyverse")

# IMPORT DATA
# Drop first row (containing NAs from return calc)
source("~/Documents/etf_tracking/Analysis/preprocessing.R")
CORN_in <- na.omit(data_pull_in("CORN"))
SOYB_in <- na.omit(data_pull_in("SOYB"))
WEAT_in <- na.omit(data_pull_in("WEAT"))
USO_in <- na.omit(data_pull_in("USO"))
UGA_in <- na.omit(data_pull_in("UGA"))

# Calculate multiple metrics of volume
plot(CORN_in$Date, CORN_in$Volume, type = "l")

CORN_in$logVolume <- log(CORN_in$Volume)
CORN_in$Turnover <- CORN_in$Price * CORN_in$Volume
CORN_in$logTurnover <- log(CORN_in$Turnover)

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
