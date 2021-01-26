###############################################################################
##### ARCH Analysis ###########################################################
###############################################################################
# Colburn Hassman
# colburn7@vt.edu
# January 18, 2021

# PURPOSE: Motivate the GARCH Analysis. Uses TDa

# IMPORT PACKAGES

# IMPORT DATA
source("~/Documents/etf_tracking/Analysis/preprocessing.R")
CORN_in <- data_pull_in("CORN")
SOYB_in <- data_pull_in("SOYB")
WEAT_in <- data_pull_in("WEAT")
USO_in <- data_pull_in("USO")
UGA_in <- data_pull_in("UGA")

# MINCER ZARNOWITZ
mz_fun <- function(data){
  data <- na.omit(data) # Omits NAs created by return
  mod <- lm(data$per_ETF_return ~ data$per_NAV_return) #runs linear model
  data$Ea <- mod$residuals # assign residuals to arbitrage episilon
  return(data)
}

CORN_in <- mz_fun(CORN_in)
SOYB_in <- mz_fun(SOYB_in)
WEAT_in <- mz_fun(WEAT_in)
USO_in <- mz_fun(USO_in)
UGA_in <- mz_fun(UGA_in)



##### Non-Squared Residuals ###################################################

# GRAPH RESIDUALS
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN_in$Date, CORN_in$Ea, type = "l", main = "CORN",
     xlab = "", ylab = "")
plot(SOYB_in$Date, SOYB_in$Ea, type = "l", main = "SOYB",
     xlab = "", ylab = "")
plot(WEAT_in$Date, WEAT_in$Ea, type = "l", main = "WEAT",
     xlab = "", ylab = "Percent Error Squared")
plot(USO_in$Date, USO_in$Ea, type = "l", main = "USO", 
     xlab = "", ylab = "")
plot(UGA_in$Date, UGA_in$Ea, type = "l", main = "UGA",
     xlab = "", ylab = "")
dev.off()


# ACF GRAPH: RESIDUALS
corn_acf <- forecast::Acf(CORN_in$Ea, plot = FALSE)
soyb_acf <- forecast::Acf(SOYB_in$Ea, plot = FALSE)
weat_acf <- forecast::Acf(WEAT_in$Ea, plot = FALSE)
uso_acf <- forecast::Acf(USO_in$Ea, plot = FALSE)
uga_acf <- forecast::Acf(UGA_in$Ea, plot = FALSE)


par(mfrow = c(3, 2) , mai = c(0.2, 0.3, 0.55, 0.5))  # come back and change format
plot(corn_acf[2:21], main = "CORN")
plot(soyb_acf[2:21], main = "SOYB")
plot(weat_acf[2:21], main = "WEAT")
plot(uso_acf[2:21], main = "USO")
plot(uga_acf[2:21], main = "UGA")
dev.off()

# DICKEY-FULLER: RESIDUALS
# In every case, we reject the null hypothesis and assume stationarity
tseries::adf.test(CORN_in$Ea)
tseries::adf.test(SOYB_in$Ea)
tseries::adf.test(WEAT_in$Ea)
tseries::adf.test(USO_in$Ea)
tseries::adf.test(UGA_in$Ea)

# LJUNG-BOX TEST: RESIDUALS
# Evidence of serial correlaction
stats::Box.test(CORN_in$Ea, type = "Ljung-Box", lag = 20)
stats::Box.test(SOYB_in$Ea, type = "Ljung-Box", lag = 20)
stats::Box.test(WEAT_in$Ea, type = "Ljung-Box", lag = 20)
stats::Box.test(USO_in$Ea, type = "Ljung-Box", lag = 20)
stats::Box.test(UGA_in$Ea, type = "Ljung-Box", lag = 20)

##### Squared Residuals #######################################################

# GRAPH SQUARED RESIDUALS
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN_in$Date, CORN_in$Ea^2, type = "l", main = "CORN",
     xlab = "", ylab = "")
plot(SOYB_in$Date, SOYB_in$Ea^2, type = "l", main = "SOYB",
     xlab = "", ylab = "")
plot(WEAT_in$Date, WEAT_in$Ea^2, type = "l", main = "WEAT",
     xlab = "", ylab = "Percent Error Squared")
plot(USO_in$Date, USO_in$Ea^2, type = "l", main = "USO", 
     xlab = "", ylab = "")
plot(UGA_in$Date, UGA_in$Ea^2, type = "l", main = "UGA",
     xlab = "", ylab = "")
dev.off()


# ACF GRAPH: SQUARED RESIDUALS
corn2_acf <- forecast::Acf(CORN_in$Ea^2, plot = FALSE)
soyb2_acf <- forecast::Acf(SOYB_in$Ea^2, plot = FALSE)
weat2_acf <- forecast::Acf(WEAT_in$Ea^2, plot = FALSE)
uso2_acf <- forecast::Acf(USO_in$Ea^2, plot = FALSE)
uga2_acf <- forecast::Acf(UGA_in$Ea^2, plot = FALSE)

par(mfrow = c(3, 2) , mai = c(0.3, 0.3, 0.55, 0.5)) 
plot(corn2_acf[2:21], main = "CORN")
plot(soyb2_acf[2:21], main = "SOYB")
plot(weat2_acf[2:21], main = "WEAT")
plot(uso2_acf[2:21], main = "USO")
plot(uga2_acf[2:21], main = "UGA")
dev.off()

# LJUNG-BOX TEST: SQUARED RESIDUALS
stats::Box.test(CORN_in$Ea^2, type = "Ljung-Box", lag = 20)
stats::Box.test(SOYB_in$Ea^2, type = "Ljung-Box", lag = 20)
stats::Box.test(WEAT_in$Ea^2, type = "Ljung-Box", lag = 20)
stats::Box.test(USO_in$Ea^2, type = "Ljung-Box", lag = 20)
stats::Box.test(UGA_in$Ea^2, type = "Ljung-Box", lag = 20)

