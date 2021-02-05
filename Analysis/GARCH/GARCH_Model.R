################################################################################
######## GARCH Analysis  #######################################################
################################################################################
#
# Final Model specification for ETF Tracking Paper
#
# Colburn Hassman
# January 25, 2021

library(rugarch)

source("~/Documents/etf_tracking/Analysis/preprocessing.R")
CORN <- na.omit(data_pull_in("CORN"))
SOYB <- na.omit(data_pull_in("SOYB"))
WEAT <- na.omit(data_pull_in("WEAT"))
USO <- na.omit(data_pull_in("USO"))
UGA <- na.omit(data_pull_in("UGA"))


### Important: we are using non systematic TDa. What is included in 
# data_pull_in is systematic TDa. I overwrite it using this function:

non_sys <- function(data){
  model <- lm(data$Price ~ data$NAV)
  data$TDa <- model$residuals
  data <- as.data.frame(data)
  return(data)
}

CORN <- non_sys(CORN)
SOYB <- non_sys(SOYB)
WEAT <- non_sys(WEAT)
USO <- non_sys(USO)
UGA <- non_sys(UGA)



######### Base Model ###########################################################

# Omega is the variance intercept value

# Define Base GARCH Model
base_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                        mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
                        distribution.model = 'std')

#Calculate Base
CORN_base <- ugarchfit(data = CORN$TDa, spec = base_spec, solver = 'hybrid')
SOYB_base <- ugarchfit(data = SOYB$TDa, spec = base_spec, solver = 'hybrid')
WEAT_base <- ugarchfit(data = WEAT$TDa, spec = base_spec, solver = 'hybrid')
USO_base <- ugarchfit(data = USO$TDa, spec = base_spec, solver = 'hybrid')
UGA_base <- ugarchfit(data = UGA$TDa, spec = base_spec, solver = 'hybrid')

CORN_base
SOYB_base
WEAT_base
USO_base
UGA_base



####### Full Model #############################################################



###### External Regressors


# CORN
# Create a data frame of the relevant variables then convert to matrix
corn_ext <- as.matrix(data.frame(Volatility = CORN$Volatility, 
                                 #Volume = log(CORN$Volume),
                                # SPY_Vol = CORN$SPY_Vol,
                                 Roll = CORN$Roll))
soyb_ext <- as.matrix(data.frame(Volatility = SOYB$Volatility, 
                                 #Volume = log(SOYB$Volume),
                                 #SPY_Vol = SOYB$SPY_Vol,
                                 Roll = SOYB$Roll))
weat_ext <- as.matrix(data.frame(Volatility = WEAT$Volatility, 
                                 #Volume = log(WEAT$Volume),
                                 #SPY_Vol = WEAT$SPY_Vol,
                                 Roll = WEAT$Roll))
uso_ext <- as.matrix(data.frame(Volatility = USO$Volatility, 
                                 #Volume = log(USO$Volume),
                                 #SPY_Vol = USO$SPY_Vol,
                                 Roll = USO$Roll))
uga_ext <- as.matrix(data.frame(Volatility = UGA$Volatility, 
                                 #Volume = log(UGA$Volume),
                                #SPY_Vol = UGA$SPY_Vol,
                                 Roll = UGA$Roll))



# Define Full GARCH Model


corn_full_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1), 
                                                   external.regressors = corn_ext),
                              mean.model = list(armaOrder = c(0,0), 
                                                include.mean = FALSE),
                              distribution.model = 'std')

soyb_full_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1),
                                                   external.regressors = soyb_ext),
                              mean.model = list(armaOrder = c(0,0), 
                                                include.mean = FALSE),
                              distribution.model = 'std')

weat_full_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1),
                                                   external.regressors = weat_ext),
                              mean.model = list(armaOrder = c(0,0), 
                                                include.mean = FALSE),
                              distribution.model = 'std')

uso_full_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1),
                                                  external.regressors = uso_ext),
                            mean.model = list(armaOrder = c(0,0), 
                                              include.mean = FALSE),
                            distribution.model = 'std')

uga_full_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1),
                                                  external.regressors = uga_ext),
                            mean.model = list(armaOrder = c(0,0), 
                                              include.mean = FALSE),
                            distribution.model = 'std')

#Calculate Full

CORN_full <- ugarchfit(data = CORN$TDa, spec = corn_full_spec, solver = 'hybrid')

SOYB_full <- ugarchfit(data = SOYB$TDa, spec = soyb_full_spec, solver = 'hybrid')
WEAT_full <- ugarchfit(data = WEAT$TDa, spec = weat_full_spec, solver = 'hybrid')
USO_full <- ugarchfit(data = USO$TDa, spec = uso_full_spec, solver = 'hybrid')
UGA_full <- ugarchfit(data = UGA$TDa, spec = uga_full_spec, solver = 'hybrid')

CORN_full
SOYB_full
WEAT_full
USO_full
UGA_full


# Make Results Table

## Come back to#
ETFs <- c("CORN", "", "SOYB", "", "WEAT", "", "USO", "", "UGA", "")
vari <- rep(c("Coef", "COEF_SE"), 5)
results <- data.frame("ETF"= ETFs, 
                           "Variables" = vari)


temp <- cbind(CORN_base@fit[['coef']], CORN_base@fit[['se.coef']],
              SOYB_base@fit[['coef']], SOYB_base@fit[['se.coef']],
              WEAT_base@fit[['coef']], WEAT_base@fit[['se.coef']],
              USO_base@fit[['coef']], USO_base@fit[['se.coef']],
              UGA_base@fit[['coef']], UGA_base@fit[['se.coef']])

AIC <- rbind(infocriteria(CORN_base)[1], "",
             infocriteria(SOYB_base)[1], "",
             infocriteria(WEAT_base)[1], "",
             infocriteria(USO_base)[1], "",
             infocriteria(UGA_base)[1], "")

base_results <- cbind(base_results, temp)
base_results$AIC <- AIC


full_results <- data.frame("ETF"= ETFs, 
                           "Variables" = vari)


temp_full <- rbind(CORN_full@fit[['coef']], CORN_full@fit[['se.coef']],
              SOYB_full@fit[['coef']], SOYB_full@fit[['se.coef']],
              WEAT_full@fit[['coef']], WEAT_full@fit[['se.coef']],
              USO_full@fit[['coef']], USO_full@fit[['se.coef']],
              UGA_full@fit[['coef']], UGA_full@fit[['se.coef']])

AIC_full <- rbind(infocriteria(CORN_full)[1], "",
             infocriteria(SOYB_full)[1], "",
             infocriteria(WEAT_full)[1], "",
             infocriteria(USO_full)[1], "",
             infocriteria(UGA_full)[1], "")

full_results <- cbind(full_results, temp_full)
full_results$AIC <- AIC_full

write_csv(full_results, "~/Documents/etf_tracking/Analysis/GARCH/garch_results.csv")

### Plot Conditional Variance ##
par(mfrow = c(5, 2), mai = c(0.3, .65, 0.2, 0.05))
plot(CORN$Date, CORN_base@fit$sigma , type = "l", main = "Base Model",
     xlab = "", ylab = "CORN")
plot(CORN$Date, CORN_full@fit$sigma , type = "l", main = "Full Model",
     xlab = "", ylab = "")

plot(SOYB$Date, SOYB_base@fit$sigma , type = "l",
     xlab = "", ylab = "SOYB")
plot(SOYB$Date, SOYB_full@fit$sigma , type = "l",
     xlab = "", ylab = "")

plot(WEAT$Date, WEAT_base@fit$sigma , type = "l",
     xlab = "", ylab = "WEAT")
plot(WEAT$Date, WEAT_full@fit$sigma , type = "l",
     xlab = "", ylab = "")

plot(USO$Date, USO_base@fit$sigma , type = "l",
     xlab = "", ylab = "USO")
plot(USO$Date, USO_full@fit$sigma , type = "l",
     xlab = "", ylab = "")

plot(UGA$Date, UGA_base@fit$sigma , type = "l", 
     xlab = "", ylab = "UGA")
plot(UGA$Date, UGA_full@fit$sigma , type = "l",
     xlab = "", ylab = "")
dev.off()