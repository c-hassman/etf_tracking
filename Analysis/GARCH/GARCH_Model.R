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


######### Base Model ###########################################################
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
plot(log(CORN$Volume), CORN$Volatility)

a <- lm(log(CORN$Volume) ~ CORN$Volatility)
summary(a)

plot(log(SOYB$Volume), SOYB$Volatility)
plot(log(USO$Volume), log(USO$Volatility))
plot(log(UGA$Volume), log(UGA$Volatility))

plot(log(CORN$Volatility), log(CORN$Volume))

# CORN
# Create a data frame of the relevant variables then convert to matrix
corn_ext <- as.matrix(data.frame(Volatility = CORN$Volatility, 
                                 Volume = log(CORN$Volume),
                                 Roll = CORN$Roll))
soyb_ext <- as.matrix(data.frame(Volatility = SOYB$Volatility, 
                                 Volume = log(SOYB$Volume),
                                 Roll = SOYB$Roll))
weat_ext <- as.matrix(data.frame(Volatility = WEAT$Volatility, 
                                 Volume = log(WEAT$Volume),
                                 Roll = WEAT$Roll))
uso_ext <- as.matrix(data.frame(Volatility = USO$Volatility, 
                                 Volume = log(USO$Volume),
                                 Roll = USO$Roll))
uga_ext <- as.matrix(data.frame(Volatility = UGA$Volatility, 
                                 Volume = log(UGA$Volume),
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


