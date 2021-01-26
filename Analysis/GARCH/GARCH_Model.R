
# GARCH Analysis
# Final Model specficiation for ETF Tracking Paper
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

# Define Full GARCH Model
# may need to break this out whne we have external variables
full_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                        mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
                        distribution.model = 'std')

#Calculate Base
CORN_full <- ugarchfit(data = CORN$TDa, spec = full_spec, solver = 'hybrid')
SOYB_full <- ugarchfit(data = SOYB$TDa, spec = full_spec, solver = 'hybrid')
WEAT_full <- ugarchfit(data = WEAT$TDa, spec = full_spec, solver = 'hybrid')
USO_full <- ugarchfit(data = USO$TDa, spec = full_spec, solver = 'hybrid')
UGA_full <- ugarchfit(data = UGA$TDa, spec = full_spec, solver = 'hybrid')

CORN_full
SOYB_full
WEAT_full
USO_full
UGA_full


