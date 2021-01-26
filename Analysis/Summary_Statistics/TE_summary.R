################################################################################
#### Initial Tracking Errror Tables ############################################
################################################################################

# Colburn Hassman
# January 26, 2021

source("~/Documents/etf_tracking/Analysis/preprocessing.R")
CORN <- na.omit(data_pull_ex("CORN"))
SOYB <- na.omit(data_pull_ex("SOYB"))
WEAT <- na.omit(data_pull_ex("WEAT"))
USO <- na.omit(data_pull_ex("USO"))
UGA <- na.omit(data_pull_ex("UGA"))

ETFs <- c("CORN", "SOYB", "WEAT", "USO", "UGA")

#### Average Absolute Return Difference ########################################

AARD <- data.frame("ETF" = ETFs)
AARD$TD <- c(mean(abs(CORN$TD)),
             mean(abs(SOYB$TD)),
             mean(abs(WEAT$TD)),
             mean(abs(USO$TD)),
             mean(abs(UGA$TD)))

AARD$TDa <- c(mean(abs(CORN$TDa)),
             mean(abs(SOYB$TDa)),
             mean(abs(WEAT$TDa)),
             mean(abs(USO$TDa)),
             mean(abs(UGA$TDa)))

AARD$TDm <- c(mean(abs(CORN$TDm)),
             mean(abs(SOYB$TDm)),
             mean(abs(WEAT$TDm)),
             mean(abs(USO$TDm)),
             mean(abs(UGA$TDm)))

write_csv(AARD, "~/Documents/etf_tracking/Analysis/Summary_Statistics/AARD_summ.csv")

#### Standard Deviation of Return Differences ####################################

SDRD <- data.frame("ETF" = ETFs)
SDRD$TD <- c(sd(CORN$TD),
          sd(SOYB$TD),
          sd(WEAT$TD),
          sd(USO$TD),
          sd(UGA$TD))

SDRD$TDa <- c(sd(CORN$TDa),
             sd(SOYB$TDa),
             sd(WEAT$TDa),
             sd(USO$TDa),
             sd(UGA$TDa))


SDRD$TDm <- c(sd(CORN$TDm),
             sd(SOYB$TDm),
             sd(WEAT$TDm),
             sd(USO$TDm),
             sd(UGA$TDm))

write_csv(SDRD, "~/Documents/etf_tracking/Analysis/Summary_Statistics/SDRD_summ.csv")
