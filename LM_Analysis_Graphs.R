########################################################
# Tracking Difference Analysis
########################################################
# Colburn Hassman
# December 15, 2020

library(tseries)
library(xts)
library(tidyverse)
library(fpp2)
library(stats)
library(gridExtra)
library(grid)

setwd("~/Documents/etf_tracking")

#-----------------Import Data from Excel and order------------#
CORN <- read.csv("~/Documents/etf_tracking/corn_master.csv")
SOYB <- read.csv("~/Documents/etf_tracking/soyb_master.csv")
WEAT <- read.csv("~/Documents/etf_tracking/weat_master.csv")
USO <- read.csv("~/Documents/etf_tracking/uso_master.csv")
UGA <- read.csv("~/Documents/etf_tracking/uga_master.csv")



#------------------Date Manipulation and Cleaning---------------#

CORN$DATE <- as.Date(CORN$DATE, format = "%m/%d/%Y")  #set date 
CORN <- CORN[order(CORN$DATE),] #order by date
CORN$asset_basket <- (CORN$`F1..35.` * 0.35) + (CORN$`F2..3.` * 0.3) + (CORN$`F3..35.` * 0.35) #reconstruct asset basket
CORN$per_asset_return <- log(CORN$asset_basket/lag(CORN$asset_basket))* 100 # calculate percent asset basket return
CORN$per_ETF_return <- log(CORN$CORN_MID/lag(CORN$CORN_MID)) * 100#calculate percent ETF return
CORN$per_nav_return <- log(CORN$CORN_NAV/lag(CORN$CORN_NAV)) * 100 # Calculate NAV Return


SOYB$DATE <- as.Date(SOYB$DATE, format = "%m/%d/%Y") # Set date
SOYB <- SOYB[order(SOYB$DATE),] #order by date
SOYB$asset_basket <- (SOYB$`F1..35.` * 0.35) + (SOYB$`F2..3.` * 0.30) + (SOYB$`F3..35.` * 0.35)
SOYB$per_asset_return <- log(SOYB$asset_basket / lag(SOYB$asset_basket)) * 100
SOYB$per_ETF_return <- log(SOYB$SOYB_MID / lag(SOYB$SOYB_MID)) * 100
SOYB$per_nav_return <- log(SOYB$SOYB_NAV/lag(SOYB$SOYB_NAV)) * 100


WEAT$DATE <- as.Date(WEAT$DATE, format = "%m/%d/%Y") #set date 
WEAT <- WEAT[order(WEAT$DATE),] #order by date
WEAT$asset_basket <- (WEAT$`F1..35.` * 0.35) + (WEAT$`F2..3.` * 0.3) + (WEAT$`F3..35.` * 0.35) #reconstruct asset basket
WEAT$per_asset_return <- log(WEAT$asset_basket/lag(WEAT$asset_basket))* 100 # calculate percent asset basket return
WEAT$per_ETF_return <- log(WEAT$WEAT_MID/lag(WEAT$WEAT_MID)) * 100#calculate percent ETF return
WEAT$per_nav_return <- log(WEAT$WEAT_NAV/lag(WEAT$WEAT_NAV)) * 100 


USO$DATE <- as.Date(USO$DATE, format = "%m/%d/%Y")
USO <- USO[order(USO$DATE),]
USO$per_asset_return <- log(USO$Futures/lag(USO$Futures)) * 100
USO$per_ETF_return <- log(USO$USO_MID /lag(USO$USO_MID)) * 100
USO$per_nav_return <- log(USO$USO_NAV/lag(USO$USO_NAV)) * 100

UGA$DATE <- as.Date(UGA$DATE, format = "%m/%d/%Y")
UGA <- UGA[order(UGA$DATE),]
UGA$per_asset_return <- log(UGA$Futures/lag(UGA$Futures)) * 100
UGA$per_ETF_return <- log(UGA$UGA_MID/lag(UGA$UGA_MID)) * 100
UGA$per_nav_return <- log(UGA$UGA_NAV/lag(UGA$UGA_NAV)) * 100



#--- More data cleaning
# The code below handles the issue of roll dates 
CORN <- na.omit(CORN)  
CORN <- CORN[!(CORN$ROLL == 1),] #directly remove roll

SOYB <- na.omit(SOYB)
SOYB <- SOYB[!(SOYB$ROLL == 1),]

WEAT <- na.omit(WEAT) 
WEAT <- WEAT[!(WEAT$ROLL == 1),]


USO <- na.omit(USO)
USO <- USO[!(USO$ROLL == 1),]

UGA <- na.omit(UGA) 
UGA <- UGA[!(UGA$ROLL == 1),]



####### Test for Unit Root in Returns
# # Benchmark Returns
# adf.test(CORN$per_asset_return)
# adf.test(SOYB$per_asset_return)
# adf.test(WEAT$per_asset_return)
# adf.test(USO$per_asset_return)
# adf.test(UGA$per_asset_return)
# # NAV Returns
# adf.test(CORN$per_nav_return)
# adf.test(SOYB$per_nav_return)
# adf.test(WEAT$per_nav_return)
# adf.test(USO$per_nav_return)
# adf.test(UGA$per_nav_return)
# # Price Returns
# adf.test(CORN$per_ETF_return)
# adf.test(SOYB$per_ETF_return)
# adf.test(WEAT$per_ETF_return)
# adf.test(USO$per_ETF_return)
# adf.test(UGA$per_ETF_return)


###### Fit LM Model
# [!!!]Need to store residuals
# (Total) Tracking Difference
TD_corn <- lm(CORN$per_ETF_return ~ CORN$per_asset_return)
TD_soyb <- lm(SOYB$per_ETF_return ~ SOYB$per_asset_return)
TD_weat <- lm(WEAT$per_ETF_return ~ WEAT$per_asset_return)
TD_uso <- lm(USO$per_ETF_return ~ USO$per_asset_return)
TD_uga <- lm(UGA$per_ETF_return ~ UGA$per_asset_return)
# summary(TD_corn)
# summary(TD_soyb)
# summary(TD_weat)
# summary(TD_uso)
# summary(TD_uga)

# Managerial Tracking Difference
TDm_corn <- lm(CORN$per_nav_return ~ CORN$per_asset_return)
TDm_soyb <- lm(SOYB$per_nav_return ~ SOYB$per_asset_return)
TDm_weat <- lm(WEAT$per_nav_return ~ WEAT$per_asset_return)
TDm_uso <- lm(USO$per_nav_return ~ USO$per_asset_return)
TDm_uga <- lm(UGA$per_nav_return ~ UGA$per_asset_return)
# summary(TDm_corn)
# summary(TDm_soyb)
# summary(TDm_weat)
# summary(TDm_uso)
# summary(TDm_uga)

# Arbitrage Tracking Difference
TDa_corn <- lm(CORN$per_ETF_return ~ CORN$per_nav_return)
TDa_soyb <- lm(SOYB$per_ETF_return ~ SOYB$per_nav_return)
TDa_weat <- lm(WEAT$per_ETF_return ~ WEAT$per_nav_return)
TDa_uso <- lm(USO$per_ETF_return ~ USO$per_nav_return)
TDa_uga <- lm(UGA$per_ETF_return ~ UGA$per_nav_return)
# summary(TDa_corn)
# summary(TDa_soyb)
# summary(TDa_weat)
# summary(TDa_uso)
# summary(TDa_uga)

###############################################################################
# # Lets visualize some results
# # First I want to create dataframes which store the coefficients and se
# 
# # Create list of names of Each ETF
# TD_name = list('CORN', 'SOYB', 'WEAT', 'USO', 'UGA')
# 
# # Create a list to hold all the models
# TD_list = list(TD_corn, TD_soyb, TD_weat, TD_uso, TD_uga)
# TDm_list = list(TDm_corn, TDm_soyb, TDm_weat, TDm_uso, TDm_uga)
# TDa_list = list(TDa_corn, TDa_soyb, TDa_weat, TDa_uso, TDa_uga)
# 
# # Create empty list to store coefs and se
# TD_alpha = list()
# TD_alpha_se = list()
# TD_beta = list()
# TD_beta_se = list()
# 
# TDm_alpha = list()
# TDm_alpha_se = list()
# TDm_beta = list()
# TDm_beta_se = list()
# 
# TDa_alpha = list()
# TDa_alpha_se = list()
# TDa_beta = list()
# TDa_beta_se = list()
# 
# # TD for loop
# for (i in TD_list){
#   m = summary(i, robust = TRUE)
#   TD_alpha <- c(TD_alpha, m$coefficients[1])
#   TD_beta <- c(TD_beta, m$coefficients[2])
#   TD_alpha_se <- c(TD_alpha_se, m$coefficients[3])
#   TD_beta_se <- c(TD_beta_se, m$coefficients[4])
# }
# 
# # TDm for loop
# for (i in TDm_list){
#   m = summary(i, robust = TRUE)
#   TDm_alpha <- c(TDm_alpha, m$coefficients[1])
#   TDm_beta <- c(TDm_beta, m$coefficients[2])
#   TDm_alpha_se <- c(TDm_alpha_se, m$coefficients[3])
#   TDm_beta_se <- c(TDm_beta_se, m$coefficients[4])
# }
# 
# # TDa for loop
# 
# for (i in TDa_list){
#   m = summary(i, robust = TRUE)
#   TDa_alpha <- c(TDa_alpha, m$coefficients[1])
#   TDa_beta <- c(TDa_beta, m$coefficients[2])
#   TDa_alpha_se <- c(TDa_alpha_se, m$coefficients[3])
#   TDa_beta_se <- c(TDa_beta_se, m$coefficients[4])
# }
# 
# # Lists of lists to store results
# TD <- list('ETF' = TD_name, 'Alpha' = TD_alpha, 'Alpha_SE' = TD_alpha_se,
#            'Beta' = TD_beta, 'Beta_SE' = TD_beta_se)
# TDm <- list(TD_name, TDm_alpha, TDm_alpha_se, TDm_beta, TDm_beta_se)
# TDa <- list(TD_name, TDa_alpha, TDa_alpha_se, TDa_beta, TDa_beta_se)
# 
# # This horrible implmentation is because I am used to using python
# TD_df <- as.data.frame(matrix(unlist(TD), nrow=length(unlist(TD[1]))))
# TDm_df <- as.data.frame(matrix(unlist(TDm), nrow=length(unlist(TDm[1]))))
# TDa_df <- as.data.frame(matrix(unlist(TDa), nrow=length(unlist(TDa[1]))))
# 
# colnames(TD_df) <- c('ETF', 'Alpha', 'Alpha_SE', 'Beta', 'Beta_SE')
# colnames(TDm_df) <- c('ETF', 'Alpha', 'Alpha_SE', 'Beta', 'Beta_SE')
# colnames(TDa_df) <- c('ETF', 'Alpha', 'Alpha_SE', 'Beta', 'Beta_SE')
# 
# # clean up messy enviroment
# rm(list = setdiff(ls(), c('TD_df', 'TDm_df', 'TDa_df')))
# 
# # Create upper and lower bounds
# df_list = list(TD_df, TDm_df, TDa_df)
# multi = 1.96
# 
# 
# # Did not work, R doesnt like for loops
# # for (i in df_list){
# #   print(i)
# #   print(deparse(substitute(i)))
# #   i <- cbind(i$ETF, as.data.frame(lapply(i[2:5], as.numeric))) # for some reason strings
# #   i$Alpha_min = i$Alpha - (multi * i$Alpha_SE)
# #   i$Alpha_max = i$Alpha + (multi * i$Alpha_SE)
# #   i$Beta_min = i$Beta - (multi * i$Beta_SE)
# #   i$Beta_max = i$Beta + (multi * i$Beta_SE)
# # }
# 
# library(dbplyr)
# TD_df <- cbind(TD_df$ETF, as.data.frame(lapply(TD_df[2:5], as.numeric)))
# TDm_df <- cbind(TDm_df$ETF, as.data.frame(lapply(TDm_df[2:5], as.numeric)))
# TDa_df <- cbind(TDa_df$ETF, as.data.frame(lapply(TDa_df[2:5], as.numeric)))
# 
# df_list <- lapply(df_list, mutate, Alpha_min = Alpha - (multi * Alpha_SE))
# df_list <- lapply(df_list, mutate, Alpha_max = Alpha + (multi * Alpha_SE))
# df_list <- lapply(df_list, mutate, Beta_min = Beta - (multi * Beta_SE))
# df_list <- lapply(df_list, mutate, Beta_max = Beta + (multi * Beta_SE))
# 
# TD_df <- as.data.frame(df_list[1])
# TDm_df <- as.data.frame(df_list[2])
# TDa_df <- as.data.frame(df_list[3])
# 
# # Rename the first column
# colnames(TD_df)[1] <- 'ETF'
# colnames(TDm_df)[1] <- 'ETF'
# colnames(TDa_df)[1] <- 'ETF'
# 
# # Set ETF Colum as an ordered factor
# TD_df$ETF <- factor(TD_df$ETF, levels = TD_df$ETF)
# TDm_df$ETF <- factor(TDm_df$ETF, levels = TDm_df$ETF)
# TDa_df$ETF <- factor(TDa_df$ETF, levels = TDa_df$ETF)
# 
# # TD Alpha
# g1 <- ggplot()+
#   geom_errorbar(data = TD_df, mapping = aes(x = ETF, ymin = Alpha_min,
#                                             ymax = Alpha_max), width = 0.25) +
#   geom_point(data = TD_df, aes(x = ETF, y = Alpha)) +
#   ggtitle('Alpha Coefficient Estimates') +
#   ylab('Total TD') + theme_bw() + xlab('') +
#   theme(plot.margin=unit(c(0.1,0.05,-0.4,0.05), "cm")) +
#   ylim(-0.041, 0.03) + geom_hline(yintercept = 0, linetype = 'dashed')
# 
# #TDm Alpha
# g2 <- ggplot()+
#   geom_errorbar(data = TDm_df, mapping = aes(x = ETF, ymin = Alpha_min,
#                                             ymax = Alpha_max), width = 0.25) +
#   geom_point(data = TDm_df, aes(x = ETF, y = Alpha)) +
#   #ggtitle('Managerial TD Alpha Coefficient Estimates') +
#   ylab('Managerial TD') + theme_bw() + xlab('') +
#   theme(plot.margin=unit(c(-0.05,0.05,-0.1,0.05), "cm")) +
#   ylim(-0.041, 0.03) + geom_hline(yintercept = 0, linetype = 'dashed')
# 
# #TDa Alpha
# g3 <- ggplot()+
#   geom_errorbar(data = TDa_df, mapping = aes(x = ETF, ymin = Alpha_min,
#                                             ymax = Alpha_max), width = 0.25) +
#   geom_point(data = TDa_df, aes(x = ETF, y = Alpha)) +
#   #ggtitle('Arbitrage TD Alpha Coefficient Estimates') +
#   ylab('Arbitrage TD') + theme_bw() + xlab('') +
#   theme(plot.margin=unit(c(-0.05,0.05,-0.1,0.05), "cm")) +
#   ylim(-0.041, 0.03) + geom_hline(yintercept = 0, linetype = 'dashed')
# 
# #TD Beta
# g4 <- ggplot()+
#   geom_errorbar(data = TD_df, mapping = aes(x = ETF, ymin = Beta_min,
#                                             ymax = Beta_max), width = 0.25) +
#   geom_point(data = TD_df, aes(x = ETF, y = Beta)) +
#   ggtitle('Beta Coefficient Estimates') +
#   ylab('') + theme_bw() + xlab('') +
#   theme(plot.margin=unit(c(0.1,0.05,-0.4,0.05), "cm")) +
#   ylim(0.88, 1.02) + geom_hline(yintercept = 1, linetype = 'dashed')
# 
# #TDm Beta
# g5 <- ggplot()+
#   geom_errorbar(data = TDm_df, mapping = aes(x = ETF, ymin = Beta_min,
#                                             ymax = Beta_max), width = 0.25) +
#   geom_point(data = TDm_df, aes(x = ETF, y = Beta)) +
#   #ggtitle('Managerial TD Beta Coefficient Estimates') +
#   ylab('') + theme_bw() + xlab('') +
#   theme(plot.margin=unit(c(-0.05,0.05,-0.1,0.05), "cm")) +
#   ylim(0.88, 1.02) + geom_hline(yintercept = 1, linetype = 'dashed')
# 
# # TDa Beta
# g6 <- ggplot()+
#   geom_errorbar(data = TDa_df, mapping = aes(x = ETF, ymin = Beta_min,
#                                             ymax = Beta_max), width = 0.25) +
#   geom_point(data = TDa_df, aes(x = ETF, y = Beta)) +
#   #ggtitle('Arbitrage TD Beta Coefficient Estimates') +
#   ylab('') + theme_bw() + xlab('') +
#   theme(plot.margin=unit(c(-0.05,0.05,-0.1,0.05), "cm")) +
#   ylim(0.88, 1.02) + geom_hline(yintercept = 1, linetype = 'dashed')
# 
# grid.arrange(g1,g4,g2,g5,g3,g6, nrow = 3)

###############################################################################
### Price Graphs

#pdf(paste0(path,"/Figures/Prices.pdf"),height = 8,
#    width = 10)
par(mfrow = c(5, 3), mai = c(0.3, .65, 0.2, 0.05))
# CORN
plot(CORN$DATE, CORN$CORN_MID, type = "l", main = "ETF",
     xlab = "", ylab = "CORN\n $/Share")
plot(CORN$DATE, CORN$CORN_NAV, type = "l", main = "NAV",
     xlab = "", ylab = "$/Share")
plot(CORN$DATE, CORN$asset_basket, type = "l", main = "Asset Basket",
     xlab = "", ylab = "Cents/Bushel")
# SOYB 
plot(SOYB$DATE, SOYB$SOYB_MID, type = "l", main = "",
     xlab = "", ylab = "SOYB\n $/Share")
plot(SOYB$DATE, SOYB$SOYB_NAV, type = "l", main = "",
     xlab = "", ylab = "$/Share")
plot(SOYB$DATE, SOYB$asset_basket, type = "l", main = "",
     xlab = "", ylab = "Cents/Bushel")
#WEAT
plot(WEAT$DATE, WEAT$WEAT_MID, type = "l", main = "",
     xlab = "", ylab = "WEAT\n $/Share")
plot(WEAT$DATE, WEAT$WEAT_NAV, type = "l", main = "",
     xlab = "", ylab = "$/Share")
plot(WEAT$DATE, WEAT$asset_basket, type = "l", main = "",
     xlab = "", ylab = "Cents/Bushel")
#USO
plot(USO$DATE, USO$USO_MID, type = "l", main = "", 
     xlab = "", ylab = "USO\n $/Share") 
plot(USO$DATE, USO$USO_NAV, type = "l", main = "", 
     xlab = "", ylab = "$/Share") 
plot(USO$DATE, USO$Futures , type = "l", main = "", 
     xlab = "", ylab = "$/Barrel") 
# UGA
plot(UGA$DATE, UGA$UGA_MID, type = "l", main = "",
     xlab = "", ylab = "UGA\n $/Share") 
plot(UGA$DATE, UGA$UGA_NAV, type = "l", main = "",
     xlab = "", ylab = "$/Share") 
plot(UGA$DATE, UGA$Futures, type = "l", main = "",
     xlab = "", ylab = "$/Gallon") 
#dev.off()




###############################################################################
### Return Graphs


#pdf(paste0(path,"/Figures/Returns.pdf"),height = 8,
#    width = 10)
par(mfrow = c(5, 3), mai = c(0.3, .55, 0.2, 0.05))
# CORN
plot(CORN$DATE, CORN$per_ETF_return, type = "l", main = "ETF Percent Return",
     xlab = "", ylab = "CORN")
plot(CORN$DATE, CORN$per_nav_return, type = "l", main = "NAV Percent Return",
     xlab = "", ylab = "")
plot(CORN$DATE, CORN$per_asset_return, type = "l", main = "Asset Basket Percent Return",
     xlab = "", ylab = "")
# SOYB 
plot(SOYB$DATE, SOYB$per_ETF_return, type = "l", main = "",
     xlab = "", ylab = "SOYB")
plot(SOYB$DATE, SOYB$per_nav_return, type = "l", main = "",
     xlab = "", ylab = "")
plot(SOYB$DATE, SOYB$per_asset_return, type = "l", main = "",
     xlab = "", ylab = "")
#WEAT
plot(WEAT$DATE, WEAT$per_ETF_return, type = "l", main = "",
     xlab = "", ylab = "WEAT")
plot(WEAT$DATE, WEAT$per_nav_return, type = "l", main = "",
     xlab = "", ylab = "")
plot(WEAT$DATE, WEAT$per_asset_return, type = "l", main = "",
     xlab = "", ylab = "")
#USO
plot(USO$DATE, USO$per_ETF_return, type = "l", main = "", 
     xlab = "", ylab = "USO") 
plot(USO$DATE, USO$per_nav_return, type = "l", main = "", 
     xlab = "", ylab = "") 
plot(USO$DATE, USO$per_asset_return , type = "l", main = "", 
     xlab = "", ylab = "") 
# UGA
plot(UGA$DATE, UGA$per_ETF_return, type = "l", main = "",
     xlab = "", ylab = "UGA") 
plot(UGA$DATE, UGA$per_nav_return, type = "l", main = "",
     xlab = "", ylab = "") 
plot(UGA$DATE, UGA$per_asset_return, type = "l", main = "",
     xlab = "", ylab = "") 
#dev.off()



###############################################################################
### Tracking Differences

# Total TD
#pdf(paste0(path,"/Figures/Total_TD.pdf"),height = 8,
#    width = 10)
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN$DATE, (CORN$per_ETF_return - CORN$per_asset_return), type = "l", main = "CORN",
     xlab = "", ylab = "")
plot(SOYB$DATE, (SOYB$per_ETF_return - SOYB$per_asset_return), type = "l", main = "SOYB",
     xlab = "", ylab = "")
plot(WEAT$DATE, (WEAT$per_ETF_return - WEAT$per_asset_return), type = "l", main = "WEAT",
     xlab = "", ylab = "Tracking Difference")
plot(USO$DATE, (USO$per_ETF_return - USO$per_asset_return), type = "l", main = "USO", 
     xlab = "", ylab = "") 
plot(UGA$DATE, (UGA$per_ETF_return - UGA$per_asset_return), type = "l", main = "UGA",
     xlab = "", ylab = "") 
#dev.off()

# Managerial TD
#pdf(paste0(path,"/Figures/Managerial_TD.pdf"),height = 8,
#    width = 10)
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN$DATE, (CORN$per_nav_return - CORN$per_asset_return), type = "l", main = "CORN",
     xlab = "", ylab = "")
plot(SOYB$DATE, (SOYB$per_nav_return - SOYB$per_asset_return), type = "l", main = "SOYB",
     xlab = "", ylab = "")
plot(WEAT$DATE, (WEAT$per_nav_return - WEAT$per_asset_return), type = "l", main = "WEAT",
     xlab = "", ylab = "Tracking Difference")
plot(USO$DATE, (USO$per_nav_return - USO$per_asset_return), type = "l", main = "USO", 
     xlab = "", ylab = "") 
plot(UGA$DATE, (UGA$per_nav_return - UGA$per_asset_return), type = "l", main = "UGA",
     xlab = "", ylab = "") 
#dev.off()

# Arbitrage TD
#pdf(paste0(path,"/Figures/Arbitrage_TD.pdf"),height = 8,
#    width = 10)
par(mfrow = c(3, 2), mai = c(0.25, 0.5, 0.2, 0.05))
plot(CORN$DATE, (CORN$per_ETF_return - CORN$per_nav_return), type = "l", main = "CORN",
     xlab = "", ylab = "")
plot(SOYB$DATE, (SOYB$per_ETF_return - SOYB$per_nav_return), type = "l", main = "SOYB",
     xlab = "", ylab = "")
plot(WEAT$DATE, (WEAT$per_ETF_return - WEAT$per_nav_return), type = "l", main = "WEAT",
     xlab = "", ylab = "Tracking Difference")
plot(USO$DATE, (USO$per_ETF_return - USO$per_nav_return), type = "l", main = "USO", 
     xlab = "", ylab = "") 
plot(UGA$DATE, (UGA$per_ETF_return - UGA$per_nav_return), type = "l", main = "UGA",
     xlab = "", ylab = "") 
#dev.off()


#### Combined
par(mfrow = c(5, 3), mai = c(0.3, 0.55, 0.2, 0.05))
# CORN
plot(CORN$DATE, (CORN$per_ETF_return - CORN$per_asset_return), type = "l", main = "Total Tracking Difference",
     xlab = "", ylab = "CORN")
plot(CORN$DATE, (CORN$per_nav_return - CORN$per_asset_return), type = "l", main = "Managerial Tracking Difference",
     xlab = "", ylab = "")
plot(CORN$DATE, (CORN$per_ETF_return - CORN$per_nav_return), type = "l", main = "Arbitrage Tracking Difference",
     xlab = "", ylab = "")
# SOYB
plot(SOYB$DATE, (SOYB$per_ETF_return - SOYB$per_asset_return), type = "l", main = "",
     xlab = "", ylab = "SOYB")
plot(SOYB$DATE, (SOYB$per_nav_return - SOYB$per_asset_return), type = "l", main = "",
     xlab = "", ylab = "")
plot(SOYB$DATE, (SOYB$per_ETF_return - SOYB$per_nav_return), type = "l", main = "",
     xlab = "", ylab = "")
# WEAT
plot(WEAT$DATE, (WEAT$per_ETF_return - WEAT$per_asset_return), type = "l", main = "",
     xlab = "", ylab = "WEAT")
plot(WEAT$DATE, (WEAT$per_nav_return - WEAT$per_asset_return), type = "l", main = "",
     xlab = "", ylab = "")
plot(WEAT$DATE, (WEAT$per_ETF_return - WEAT$per_nav_return), type = "l", main = "",
     xlab = "", ylab = "")
# USO
plot(USO$DATE, (USO$per_ETF_return - USO$per_asset_return), type = "l", main = "", 
     xlab = "", ylab = "USO") 
plot(USO$DATE, (USO$per_nav_return - USO$per_asset_return), type = "l", main = "", 
     xlab = "", ylab = "") 
plot(USO$DATE, (USO$per_ETF_return - USO$per_nav_return), type = "l", main = "", 
     xlab = "", ylab = "") 
# UGA
plot(UGA$DATE, (UGA$per_ETF_return - UGA$per_asset_return), type = "l", main = "",
     xlab = "", ylab = "UGA") 
plot(UGA$DATE, (UGA$per_nav_return - UGA$per_asset_return), type = "l", main = "",
     xlab = "", ylab = "") 
plot(UGA$DATE, (UGA$per_ETF_return - UGA$per_nav_return), type = "l", main = "",
     xlab = "", ylab = "") 
dev.off()



#############################################################################
### Scatterplots

#### !!! Need to add labels: make the first labels two lined
par(mfrow = c(5, 3), mai = c(0.5, 0.65, 0.2, 0.05))
# CORN
plot(CORN$per_ETF_return, CORN$per_asset_return, type = "p", main = "Total Tracking Difference",
     xlab = "ETF Return", ylab = "CORN\n Asset Return", pch = 19)
plot(CORN$per_nav_return,  CORN$per_asset_return, type = "p", main = "Managerial Tracking Difference",
     xlab = "NAV Return", ylab = "Asset Return", pch = 19)
plot(CORN$per_ETF_return, CORN$per_nav_return, type = "p", main = "Arbitrage Tracking Difference",
     xlab = "ETF Return", ylab = "NAV Return", pch = 19)
# SOYB
plot(SOYB$per_ETF_return, SOYB$per_asset_return, type = "p", main = "",
     xlab = "ETF Return", ylab = "SOYB\n Asset Return", pch = 19)
plot(SOYB$per_nav_return, SOYB$per_asset_return, type = "p", main = "",
     xlab = "NAV Return", ylab = "Asset Return", pch = 19)
plot(SOYB$per_ETF_return, SOYB$per_nav_return, type = "p", main = "",
     xlab = "ETF Return", ylab = "NAV Return", pch = 19)
# WEAT
plot(WEAT$per_ETF_return, WEAT$per_asset_return, type = "p", main = "",
     xlab = "ETF Return", ylab = "WEAT\n Asset Retunr", pch = 19)
plot(WEAT$per_nav_return, WEAT$per_asset_return, type = "p", main = "",
     xlab = "NAV Return", ylab = "Asset Return", pch = 19)
plot(WEAT$per_ETF_return, WEAT$per_nav_return, type = "p", main = "",
     xlab = "ETF Return", ylab = "NAV Return", pch = 19)
# USO
plot(USO$per_ETF_return, USO$per_asset_return, type = "p", main = "", 
     xlab = "ETF Return", ylab = "USO\n Asset Return", pch = 19) 
plot(USO$per_nav_return, USO$per_asset_return, type = "p", main = "", 
     xlab = "NAV Return", ylab = "Asset Return", pch = 19) 
plot(USO$per_ETF_return, USO$per_nav_return, type = "p", main = "", 
     xlab = "ETF Return", ylab = "NAV Return", pch = 19) 
# UGA
plot(UGA$per_ETF_return, UGA$per_asset_return, type = "p", main = "",
     xlab = "ETF Return", ylab = "UGA\n Asset Return", pch = 19) 
plot(UGA$per_nav_return, UGA$per_asset_return, type = "p", main = "",
     xlab = "NAV Return", ylab = "Asset Return", pch = 19) 
plot(UGA$per_ETF_return, UGA$per_nav_return, type = "p", main = "",
     xlab = "ETF Return", ylab = "NAV Return", pch = 19) 
dev.off()




###############################################################################
### Residuals Graphs


#pdf(paste0(path,"/Figures/Residuals.pdf"),height = 8,
#    width = 10)
par(mfrow = c(5, 3), mai = c(0.3, .55, 0.2, 0.05))
# CORN
plot(CORN$DATE, TD_corn$residuals, type = "l", main = "Epsilon TD",
     xlab = "", ylab = "CORN")
plot(CORN$DATE, TDm_corn$residuals, type = "l", main = "Epsilon TDm",
     xlab = "", ylab = "")
plot(CORN$DATE, TDa_corn$residuals, type = "l", main = "Epsilon TDa",
     xlab = "", ylab = "")
# SOYB 
plot(SOYB$DATE, TD_soyb$residuals, type = "l", main = "",
     xlab = "", ylab = "SOYB")
plot(SOYB$DATE, TDm_soyb$residuals, type = "l", main = "",
     xlab = "", ylab = "")
plot(SOYB$DATE, TDa_soyb$residuals, type = "l", main = "",
     xlab = "", ylab = "")
#WEAT
plot(WEAT$DATE, TD_weat$residuals, type = "l", main = "",
     xlab = "", ylab = "WEAT")
plot(WEAT$DATE, TDm_weat$residuals, type = "l", main = "",
     xlab = "", ylab = "")
plot(WEAT$DATE, TDa_weat$residuals, type = "l", main = "",
     xlab = "", ylab = "")
#USO
plot(USO$DATE, TD_uso$residuals, type = "l", main = "", 
     xlab = "", ylab = "USO") 
plot(USO$DATE, TDm_uso$residuals, type = "l", main = "", 
     xlab = "", ylab = "") 
plot(USO$DATE, TDa_uso$residuals, type = "l", main = "", 
     xlab = "", ylab = "") 
# UGA
plot(UGA$DATE, TD_uga$residuals, type = "l", main = "",
     xlab = "", ylab = "UGA") 
plot(UGA$DATE, TDm_uga$residuals, type = "l", main = "",
     xlab = "", ylab = "") 
plot(UGA$DATE, TDa_uga$residuals, type = "l", main = "",
     xlab = "", ylab = "") 
#dev.off()

## Squared Residuals
#pdf(paste0(path,"/Figures/SQ_Residuals.pdf"),height = 8,
#    width = 10)
par(mfrow = c(5, 3), mai = c(0.3, .55, 0.2, 0.05))
# CORN
plot(CORN$DATE, TD_corn$residuals^2, type = "l", main = "Epsilon TD",
     xlab = "", ylab = "CORN")
plot(CORN$DATE, TDm_corn$residuals^2, type = "l", main = "Epsilon TDm",
     xlab = "", ylab = "")
plot(CORN$DATE, TDa_corn$residuals^2, type = "l", main = "Epsilon TDa",
     xlab = "", ylab = "")
# SOYB 
plot(SOYB$DATE, TD_soyb$residuals^2, type = "l", main = "",
     xlab = "", ylab = "SOYB")
plot(SOYB$DATE, TDm_soyb$residuals^2, type = "l", main = "",
     xlab = "", ylab = "")
plot(SOYB$DATE, TDa_soyb$residuals^2, type = "l", main = "",
     xlab = "", ylab = "")
#WEAT
plot(WEAT$DATE, TD_weat$residuals^2, type = "l", main = "",
     xlab = "", ylab = "WEAT")
plot(WEAT$DATE, TDm_weat$residuals^2, type = "l", main = "",
     xlab = "", ylab = "")
plot(WEAT$DATE, TDa_weat$residuals^2, type = "l", main = "",
     xlab = "", ylab = "")
#USO
plot(USO$DATE, TD_uso$residuals^2, type = "l", main = "", 
     xlab = "", ylab = "USO") 
plot(USO$DATE, TDm_uso$residuals^2, type = "l", main = "", 
     xlab = "", ylab = "") 
plot(USO$DATE, TDa_uso$residuals^2, type = "l", main = "", 
     xlab = "", ylab = "") 
# UGA
plot(UGA$DATE, TD_uga$residuals^2, type = "l", main = "",
     xlab = "", ylab = "UGA") 
plot(UGA$DATE, TDm_uga$residuals^2, type = "l", main = "",
     xlab = "", ylab = "") 
plot(UGA$DATE, TDa_uga$residuals^2, type = "l", main = "",
     xlab = "", ylab = "") 
#dev.off()

