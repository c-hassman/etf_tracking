rm(list=ls())
library(readxl)
library(tidyverse)
library(ggthemes)


#------------------------Load in Data from Excel------------------------------#
data <- read_excel("G:/My Drive/3_Massa Undergrad Research/Neff Paper/Updated_Data/Data_Update_05.xlsx", 
                   sheet = "USO", col_types = c("date", "numeric", "numeric",
                                                "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric"))
data <- data[order(data$DATE),] #order by date

#--------------------Calculate Returns and Errors ---------------------------#
data$per_nav_return <- log(data$USO_NAV/lag(data$USO_NAV)) #calculate the percent daily return in NAV
data$per_etf_return <- log(data$USO_MID/lag(data$USO_MID)) #calculate the percent daily return in ETF
data$per_asset_return <- log(data$Futures/lag(data$Futures)) #calculate the percent daily return in futures
data$etf_asset_error <- data$per_etf_return - data$per_asset_return #calculate error (%) between etf and asset

#--------------------Exploratory Plots -------------------------------------#
#Plot of Return Error over time
qplot(data$DATE, (data$etf_asset_error * 100), geom='line') + theme_bw() + 
  ggtitle('Daily Return Error: USO ETF - Asset Basket') +
  ylab('Error (%)') + xlab('') + ylim(-7,5)


data_long <- data #recreate new dataframe
names(data_long)[names(data_long) == 'USO_MID'] <- 'Price' #change USO_MID colname to Price
names(data_long)[names(data_long) == 'USO_NAV'] <- 'NAV' #change USO_NAV colname to NAV

#transform data into proper storage (long format)
data_long <- data_long %>%   
  select(DATE, Price, NAV) %>%
  gather(key = 'variable', value = 'value', -DATE)

#Plot of NAV and Price over time
ggplot(data = data_long, aes(x = DATE, y = value)) +
  geom_line(aes(linetype = variable)) + theme_bw() +
  ggtitle('USO Price and NAV') + labs(linetype="") + 
  theme(legend.position = 'bottom') + xlab("") + 
  ylab('Dollar per Share')

#------------------Model---------------------------------------------------#
#Simple OLS
simple_model <- lm(data$etf_asset_error ~ data$per_asset_return, data= data)
summary(simple_model)

#Dummy Model

dummy_model <- lm(abs(data$etf_asset_error) ~ abs(data$per_etf_return) + data$`CL Day Before Roll` +
                    data$`CL Day After Roll` + data$`CL Feb` + data$`CL Mar` + data$`CL April` +
                    data$`CL May` + data$`CL June` + data$`CL July` + data$`CL Aug` + data$`CL Sept` +
                    data$`CL Oct` + data$`CL Nov` + data$`CL Dec` + data$`CL 2013` + data$`CL 2014` +
                    data$`CL 2015` + data$`CL 2016` + data$`CL 2017` + data$`CL 2018` + data$`CL 2019` +
                    data$`CL 2020` + data$`CL STEO` + data$`CL Drilling Prod` + data$`CL Petro Supply/Prod` +
                    data$`CL Annual Energy Outlook`)
summary(dummy_model)
