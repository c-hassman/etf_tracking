################################################################################
####  MZ Analysis ##############################################################
################################################################################

# Colburn Hassman
# colburn7@vt.edu
# January 26, 2021

# IMPORT PACKAGES
library(gridExtra)

# IMPORT DATA
source("~/Documents/etf_tracking/Analysis/preprocessing.R")

CORN <- data_pull_ex("CORN")
SOYB <- data_pull_ex("SOYB")
WEAT <- data_pull_ex("WEAT")
USO <- data_pull_ex("USO")
UGA <- data_pull_ex("UGA")

#### Fit Models ################################################################

TD_corn <- lm(CORN$per_ETF_return ~ CORN$per_asset_return)
TD_soyb <- lm(SOYB$per_ETF_return ~ SOYB$per_asset_return)
TD_weat <- lm(WEAT$per_ETF_return ~ WEAT$per_asset_return)
TD_uso <- lm(USO$per_ETF_return ~ USO$per_asset_return)
TD_uga <- lm(UGA$per_ETF_return ~ UGA$per_asset_return)

TDm_corn <- lm(CORN$per_NAV_return ~ CORN$per_asset_return)
TDm_soyb <- lm(SOYB$per_NAV_return ~ SOYB$per_asset_return)
TDm_weat <- lm(WEAT$per_NAV_return ~ WEAT$per_asset_return)
TDm_uso <- lm(USO$per_NAV_return ~ USO$per_asset_return)
TDm_uga <- lm(UGA$per_NAV_return ~ UGA$per_asset_return)

TDa_corn <- lm(CORN$per_ETF_return ~ CORN$per_NAV_return)
TDa_soyb <- lm(SOYB$per_ETF_return ~ SOYB$per_NAV_return)
TDa_weat <- lm(WEAT$per_ETF_return ~ WEAT$per_NAV_return)
TDa_uso <- lm(USO$per_ETF_return ~ USO$per_NAV_return)
TDa_uga <- lm(UGA$per_ETF_return ~ UGA$per_NAV_return)


t <- -19.8969
df <- 2050
  
2 * pt(t, df = df)
se_calc <- function(model){
  k = length(model$coefficients)-1
  SSE = sum(model$residuals**2)
  n = length(model$residuals)
  return(sqrt(SSE/(n-(1+k))))
}

ETFs <- c("CORN", "SOYB", "WEAT", "USO", "UGA")

TD_se <- c(se_calc(TD_corn),
        se_calc(TD_soyb),
        se_calc(TD_weat),
        se_calc(TD_uso),
        se_calc(TD_uga))

TDa_se <- c(se_calc(TDa_corn),
           se_calc(TDa_soyb),
           se_calc(TDa_weat),
           se_calc(TDa_uso),
           se_calc(TDa_uga))

TDm_se <- c(se_calc(TDm_corn),
           se_calc(TDm_soyb),
           se_calc(TDm_weat),
           se_calc(TDm_uso),
           se_calc(TDm_uga))

TD_se_df <- as.data.frame(cbind(ETFs, TD_se, TDa_se, TDm_se))
write_csv(TD_se_df, "~/Documents/etf_tracking/Analysis/Summary_Statistics/TD_SE.csv")

##### Result Tables ############################################################

#### TD
# Make dataframe of coefficients
TD <- data.frame(summary(TD_corn)$coefficients)
TD <- rbind(TD, data.frame(summary(TD_soyb)$coefficients), 
            data.frame(summary(TD_weat)$coefficients),
            data.frame(summary(TD_uso)$coefficients),
            data.frame(summary(TD_uga)$coefficients))

# Add a column of R Squared Values
TD <- cbind(TD,  c(summary(TD_corn)$r.squared, NA, 
                  summary(TD_soyb)$r.squared, NA, 
                  summary(TD_weat)$r.squared, NA, 
                  summary(TD_uso)$r.squared, NA, 
                  summary(TD_uga)$r.squared, NA))
# Write to CSV
write.csv(TD, "~/Documents/etf_tracking/Analysis/MZ/TD_results.csv")


#### TDm
TDm <- data.frame(summary(TDm_corn)$coefficients)
TDm <- rbind(TDm, data.frame(summary(TDm_soyb)$coefficients), 
            data.frame(summary(TDm_weat)$coefficients),
            data.frame(summary(TDm_uso)$coefficients),
            data.frame(summary(TDm_uga)$coefficients))

TDm <- cbind(TDm,  c(summary(TDm_corn)$r.squared, NA, 
                   summary(TDm_soyb)$r.squared, NA, 
                   summary(TDm_weat)$r.squared, NA, 
                   summary(TDm_uso)$r.squared, NA, 
                   summary(TDm_uga)$r.squared, NA))

write.csv(TDm, "~/Documents/etf_tracking/Analysis/MZ/TDm_results.csv")

#### TDa
TDa <- data.frame(summary(TDa_corn)$coefficients)
TDa <- rbind(TDa, data.frame(summary(TDa_soyb)$coefficients), 
            data.frame(summary(TDa_weat)$coefficients),
            data.frame(summary(TDa_uso)$coefficients),
            data.frame(summary(TDa_uga)$coefficients))

TDa <- cbind(TDa,  c(summary(TDa_corn)$r.squared, NA, 
                   summary(TDa_soyb)$r.squared, NA, 
                   summary(TDa_weat)$r.squared, NA, 
                   summary(TDa_uso)$r.squared, NA, 
                   summary(TDa_uga)$r.squared, NA))

write.csv(TDa, "~/Documents/etf_tracking/Analysis/MZ/TDa_results.csv")

#### Graph Residuals ###########################################################

# [-1] removes the first date (which contains na, so no residuals)

par(mfrow = c(5, 3), mai = c(0.3, .65, 0.2, 0.05))
# CORN
plot(CORN$DATE[-1], TD_corn$residuals, type = "l", main = "TD Residuals",
     xlab = "", ylab = "CORN")
plot(CORN$DATE[-1], TDm_corn$residuals, type = "l", main = "TDm Residuals",
     xlab = "", ylab = "")
plot(CORN$DATE[-1], TDa_corn$residuals, type = "l", main = "TDa Residuals",
     xlab = "", ylab = "")
# SOYB 
plot(SOYB$DATE[-1], TD_soyb$residuals, type = "l", main = "",
     xlab = "", ylab = "SOYB")
plot(SOYB$DATE[-1], TDm_soyb$residuals, type = "l", main = "",
     xlab = "", ylab = "")
plot(SOYB$DATE[-1], TDa_soyb$residuals, type = "l", main = "",
     xlab = "", ylab = "")
#WEAT
plot(WEAT$DATE[-1], TD_weat$residuals, type = "l", main = "",
     xlab = "", ylab = "WEAT")
plot(WEAT$DATE[-1], TDm_weat$residuals, type = "l", main = "",
     xlab = "", ylab = "")
plot(WEAT$DATE[-1], TDa_weat$residuals, type = "l", main = "",
     xlab = "", ylab = "")
#USO
plot(USO$DATE[-1], TD_uso$residuals, type = "l", main = "", 
     xlab = "", ylab = "USO") 
plot(USO$DATE[-1], TDm_uso$residuals, type = "l", main = "", 
     xlab = "", ylab = "") 
plot(USO$DATE[-1], TDa_uso$residuals , type = "l", main = "", 
     xlab = "", ylab = "") 
# UGA
plot(UGA$DATE[-1], TD_uga$residuals, type = "l", main = "",
     xlab = "", ylab = "UGA") 
plot(UGA$DATE[-1], TDm_uga$residuals, type = "l", main = "",
     xlab = "", ylab = "") 
plot(UGA$DATE[-1], TDa_uga$residuals, type = "l", main = "",
     xlab = "", ylab = "") 

dev.off()



##### Visualize Results ########################################################

# Create list of names of Each ETF
TD_name = list('CORN', 'SOYB', 'WEAT', 'USO', 'UGA')

# Create a list to hold all the models
TD_list = list(TD_corn, TD_soyb, TD_weat, TD_uso, TD_uga)
TDm_list = list(TDm_corn, TDm_soyb, TDm_weat, TDm_uso, TDm_uga)
TDa_list = list(TDa_corn, TDa_soyb, TDa_weat, TDa_uso, TDa_uga)

# Create empty list to store coefs and se
TD_alpha = list()
TD_alpha_se = list()
TD_beta = list()
TD_beta_se = list()

TDm_alpha = list()
TDm_alpha_se = list()
TDm_beta = list()
TDm_beta_se = list()

TDa_alpha = list()
TDa_alpha_se = list()
TDa_beta = list()
TDa_beta_se = list()

# TD for loop
for (i in TD_list){
  m = summary(i, robust = TRUE)
  TD_alpha <- c(TD_alpha, as.numeric(m$coefficients[1]))
  TD_beta <- c(TD_beta, as.numeric(m$coefficients[2]))
  TD_alpha_se <- c(TD_alpha_se, as.numeric(m$coefficients[3]))
  TD_beta_se <- c(TD_beta_se, as.numeric(m$coefficients[4]))
}

# TDm for loop
for (i in TDm_list){
  m = summary(i, robust = TRUE)
  TDm_alpha <- c(TDm_alpha, as.numeric(m$coefficients[1]))
  TDm_beta <- c(TDm_beta, as.numeric(m$coefficients[2]))
  TDm_alpha_se <- c(TDm_alpha_se, as.numeric(m$coefficients[3]))
  TDm_beta_se <- c(TDm_beta_se, as.numeric(m$coefficients[4]))
}

# TDa for loop

for (i in TDa_list){
  m = summary(i, robust = TRUE)
  TDa_alpha <- c(TDa_alpha, as.numeric(m$coefficients[1]))
  TDa_beta <- c(TDa_beta, as.numeric(m$coefficients[2]))
  TDa_alpha_se <- c(TDa_alpha_se, as.numeric(m$coefficients[3]))
  TDa_beta_se <- c(TDa_beta_se, as.numeric(m$coefficients[4]))
}

# Lists of lists to store results
TD <- list('ETF' = TD_name, 'Alpha' = TD_alpha, 'Alpha_SE' = TD_alpha_se,
           'Beta' = TD_beta, 'Beta_SE' = TD_beta_se)
TDm <- list(TD_name, TDm_alpha, TDm_alpha_se, TDm_beta, TDm_beta_se)
TDa <- list(TD_name, TDa_alpha, TDa_alpha_se, TDa_beta, TDa_beta_se)

# This horrible implmentation is because I am used to using python
TD_df <- as.data.frame(matrix(unlist(TD), nrow=length(unlist(TD[1]))))
TDm_df <- as.data.frame(matrix(unlist(TDm), nrow=length(unlist(TDm[1]))))
TDa_df <- as.data.frame(matrix(unlist(TDa), nrow=length(unlist(TDa[1]))))

colnames(TD_df) <- c('ETF', 'Alpha', 'Alpha_SE', 'Beta', 'Beta_SE')
colnames(TDm_df) <- c('ETF', 'Alpha', 'Alpha_SE', 'Beta', 'Beta_SE')
colnames(TDa_df) <- c('ETF', 'Alpha', 'Alpha_SE', 'Beta', 'Beta_SE')

# clean up messy enviroment
rm(list = setdiff(ls(), c('TD_df', 'TDm_df', 'TDa_df')))

# Create upper and lower bounds
#df_list = list(TD_df, TDm_df, TDa_df)
multi = 1.96


TD_df <- cbind(TD_df$ETF, as.data.frame(lapply(TD_df[2:5], as.numeric)))
TDm_df <- cbind(TDm_df$ETF, as.data.frame(lapply(TDm_df[2:5], as.numeric)))
TDa_df <- cbind(TDa_df$ETF, as.data.frame(lapply(TDa_df[2:5], as.numeric)))


typeof(TD_df$Alpha)



# Tried to do this a better way... but not great
TD_df$Alpha_min <- TD_df$Alpha - (multi * TD_df$Alpha_SE)
TD_df$Alpha_max <- TD_df$Alpha + (multi * TD_df$Alpha_SE)
TD_df$Beta_min <- TD_df$Beta - (multi * TD_df$Beta_SE)
TD_df$Beta_max <- TD_df$Beta + (multi * TD_df$Beta_SE)

TDm_df$Alpha_min <- TDm_df$Alpha - (multi * TDm_df$Alpha_SE)
TDm_df$Alpha_max <- TDm_df$Alpha + (multi * TDm_df$Alpha_SE)
TDm_df$Beta_min <- TDm_df$Beta - (multi * TDm_df$Beta_SE)
TDm_df$Beta_max <- TDm_df$Beta + (multi * TDm_df$Beta_SE)

TDa_df$Alpha_min <- TDa_df$Alpha - (multi * TDa_df$Alpha_SE)
TDa_df$Alpha_max <- TDa_df$Alpha + (multi * TDa_df$Alpha_SE)
TDa_df$Beta_min <- TDa_df$Beta - (multi * TDa_df$Beta_SE)
TDa_df$Beta_max <- TDa_df$Beta + (multi * TDa_df$Beta_SE)

# Rename the first column
colnames(TD_df)[1] <- 'ETF'
colnames(TDm_df)[1] <- 'ETF'
colnames(TDa_df)[1] <- 'ETF'

# Set ETF Colum as an ordered factor
TD_df$ETF <- factor(TD_df$ETF, levels = TD_df$ETF)
TDm_df$ETF <- factor(TDm_df$ETF, levels = TDm_df$ETF)
TDa_df$ETF <- factor(TDa_df$ETF, levels = TDa_df$ETF)




# TD Alpha
g1 <- ggplot()+
  geom_errorbar(data = TD_df, mapping = aes(x = ETF, ymin = Alpha_min,
                                            ymax = Alpha_max), width = 0.25) +
  geom_point(data = TD_df, aes(x = ETF, y = Alpha)) +
  ggtitle('Alpha Coefficient Estimates') +
  ylab('Total TD') + theme_bw() + xlab('') +
  theme(plot.margin=unit(c(0.1,0.05,-0.4,0.05), "cm")) +
  ylim(-0.041, 0.03) + geom_hline(yintercept = 0, linetype = 'dashed')

#TDm Alpha
g2 <- ggplot()+
  geom_errorbar(data = TDm_df, mapping = aes(x = ETF, ymin = Alpha_min,
                                             ymax = Alpha_max), width = 0.25) +
  geom_point(data = TDm_df, aes(x = ETF, y = Alpha)) +
  #ggtitle('Managerial TD Alpha Coefficient Estimates') +
  ylab('Managerial TD') + theme_bw() + xlab('') +
  theme(plot.margin=unit(c(-0.05,0.05,-0.1,0.05), "cm")) +
  ylim(-0.041, 0.03) + geom_hline(yintercept = 0, linetype = 'dashed')

#TDa Alpha
g3 <- ggplot()+
  geom_errorbar(data = TDa_df, mapping = aes(x = ETF, ymin = Alpha_min,
                                             ymax = Alpha_max), width = 0.25) +
  geom_point(data = TDa_df, aes(x = ETF, y = Alpha)) +
  #ggtitle('Arbitrage TD Alpha Coefficient Estimates') +
  ylab('Arbitrage TD') + theme_bw() + xlab('') +
  theme(plot.margin=unit(c(-0.05,0.05,-0.1,0.05), "cm")) +
  ylim(-0.041, 0.03) + geom_hline(yintercept = 0, linetype = 'dashed')

#TD Beta
g4 <- ggplot()+
  geom_errorbar(data = TD_df, mapping = aes(x = ETF, ymin = Beta_min,
                                            ymax = Beta_max), width = 0.25) +
  geom_point(data = TD_df, aes(x = ETF, y = Beta)) +
  ggtitle('Beta Coefficient Estimates') +
  ylab('') + theme_bw() + xlab('') +
  theme(plot.margin=unit(c(0.1,0.05,-0.4,0.05), "cm")) +
  ylim(0.88, 1.02) + geom_hline(yintercept = 1, linetype = 'dashed')

#TDm Beta
g5 <- ggplot()+
  geom_errorbar(data = TDm_df, mapping = aes(x = ETF, ymin = Beta_min,
                                             ymax = Beta_max), width = 0.25) +
  geom_point(data = TDm_df, aes(x = ETF, y = Beta)) +
  #ggtitle('Managerial TD Beta Coefficient Estimates') +
  ylab('') + theme_bw() + xlab('') +
  theme(plot.margin=unit(c(-0.05,0.05,-0.1,0.05), "cm")) +
  ylim(0.88, 1.02) + geom_hline(yintercept = 1, linetype = 'dashed')

# TDa Beta
g6 <- ggplot()+
  geom_errorbar(data = TDa_df, mapping = aes(x = ETF, ymin = Beta_min,
                                             ymax = Beta_max), width = 0.25) +
  geom_point(data = TDa_df, aes(x = ETF, y = Beta)) +
  #ggtitle('Arbitrage TD Beta Coefficient Estimates') +
  ylab('') + theme_bw() + xlab('') +
  theme(plot.margin=unit(c(-0.05,0.05,-0.1,0.05), "cm")) +
  ylim(0.88, 1.02) + geom_hline(yintercept = 1, linetype = 'dashed')

grid.arrange(g1,g4,g2,g5,g3,g6, nrow = 3)


