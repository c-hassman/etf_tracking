################################################################################
#### Augmented Dickey Fuller Scratchpad ########################################
################################################################################

# Colburn Hassman
# January 28, 2021

# Import Data
source("~/Documents/etf_tracking/Analysis/preprocessing.R")

CORN <- na.omit(data_pull_ex("CORN"))
SOYB <- na.omit(data_pull_ex("SOYB"))
WEAT <- na.omit(data_pull_ex("WEAT"))
USO <- na.omit(data_pull_ex("USO"))
UGA <- na.omit(data_pull_ex("UGA"))

### ETF Returns
corn_etf_adf <- tseries::adf.test(CORN$per_ETF_return, k = 20)
soyb_etf_adf <- tseries::adf.test(SOYB$per_ETF_return, k = 20)
weat_etf_adf <- tseries::adf.test(WEAT$per_ETF_return, k = 20)
uso_etf_adf <- tseries::adf.test(USO$per_ETF_return, k = 20)
uga_etf_adf <- tseries::adf.test(UGA$per_ETF_return, k = 20)

etf_df <- data.frame(ETF = c("CORN", "SOYB", "WEAT", "USO", "UGA"))
etf_df$Statistic <- c(corn_etf_adf$statistic,
                      soyb_etf_adf$statistic,
                      weat_etf_adf$statistic,
                      uso_etf_adf$statistic,
                      uga_etf_adf$statistic)
etf_df$p_value <- c(corn_etf_adf$p.value,
                    soyb_etf_adf$p.value,
                    weat_etf_adf$p.value,
                    uso_etf_adf$p.value,
                    uga_etf_adf$p.value)
write_csv(etf_df, "~/Documents/etf_tracking/Analysis/ADF/etf.csv")

### NAV Returns
corn_nav_adf <- tseries::adf.test(CORN$per_NAV_return, k = 20)
soyb_nav_adf <- tseries::adf.test(SOYB$per_NAV_return, k = 20)
weat_nav_adf <- tseries::adf.test(WEAT$per_NAV_return, k = 20)
uso_nav_adf <- tseries::adf.test(USO$per_NAV_return, k = 20)
uga_nav_adf <- tseries::adf.test(UGA$per_NAV_return, k = 20)

nav_df <- data.frame(ETF = c("CORN", "SOYB", "WEAT", "USO", "UGA"))
nav_df$Statistic <- c(corn_nav_adf$statistic,
                      soyb_nav_adf$statistic,
                      weat_nav_adf$statistic,
                      uso_nav_adf$statistic,
                      uga_nav_adf$statistic)
nav_df$p_value <- c(corn_nav_adf$p.value,
                    soyb_nav_adf$p.value,
                    weat_nav_adf$p.value,
                    uso_nav_adf$p.value,
                    uga_nav_adf$p.value)
write_csv(nav_df, "~/Documents/etf_tracking/Analysis/ADF/nav.csv")

### Benchmark Returns
corn_asset_adf <- tseries::adf.test(CORN$per_asset_return, k = 20)
soyb_asset_adf <- tseries::adf.test(SOYB$per_asset_return, k = 20)
weat_asset_adf <- tseries::adf.test(WEAT$per_asset_return, k = 20)
uso_asset_adf <- tseries::adf.test(USO$per_asset_return, k = 20)
uga_asset_adf <- tseries::adf.test(UGA$per_asset_return, k = 20)

asset_df <- data.frame(ETF = c("CORN", "SOYB", "WEAT", "USO", "UGA"))
asset_df$Statistic <- c(corn_asset_adf$statistic,
                      soyb_asset_adf$statistic,
                      weat_asset_adf$statistic,
                      uso_asset_adf$statistic,
                      uga_asset_adf$statistic)
asset_df$p_value <- c(corn_asset_adf$p.value,
                    soyb_asset_adf$p.value,
                    weat_asset_adf$p.value,
                    uso_asset_adf$p.value,
                    uga_asset_adf$p.value)
write_csv(asset_df, "~/Documents/etf_tracking/Analysis/ADF/asset.csv")
