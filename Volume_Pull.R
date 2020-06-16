#####
# Rather than pull volume data every time I run one of the analysis files, 
# I will pull all the data here, clean and organize it, then export it to a CSV
# which can then be pulled into the other files

library(quantmod)
library(xts)

rm(list = ls())
#Assign arguments
start_date <- "2012-01-01"
end_date <- '2020-02-10'
symbols <- c("CORN", 'SOYB', 'WEAT', 'UGA', 'USO')

#Pull Data from Yahoo Finance
quantmod::getSymbols(Symbols = symbols, 
                     src = "yahoo", 
                     index.class = "POSIXct",
                     from = start_date, 
                     to = end_date, 
                     adjust = FALSE)

# Merge the relevant columns to form a larger xts object
volume = merge(CORN$CORN.Volume, SOYB$SOYB.Volume, WEAT$WEAT.Volume, USO$USO.Volume, UGA$UGA.Volume)

#convert xts object to data frame
volume_df = data.frame(DATE = as.Date(index(volume)), volume)

#Export the data to a csv file
write.csv(volume_df, "G:/My Drive/3_Massa Research/Neff Paper/Working_Folder/Volume.csv")
