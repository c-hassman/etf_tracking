rm(list=ls())
library(readxl)
library(tidyverse)
library(ggthemes)

data <- read_excel("G:/My Drive/3_Massa Undergrad Research/Neff Paper/Thesis new return.xlsx", 
              col_types = c("numeric", "numeric", "numeric", 
                            "numeric", "numeric", "numeric", 
                            "skip", "skip", "skip","skip", 
                            "skip", "skip","skip", 
                            "skip", "skip","skip", 
                            "skip", "skip","skip", 
                            "skip", "skip","skip", 
                            "skip", "skip","skip", 
                            "skip", "skip","skip" ))

