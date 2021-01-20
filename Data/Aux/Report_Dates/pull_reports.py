#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
# Pull Reports

# This script processes the report dates files and produces two csv files: one
# for agricultural reports and one for energy

@author Colburn Hassman
"""
# Import packages
import pandas as pd
import sys


# Pull txt files containing dates and convert to list
sys.path.insert(1, "/home/colburn/Documents/etf_tracking/Data/Aux")
from parse_dates import parse_dates

start_d = "2012-01-01"
end_d = "2020-07-30"

##### Agriculture Reports ####################################################

# create empty dataframe 
ag_data = pd.DataFrame() # create dataframe
ag_data["Date"] = pd.date_range(start= start_d, end = end_d, freq="d") #create dates
ag_data = ag_data.set_index('Date') # set date to index

# Pandas like to convert dates to a timestamp. This prevents:
ag_data.index = ag_data.index.date

# pull dates
wasde = parse_dates("WASDE.txt")
cof = parse_dates("Cattle_on_Feed.txt")
grain = parse_dates("Grain_Stocks.txt")
hogs = parse_dates("Hogs_and_Pigs.txt")
acreage = parse_dates("June_Acreage.txt")
planting = parse_dates("Prosp_Plantings.txt")

ag_data['WASDE'] = 0
for index, row in ag_data.iterrows():
    if index in wasde:
        ag_data['WASDE'].loc[index] = 1

ag_data['COF'] = 0
for index, row in ag_data.iterrows():
    if index in cof:
        ag_data['COF'].loc[index] = 1
        
ag_data['Grain_Stocks'] = 0
for index, row in ag_data.iterrows():
    if index in grain:
        ag_data['Grain_Stocks'].loc[index] = 1
        
ag_data['Hogs_Pigs'] = 0
for index, row in ag_data.iterrows():
    if index in hogs:
        ag_data['Hogs_Pigs'].loc[index] = 1
        
ag_data['Acreage'] = 0
for index, row in ag_data.iterrows():
    if index in acreage:
        ag_data['Acreage'].loc[index] = 1
        
ag_data['Planting'] = 0
for index, row in ag_data.iterrows():
    if index in planting:
        ag_data['Planting'].loc[index] = 1

ag_data.to_csv("Ag_Reports.csv")


### Energy Reports ###########################################################

# create empty dataframe 
en_data = pd.DataFrame() # create dataframe
en_data["Date"] = pd.date_range(start= start_d, end = end_d, freq="d") #create dates
en_data = en_data.set_index('Date') # set date to index

# Pandas like to convert dates to a timestamp. This prevents:
en_data.index = en_data.index.date

an_energy = parse_dates("Annual_Energy.txt")
drilling = parse_dates("Drilling_Productivity.txt")
petro_mon = parse_dates("Petro_Monthly.txt")
steo = parse_dates("STEO.txt")

en_data['Annual_Energy'] = 0
for index, row in en_data.iterrows():
    if index in an_energy:
        en_data['Annual_Energy'].loc[index] = 1
        
en_data['Drilling'] = 0
for index, row in en_data.iterrows():
    if index in drilling:
        en_data['Drilling'].loc[index] = 1

en_data['Petro_Monthly'] = 0
for index, row in en_data.iterrows():
    if index in petro_mon:
        en_data['Petro_Monthly'].loc[index] = 1
        
en_data['STEO'] = 0
for index, row in en_data.iterrows():
    if index in steo:
        en_data['STEO'].loc[index] = 1
        
en_data.to_csv("En_Reports.csv")