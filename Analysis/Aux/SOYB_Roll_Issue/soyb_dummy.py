#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jan 20 12:09:22 2021

# This script produces a single and unique dummy set for the soyb issue dates
# in support of SOYB_Roll_Scratchpad.R file. Produces a CSV

# currently there are 14 issue days.. will need to change how unique works 
# if we add/delete more

@author: Colburn Hassman
@contact: colburn7@vt.edu
"""

# Import packages
import pandas as pd
#%%

# Read in issue days.. similiar to parse_dates.py
days = pd.read_csv("SOYB_Issue_Days.txt", sep = "\n", header = None)
days[0] = pd.to_datetime(days[0], format = "%Y-%m-%d")
days[0] = days[0].dt.date
days = days[0].tolist()


issue_num = 14 # number of issue days

start_d = "2012-01-04"
end_d = "2020-07-10"

data = pd.DataFrame() # create dataframe
data["Date"] = pd.date_range(start= start_d, end = end_d, freq="d") #create dates
data = data.set_index('Date') # set date to index

# Pandas like to convert dates to a timestamp. This prevents:
data.index = data.index.date

# Single Dummy
data['Single'] = 0
for index, row in data.iterrows():
    if index in days:
        data['Single'].loc[index] = 1


for i in range(0,issue_num): # loop through once per issue day
    data["u{}".format(i)] = 0 # create a unique column for issue day
    for index, row in data.iterrows(): 
        if index == days[i]:
            data["u{}".format(i)].loc[index] = 1
            
data.to_csv("soyb_dummies.csv")

