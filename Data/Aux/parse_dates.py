#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jan 19 10:49:26 2021

# Function to read in Text files of dates and returns a list containing 
# those dates in the proper format

@author: Colburn Hassmab
"""

import pandas as pd

def parse_dates(filename):
    # Open file
    df = pd.read_csv(filename, sep = "\n", header = None) 
    # convert to datetime
    df[0] = pd.to_datetime(df[0], format = "%Y-%m-%d")
     # Drop the timestamp, use only the date
    df[0] = df[0].dt.date 
    # Convert to list
    roll_dates = df[0].tolist()
    return(roll_dates)

    
