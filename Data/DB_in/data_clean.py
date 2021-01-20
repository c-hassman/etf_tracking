#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan 14 10:56:40 2021

This program cleans and validates the data contained in Price_NAV_DB.xlsx 
which was pull directly from TR Eikon with minimum manipulation to ensure 
data intregrity.

@author: Colburn Hassman
@contact: colburn7@vt.edu
"""
import pandas as pd
import os
#os.chdir("home/colburn/Documents/etf_tracking/Price_NAV_Full_DB") #set wkdir

# Need to import parse data from other folder
import sys
sys.path.insert(1, "/home/colburn/Documents/etf_tracking/Data/Aux")
from parse_dates import parse_dates

roll_path = "/home/colburn/Documents/etf_tracking/Data/Aux/Roll_Dates/"
ohlv_path = "/home/colburn/Documents/etf_tracking/Data/Aux/OHLV/"

def import_data(ETF_ticker):
    # define sheet names
    NAV_sheet = "{0}_NAV".format(ETF_ticker)
    price_sheet = "{0}_Price".format(ETF_ticker)
    # import data using read_excel
    NAV_data = pd.read_excel("Price_NAV_DB.xlsx", sheet_name = NAV_sheet)
    price_data = pd.read_excel("Price_NAV_DB.xlsx", sheet_name = price_sheet)
    
    return NAV_data, price_data
    

def clean_data(data_df):
    # Convert date column to date-time
    data_df['Date'] = pd.to_datetime(data_df['Date'], format = '%d-%b-%Y' )
    # Drop any erroneous columns from TR Eikon formatting
    data_df = data_df[data_df.columns[0:2]]
    # Set Date as index
    data_df = data_df.set_index('Date')
    return data_df
    

def combine_subset_data(NAV_df, price_df, start, end):
    # combine on index
    data_df = NAV_df
    data_df['Price'] = price_df['Price']
    # check for NAN
    num_nan = data_df.isnull().sum()
    print("DataFrame had these NaNs: ")
    print(num_nan)
    # Subset Data
    data_df = data_df.loc[start:end]
    return data_df
    
def add_rolls(data_df, roll_file):
    # Call the parse_dates function, which returns a list of the dates found 
    # in roll_file
    roll_dates = parse_dates(roll_file)
    # Default value for the Roll is 0
    data_df['Roll'] = 0
    # Pandas like to convert dates to a timestamp. This prevents:
    data_df.index = data_df.index.date
    # Loop through each row of the dataframe and change "Roll" dummy to 1
    # is the data is found in the roll_list
    for index, row in data_df.iterrows():
        if index in roll_dates:
            data_df['Roll'].loc[index] = 1
    return data_df

def pull_ohlc(data_df, ohlv_file):
    # Pull open, high, low, close, volume, file and combine with data
    ohlv_df = pd.read_csv(ohlv_path + ohlv_file)
    # Convert date from character to datetime
    ohlv_df['Date'] = pd.to_datetime(ohlv_df['Date'])
    #Set date as index
    ohlv_df = ohlv_df.set_index("Date")
    # Keep only date from timestamp
    ohlv_df.index = ohlv_df.index.date
    # Remove umnecessary Close and Turnover columns
    ohlv_df = ohlv_df.drop(['Close', 'Turnover'], axis = 1)
    data_df = pd.concat([data_df, ohlv_df], axis = 1, join = "inner")
    return(data_df)
    

def main():
    # CORN
    print("Importing CORN...")
    CORN_NAV, CORN_price = import_data("CORN") # call import and unpack list
    print("Cleaning CORN...")
    CORN_NAV = clean_data(CORN_NAV)
    CORN_price = clean_data(CORN_price)
    print("Combining and Subsetting CORN...")
    CORN_df = combine_subset_data(CORN_NAV, CORN_price, "2012-01-04", "2020-07-31")
    print("Adding Roll Dates to CORN")
    roll_file = roll_path + "CORN_Roll_Dates.txt"
    CORN_df = add_rolls(CORN_df, roll_file)
    print("Adding CORN OHLV Data")
    CORN_df = pull_ohlc(CORN_df, "CORN_OHLV.csv")
    print("Writing CORN to CSV")
    CORN_df.to_csv("CORN_in.csv", index_label = "Date")
    
   
    # SOYB
    print("Importing SOYB...")
    SOYB_NAV, SOYB_price = import_data("SOYB") # call import and unpack list
    print("Cleaning SOYB...")
    SOYB_NAV = clean_data(SOYB_NAV)
    SOYB_price = clean_data(SOYB_price)
    print("Combining and Subsetting SOYB...")
    SOYB_df = combine_subset_data(SOYB_NAV, SOYB_price, "2012-01-04", "2020-07-31")
    print("Adding Roll Dates to SOYB")
    roll_file = roll_path + "SOYB_Roll_Dates.txt"
    SOYB_df = add_rolls(SOYB_df, roll_file)
    print("Adding OHLV Data")
    SOYB_df = pull_ohlc(SOYB_df, "SOYB_OHLV.csv")
    print("Writing SOYB to CSV")
    SOYB_df.to_csv("SOYB_in.csv", index_label = "Date")
    
    #WEAT
    print("Importing WEAT...")
    WEAT_NAV, WEAT_price = import_data("WEAT") # call import and unpack list
    print("Cleaning WEAT...")
    WEAT_NAV = clean_data(WEAT_NAV)
    WEAT_price = clean_data(WEAT_price)
    print("Combining and Subsetting WEAT...")
    WEAT_df = combine_subset_data(WEAT_NAV, WEAT_price, "2012-01-04", "2020-07-31")
    print("Adding Roll Dates to WEAT")
    roll_file = roll_path + "WEAT_Roll_Dates.txt"
    WEAT_df = add_rolls(WEAT_df, roll_file)
    print("Adding WEAT OHLV Data")
    WEAT_df = pull_ohlc(WEAT_df, "WEAT_OHLV.csv")
    print("Writing WEAT to CSV")
    WEAT_df.to_csv("WEAT_in.csv", index_label = "Date")
    
    #USO
    print("Importing USO...")
    USO_NAV, USO_price = import_data("USO") # call import and unpack list
    print("Cleaning USO...")
    USO_NAV = clean_data(USO_NAV)
    USO_price = clean_data(USO_price)
    print("Combining and Subsetting USO...")
    USO_df = combine_subset_data(USO_NAV, USO_price, "2013-07-15", "2020-01-30")
    #print("Adding Roll Dates to USO")
    #roll_file = roll_path + "USO_Roll_Dates.txt"
    #USO_df = add_rolls(USO_df, roll_file)
    print("Adding USO OHLV Data")
    USO_df = pull_ohlc(USO_df, "USO_OHLV.csv")
    print("Writing USO to CSV")
    USO_df.to_csv("USO_in.csv", index_label = "Date")
    
    #UGA
    print("Importing UGA...")
    UGA_NAV, UGA_price = import_data("UGA") # call import and unpack list
    print("Cleaning UGA...")
    UGA_NAV = clean_data(UGA_NAV)
    UGA_price = clean_data(UGA_price)
    print("Combining and Subsetting UGA...")
    UGA_df = combine_subset_data(UGA_NAV, UGA_price, "2012-01-04", "2020-07-31")
    #print("Adding Roll Dates to UGA")
    #roll_file = roll_path + "UGA_Roll_Dates.txt"
    #UGA_df = add_rolls(UGA_df, roll_file)
    print("Adding UGA OHLV Data")
    UGA_df = pull_ohlc(UGA_df, "UGA_OHLV.csv")
    print("Writing UGA to CSV")
    UGA_df.to_csv("UGA_in.csv", index_label = "Date")


if __name__ == "__main__":
    main()