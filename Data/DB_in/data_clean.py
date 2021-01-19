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
    

def main():
    # CORN
    print("Importing CORN...")
    CORN_NAV, CORN_price = import_data("CORN") # call import and unpack list
    print("Cleaning CORN...")
    CORN_NAV = clean_data(CORN_NAV)
    CORN_price = clean_data(CORN_price)
    print("Combining and Subsetting CORN...")
    CORN_df = combine_subset_data(CORN_NAV, CORN_price, "2012-01-04", "2020-07-31")
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
    print("Writing UGA to CSV")
    UGA_df.to_csv("UGA_in.csv", index_label = "Date")


if __name__ == "__main__":
    main()