#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Nov 30 18:57:40 2020

Cleans and process the historical margin data provided by the CME group. 
Written for the ETF Tracking Error Research Project

If I was going to improve this I would rework 'contract_letter' and 
'roll_number' to be collected from the actual data. I would also put logging 
information and error handling. Because I am only going to run this script
once though...

@author: Colburn Hassman, colburn7@vt.edu
"""

import tabula
import sys
import pandas as pd



def read_margin(file_location):
    '''
    This reads in the PDF of the historic margin requirements from the CME  
    group and returns a cleaned pandas data frame with the same format as
    the data in table. Sets "Business Date" Column as date time

    Parameters
    ----------
    file_location : a string containing the location of the file and name 
                    relative to the working directory. For example: 
                    'CORN/C_2009_to_2013.pdf'

    Returns
    -------
    table : A cleaned dataframe containing all the margin data in the original
            formating

    '''
    # Begin by reading in the pdf using the tabula package. There is a single
    # table contained in multiple (many) pages. The pandas_options prevents 
    # the first row of each table being read as the column names
    
    col_names = ['Business Date', 'Description', 'Exchange',
                    'Margin', 'Roll Product Code']
    
    table = tabula.read_pdf(file_location, pages = 'all', multiple_tables= 'False',
                            pandas_options=({'header': None, 
                                             'names': col_names}))
    # Concatenate all the dataframes 
    table = pd.concat(table) 
    # when concatenated, pandas dataframes retain their original index
    # Here I reset the index in order to be able to drop the first row
    table = table.reset_index(drop = True)
    # Drop the first row (which contains column names)
    table = table.drop(table.index[0])

    # Set date column to datetime object and set index. 
    table['Business Date'] = pd.to_datetime(table['Business Date'])
    table = table.set_index('Business Date')
    return table


def format_margin(table, contract_letter, roll_number):
    '''
    This function takes in the table created by read_margin function and 
    reformats it. 

    Parameters
    ----------
    table : the dataframe returned by read_margin
    
    contract_letter = The prefix for the "Roll Product Code". Should be a 
                        string. For example: 'C'
    
    roll_number = is the maximum contract that they list. It is the largest 
                    number in the "Roll Product Code" column
    
    Returns
    -------
    table : the same data in a different format (still dataframe)

    '''
    # Drop unused columns
    table = table.drop(columns = ['Description', 'Exchange'])
    # Pivot the table so that Each Roll Product is a column and there is only
    # one ovservation per day
    table = table.pivot(columns = 'Roll Product Code', values = 'Margin')
    # The columns are not in the correct order. I will rearrange them
    # First create a list of actual column names
    # Some are C-1 and some are C-01
    cols = []
    for i in range(roll_number):
        code = "{}-{}".format(contract_letter, i+1)
        cols.append(code)
        
    cols_1 = []
    for i in range(roll_number):
        num = '{0:0=2d}'.format(i+1)
        code = "{}-{}".format(contract_letter, num)
        cols_1.append(code)
        
    # Create a dictionary where cols and cols_1 are matched up
    cols_dict = {}
    for i in range(len(cols)):
        cols_dict[cols[i]] = cols_1[i]
    # Rename columns using the dictionary just created
    table = table.rename(columns = cols_dict)
    # Order the Columns
    # Below handles the number of columns changing
    len_col = len(table.columns) #  number of columns 

    table = table[cols_1[0:len_col]]
    return table



def process_margin(file_list, contract_letter, roll_number):
    '''
    This function pulls, converts, cleans, concatenates, and formats the data
    contained in the CME Historical Margin PDFs
    

    Parameters
    ----------
    file_list : A list of strings which contain the file location and 
                names of the relevant files
                
    contract_letter : The prefix to the Roll Codes
    
    roll_number : The maximum number of Roll Codes

    Returns
    -------
    table : A dataframe containing all of the clean data

    '''
    table = pd.DataFrame()
    for file in file_list:
        print('Reading: {}'.format(file))
        df = read_margin(file)
        print('Formatting: {}'.format(file))
        df = format_margin(df, contract_letter, roll_number)
        print('Concatenating: {}'.format(file))
        table = pd.concat([table, df])
        print('Finished: {}'.format(file))
    return table

# USO needs its own processor
def uso_format_margin(table, contract_letter, roll_number):
    '''
    This function takes in the table created by read_margin function and 
    reformats it. 
    
    The only difference between this function and the original function is that
    it calls allows for more than 99 contracts listed. 

    Parameters
    ----------
    table : the dataframe returned by read_margin
    
    contract_letter = The prefix for the "Roll Product Code". Should be a 
                        string. For example: 'C'
    
    roll_number = is the maximum contract that they list. It is the largest 
                    number in the "Roll Product Code" column
    
    Returns
    -------
    table : the same data in a different format (still dataframe)

    '''
    # Drop unused columns
    table = table.drop(columns = ['Description', 'Exchange'])
    # Pivot the table so that Each Roll Product is a column and there is only
    # one ovservation per day
    table = table.pivot(columns = 'Roll Product Code', values = 'Margin')
    # The columns are not in the correct order. I will rearrange them
    # First create a list of actual column names
    # Some are C-1 and some are C-01
    cols = []
    for i in range(roll_number):
        code = "{}-{}".format(contract_letter, i+1)
        cols.append(code)
        
    cols_1 = []
    for i in range(roll_number):
        num = '{0:0=3d}'.format(i+1)
        code = "{}-{}".format(contract_letter, num)
        cols_1.append(code)
        
    # Create a dictionary where cols and cols_1 are matched up
    cols_dict = {}
    for i in range(len(cols)):
        cols_dict[cols[i]] = cols_1[i]
    # Rename columns using the dictionary just created
    table = table.rename(columns = cols_dict)
    # Order the Columns
    # Below handles the number of columns changing
    len_col = len(table.columns) #  number of columns 

    table = table[cols_1[0:len_col]]
    return table



def uso_process_margin(file_list, contract_letter, roll_number):
    '''
    This function pulls, converts, cleans, concatenates, and formats the data
    contained in the CME Historical Margin PDFs
    
    The onyl difference between this version and the original version is 
    that is calls the uso_format_margin function
    
    Parameters
    ----------
    file_list : A list of strings which contain the file location and 
                names of the relevant files
                
    contract_letter : The prefix to the Roll Codes
    
    roll_number : The maximum number of Roll Codes

    Returns
    -------
    table : A dataframe containing all of the clean data

    '''
    table = pd.DataFrame()
    for file in file_list:
        print('Reading: {}'.format(file))
        df = read_margin(file)
        print('Formatting: {}'.format(file))
        df = uso_format_margin(df, contract_letter, roll_number)
        print('Concatenating: {}'.format(file))
        table = pd.concat([table, df])
        print('Finished: {}'.format(file))
    return table




def main():
    corn_list = ['CORN/C_2009_to_2013.pdf', 'CORN/C_2014_to_2019.pdf', 
                 'CORN/C_2020_to_present.pdf']
    soyb_list = ['SOYB/S_2009_to_2013.pdf', 'SOYB/S_2014_to_2019.pdf', 
                 'SOYB/S_2020_to_present.pdf']
    weat_list = ['WEAT/W_2009_to_2013.pdf', 'WEAT/W_2014_to_2019.pdf', 
                 'WEAT/W_2020_to_present.pdf']
    uso_list = ['USO/CL_2009_to_2013.pdf', 'USO/CL_2014_to_2019.pdf', 
                'USO/CL_2020_to_present.pdf']
    uga_list = ['UGA/RB_2009_to_2013.pdf', 'UGA/RB_2014_to_2019.pdf', 
                'UGA/RB_2020_to_present.pdf']
    
    print('CORN')
    corn_margin = process_margin(corn_list, 'C', 15)
    corn_margin.to_csv('CORN/corn_margin.csv')
    print('SOYB')
    soyb_margin = process_margin(soyb_list, 'S', 18)
    soyb_margin.to_csv('SOYB/soyb_margin.csv')
    print('WEAT')
    weat_margin = process_margin(weat_list, 'W', 15)
    weat_margin.to_csv('WEAT/weat_margin.csv')
    print('UGA')
    uga_margin = process_margin(uga_list, 'RB', 60)
    uga_margin.to_csv('UGA/uga_margin.csv')
    print('USO')
    uso_margin = uso_process_margin(uso_list, 'CL', 150)
    uso_margin.to_csv('USO/uso_margin.csv')
    
    print('Subsetting Data')
    # Subset the data for the relevant time
    subset_corn_margin = corn_margin['2012-01-01':'2020-08-01']
    subset_soyb_margin = soyb_margin['2012-01-01':'2020-08-01']
    subset_weat_margin = weat_margin['2012-01-01':'2020-08-01']
    subset_uga_margin = uga_margin['2012-01-01':'2020-08-01']
    subset_uso_margin = uso_margin['2012-01-01':'2020-02-01']
    
    # Subset data by columnes
    subset_corn_margin = subset_corn_margin[['C-02', 'C-03']]
    subset_soyb_margin = subset_soyb_margin[['S-02', 'S-03']]
    subset_weat_margin = subset_weat_margin[['W-02', 'W-03']]
    subset_uga_margin = subset_uga_margin[['RB-02', 'RB-03']]
    subset_uso_margin = subset_uso_margin[['CL-002', 'CL-003']]
    
    # Write subsets to csv files
    subset_corn_margin.to_csv('CORN/subset_corn_margin.csv')
    subset_soyb_margin.to_csv('SOYB/subset_soyb_margin.csv')
    subset_weat_margin.to_csv('WEAT/subset_weat_margin.csv')
    subset_uga_margin.to_csv('UGA/subset_uga_margin.csv')
    subset_uso_margin.to_csv('USO/subset_uso_margin.csv')


main()




