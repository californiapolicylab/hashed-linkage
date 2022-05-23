#!/usr/bin/env python
# coding: utf-8

## script: hashing_base.ipynb 
## author: california policy lab 
## purpose: this is a general-purpose hashing script that will hash the 
##          following identifying variables (and various permutations of those variables)
##          in the source data. This script should be modified for each dataset it is run on.
##          1. SSN
##          2. DOB
##             - Day
##             - Month
##             - Year
##          3. Name
##             - First
##             - Last
##      Some commonly needed modifications include:
##      1. Changing the salt to whatever is applicable for the project
##      2. Changing the latest year to the year corresponding to the latest year of data 
##      3. Changing the variable names to correspond to the raw data 
##      4. Modifying as necessary to hash additional PII, or PII across multiple files 


## import packages 

import pandas as pd
import numpy as np
import datetime
import hashlib
from abydos.phonetic import Soundex
import re
from pathlib import Path
import configparser
from sys import argv

import pdb


def linecount(fpath):
    i = -1  # For if it's an empty file
    with open(fpath) as f:
        for i, _ in enumerate(f):
            pass
    return i + 1


def clean_and_preprocess_data(
    input_filepath,
    ssn,
    first_name,
    last_name,
    dob,
    latest_year,
    date_format=None,
    date_errors='raise',
    check_linecount=True,
    **kwargs
):

    # Standardize path for filesystem this is running on, and resolve relative paths (keeping as str)
    input_filepath = str(Path(input_filepath).resolve())

    # import raw data, using strings for all columns except DOB, which is a date
    raw = pd.read_csv(
        input_filepath,
        dtype=str,
        parse_dates=[dob] if dob and not date_format else [],
        # Don't want anything to be read as NaN so that the substrings etc will be well defined;
        # we will replace all empty strings with NaN as we gp
        keep_default_na=False,
        **kwargs
    )
    print('CSV imported')
    # Check the linecount if we're supposed to and if we're not using a sample of the file
    if check_linecount:
        if 'nrows' in kwargs:
            print('Using sample of CSV, skipping linecount')
        else:
            print('Checking linecount...')
            df_linecount = len(raw)
            sys_linecount = linecount(input_filepath)
            if sys_linecount != df_linecount + 1:  # Plus one for the header row
                raise ValueError('Issue reading csv; pandas sees {} rows, but system linecount utility sees {}. Try '
                                 'checking quoting, newline characters, encoding.'.format(
                    df_linecount, sys_linecount
                ))
            else:
                print('Pandas linecount matches raw file linecount.')

    # Read in the table of special characters we want to convert
    special_characters = pd.read_csv(
        str(Path(__file__).parent.joinpath('Unicode Values Sheet - unicode_short (1).csv')),
        dtype=str
    )
    # Clean up the column names a little
    special_characters = special_characters.rename(columns={x: x.strip() for x in special_characters.columns})
    # Convert to a dictionary
    special_characters_dict = special_characters.set_index('Symbol')['To Value'].to_dict()

    # Need to parse datetimes after reading in the CSV if a. we're using a custom date format (this is
    # the way pandas recommends you do it in their read_csv documentation); or b. we're using the default
    # date format but pandas didn't read it in as a date, which happens if any date in the column raises an error
    # (usually because it is outside the years 1678-2261)
    if dob and (date_format or not pd.api.types.is_datetime64_any_dtype(raw[dob])):
        # Need to make date_format None if it wasn't passed in (which would be an empty string by default)
        raw[dob] = pd.to_datetime(raw[dob], format=None if not date_format else date_format, errors=date_errors)

    if ssn:
        ## clean SSNs
        ## remove all non-digit characters 
        raw['ssn'] = raw[ssn].str.replace(r'[^0-9]', '', regex=True)
        # Pad SSNs to 9 characters
        raw['ssn'] = raw['ssn'].str.pad(width=9, side='left', fillchar='0')
        ## tag invalid SSNs    
        raw['badSSN'] = (
            (raw['ssn'] == '') |
            (raw['ssn'].str.len() > 9) |
            (raw['ssn'].str[0:3].isin(["000", "666"])) |
            (raw['ssn'].str[3:5]=="00") |
            (raw['ssn'].str[5:9]=="0000") |
            (raw['ssn'].str.startswith('9')) |
            (raw['ssn'].isin(["078051120", "123456789"]))
        )
        # Make SSN missing where badSSN is true
        raw['ssn'] = raw['ssn'].mask(raw['badSSN'])

        ## create ssn substrings to hash
        # Do this with a loop over the first of the two digits to hold out
        for i in range(1, 9):
            # ie `raw['ssn56'] = raw['ssn'].str[0:4] + raw['ssn'].str[6:9]
            # Only want to do this where badSSN is not true
            # This implicitly sets NaN where the condition in loc isn't met
            raw.loc[~raw['badSSN'], 'ssn' + str(i) + str(i+1)] = raw['ssn'].str[0:i-1] + raw['ssn'].str[i+1:9]

        
    ## clean names


    pe = Soundex()
    if first_name:
        ## remove common prefixes/suffixes from first & last name, making sure to escape the periods
        # so we can use them in a regex
        fn_prefix_titles = ['mr', 'mrs', 'dr', 'ms']
        fn_prefix_formats = ['{}\\ ', '{}\\.', '{}$']
        # Take cartesian product of the title and possible formats for them and make them a single "or" regex
        fn_prefixes = '|'.join([prefix_format.format(title) for prefix_format in
                       fn_prefix_formats for title in fn_prefix_titles])
        raw['fn'] = (raw[first_name]
                     # str.maketrans makes a translation table from a dictionary; it converts the special characters
                     # to numeric codes, which it can then evidently parse (it doesn't work without the call to maketrans)
                     .str.translate(str.maketrans(special_characters_dict))  # Convert to 26-character alphabet characters
                     .str.lower()  # Make lowercase
                     .str.strip()  # Strip whitespace at ends of string
                     # Replace prefixes at beginning of string
                     .str.replace(r'^({})+'.format(fn_prefixes), '', regex=True)
                     # Remove all non-alphabetical characters
                     .str.replace(r'[^a-z]', '', regex=True))
        # ie the regex this gives will be '^(mr|mrs|dr\.|...|ms\.)', meaning match any of those at the start
        # of the string, and analogously at the end of the string for ln
        # Fill both with nan where empty
        raw['fn'] = raw['fn'].mask(raw['fn'] == '')
        # Tag bad names,
        raw['badFN'] = raw['fn'].isnull()
        ## create shortened and soundex versions of names, for non bad names
        raw.loc[~raw['badFN'], 'fn2l'] = raw.loc[~raw['badFN'], 'fn'].str[0:2]
        raw.loc[~raw['badFN'], 'fn4l'] = raw.loc[~raw['badFN'], 'fn'].str[0:4]
        raw.loc[~raw['badFN'], 'fn_sdx'] = raw.loc[~raw['badFN'], 'fn'].apply(pe.encode)


    if last_name:
        ln_suffixes = '|'.join([re.escape(x) for x in [' jr',' sr', ' jr.', ' sr.']])
        raw['ln'] = (raw[last_name]
                     # str.maketrans makes a translation table from a dictionary; it converts the special characters
                     # to numeric codes, which it can then evidently parse (it doesn't work without the call to maketrans)
                     .str.translate(str.maketrans(special_characters_dict))  # Convert to 26-character alphabet characters
                     .str.lower()
                     .str.strip()
                     # Replace suffixes at end of string
                     .str.replace(r'({})+$'.format(ln_suffixes), '', regex=True)
                     .str.replace(r'[^a-z]', '', regex=True))
        raw['ln'] = raw['ln'].mask(raw['ln'] == '')
        raw['badLN'] = raw['ln'].isnull()
        raw.loc[~raw['badLN'], 'ln2l'] = raw.loc[~raw['badLN'], 'ln'].str[0:2]
        raw.loc[~raw['badLN'], 'ln4l'] = raw.loc[~raw['badLN'], 'ln'].str[0:4]
        raw.loc[~raw['badLN'], 'ln_sdx'] = raw.loc[~raw['badLN'], 'ln'].apply(pe.encode)

## clean DOB
    ## extract day, month & year
    # Missing values are populated with pd.NaT (Not a Time, pandas' representation of missing dates)
    # when parsing the csv, even if keep_default_na is set to False (which makes sense, not sure what
    # else they could be)
    # NaT.year gives NaN, so this propagates missing values appropriately
    if dob:
        raw['dob_y'] = raw[dob].dt.year
        raw['dob_m'] = raw[dob].dt.month
        raw['dob_d'] = raw[dob].dt.day
        ## tag invalid dates
        raw['badDOB'] = (
            (pd.isnull(raw[dob])) |
            (raw['dob_y'] < 1900) |
            (raw['dob_y'] > latest_year)
        )
        ## convert to string for hashing to work (where we haven't flagged it as bad)
        raw.loc[~raw['badDOB'], 'dob_y'] = raw.loc[~raw['badDOB'], 'dob_y'].apply(lambda x: '{:.0f}'.format(x))
        raw.loc[~raw['badDOB'], 'dob_m'] = raw.loc[~raw['badDOB'], 'dob_m'].apply(lambda x: '{:.0f}'.format(x))
        raw.loc[~raw['badDOB'], 'dob_d'] = raw.loc[~raw['badDOB'], 'dob_d'].apply(lambda x: '{:.0f}'.format(x))

    return raw


## main function to import, clean, and hash data 
def hash_data(salt, 
              input_filepath, 
              output_filepath,  
              ssn, 
              first_name, 
              last_name, 
              dob,
              latest_year,
              drop_unhashed_pii=True,
              date_errors='raise',
              check_linecount=True,
             **kwargs):
    """ Hash PII (SSN, first name, last name, and date of birthday) and derivatives
        from an input CSV file. 
        
        Parameters 
        ----------
        
        salt: string 
            the agreed-upon salt  
        input_filepath: string 
            the local filepath of the input csv 
        output_filepath: string
            the local filepath of the output csv 
        ssn: string
            the name of the ssn variable in the input csv 
        first_name: string 
            the name of the first name variable in the input csv 
        last_name: string 
            the name of the last name variable in the input csv 
        dob: string
            the name of the birthdate variable in the input csv
        latest_year: integer 
            the latest plausible birthyear in the input data
        kwargs: dict
            Keyword arguments that are passed to clean_and_preprocess_data
       
        Returns
        -------

        
        hashed data: a csv file with all programmatic data plus hashed PII 
    """

    raw = clean_and_preprocess_data(
        input_filepath=input_filepath,
        ssn=ssn,
        first_name=first_name,
        last_name=last_name,
        dob=dob,
        latest_year=latest_year,
        date_errors=date_errors,
        check_linecount=check_linecount,
        **kwargs
    )
    
    ## drop intermediate & raw vars, do some renaming    
    vars_to_hash = (
        (['ssn','ssn89','ssn78','ssn67','ssn56','ssn45',
         'ssn34','ssn23','ssn12'] if ssn else []) +
        (['fn', 'fn2l', 'fn4l','fn_sdx'] if first_name else []) +
        (['ln', 'ln2l', 'ln4l','ln_sdx'] if last_name else []) +
        (['dob_d', 'dob_m', 'dob_y'] if dob else [])
)
    clean = raw[vars_to_hash + [x for x in raw.columns if x not in vars_to_hash and 
                                x not in ([ssn, first_name, last_name, dob] if drop_unhashed_pii else [])]]
    

## hash SSN and derivatives, first and last name and derivatives, and DOB and derivatives
#     pdb.set_trace()
    for col in vars_to_hash:
#         print(col)
        # Get which base field this is a part of, so we can not hash fields where it's flagged as bad
        this_base_col = re.search(r'^([a-z]+)', col).group(1)
        this_bad_flag = 'bad' + this_base_col.upper()
        clean.loc[~clean[this_bad_flag], col + '_hash'] = clean.loc[~clean[this_bad_flag], col].apply(
            lambda s: hashlib.sha256(str.encode(salt+s)).hexdigest().upper()
        )

## drop unhashed PII and export
    if drop_unhashed_pii:
        clean = clean.drop(columns=vars_to_hash)
    clean.to_csv(output_filepath, sep=",", index=False)  
    
    return clean

# Read in the config file
# Get config file path from commandline if passed, otherwise use whatever's in the same directory as this script
config_path = Path(argv[1]) if len(argv) > 1 else Path(__file__).parent.joinpath('hashing_config.ini')
config_path = str(config_path.resolve())
# Print it out so there's no ambiguity
print('Reading config file from {}'.format(config_path))
config = configparser.ConfigParser()
config.read(config_path)

# Pull out the additional pandas arguments config section so we can clean it a bit and make it a dict
config_additional_pandas_args = dict(config['Additional pandas arguments'])
if 'nrows' in config_additional_pandas_args:
    config_additional_pandas_args['nrows'] = config['Additional pandas arguments'].getint('nrows')

hashed = hash_data(
    # Perhaps a little ugly to have all of these just be values from a dictionary, but this is so that the config file
    # can have sections and nice human-readable parameter names
    salt=config['Salt']['salt'],
    input_filepath=config['Filepaths']['Input'],
    output_filepath=config['Filepaths']['Output'],
    ssn=config['Column names']['SSN'],
    first_name=config['Column names']['First name'],
    last_name=config['Column names']['Last name'],
    dob=config['Column names']['Date of birth'],
    latest_year=config['Parameters'].getint('Max DOB year'),
    sep=config['Parameters']['CSV delimiter'],
    drop_unhashed_pii=config['Parameters'].getboolean('Drop unhashed PII?'),
    date_errors=config['Parameters']['Date errors'],
    date_format=config['Parameters']['Date format'],
    check_linecount=config['Parameters'].getboolean('Check linecount?'),
    **config_additional_pandas_args
)
