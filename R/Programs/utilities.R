########################################################################
# Name: utilities.R                                                    #
# Description: Program of Utility Functions                            #
# Creation Date: 01/06/2023                                            #
# Created by: James Wright                                             #
#             Graduate Programmer                                      #
#             Katalyze Data Ltd.                                       #
########################################################################

#------------------------ Do not unintentionally edit below this line ------------------------



### Import Utilities


## Function to read in CSVs with upper case variable names
#' @param path String of data path
#' @param file String of filename
#' @return Dataframe of data
read_csv_better <- function(path, file){
  # Read in a CSV
  df <- readr::read_csv(file.path(data_path_raw, file))
  # Make names upper case
  names(df) <- toupper(names(df))
  
  return(df)
}

### Cleaning Utilities

## Function to produce dataframe with duplicates removed
#' @param df Dataframe to remove duplicates from
#' @return Dataframe without duplicates
df_unduplicated <- function(df){
  
  # Remove duplicates
  df_unduplicated <- df[!duplicated(df),]
  
  # Return and explain duplicates removed
  print(paste0("NOTE: New dataframe without duplicates produced for ", deparse(substitute(df)),"."))
  return(df_unduplicated)
}

## Function to produce dataframe containing all duplicates
#' @param df Dataframe to find duplicates from
#' @return Dataframe of duplicates
df_duplicated <- function(df){
  
  # Find duplicates
  return(df[duplicated(df),])
}


## Function to count missing values in a dataframe
#' Identify Duplicate Rows, Output Duplicate and unduplicated datasets.
#' @param df Dataframe to remove duplicates from
#' @param path String as path to folder
#' @param filename String to name csv file
#' @return Dataframe of summed missing values
df_missing_count <- function(df){
  
  # Produce dataframe counting missing values
  df_missing <- map_df(df, function(x) sum(is.na(x)))
  
  # Return dataframe  containing missing values
  return(df_missing)
}



### Plotting Utilities
