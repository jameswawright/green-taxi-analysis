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

## Function to produce dataframe of duplicates if duplicates exist, else print they don't exist.
#' @param str String of library name
test_library <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}


### Cleaning Utilities


## Function to produce dataframe of duplicates if duplicates exist, else print they don't exist.
#' @param df Dataframe to find duplicates from
#' @return Dataframe of duplicates
df_duplicated <- function(df){
  
  # Find duplicates
  df_duplicates <- df[duplicated(df),]
  
  # If duplicates save them as a dataframe, otherwise there is nothing to save and send message
  if (nrow(df_duplicates) == 0) {
    print(paste("NOTE: There are no duplicates in",deparse(substitute(df)),"- no duplicates dataframe produced."))
  } else{
    print(paste("NOTE: There are",nrow(df_duplicates),"duplicates in", deparse(substitute(df)), "- outputting dataframe containing duplicates."))
    return(df_duplicates)
  }
}


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
