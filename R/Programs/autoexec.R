########################################################################
# Name: autoexec.R                                                     #
# Description: Set Up Environment for Taxi Case Study                  #
# Creation Date: 01/06/2023                                            #
# Created by: James Wright                                             #
#             Graduate Programmer                                      #
#             Katalyze Data Ltd.                                       #
########################################################################



### Install and import required packages and libraries

## Install any missing packages if missing

# Install ggplot2
if(!require("ggplot2")){
  install.packages("ggplot2")
} 
# Install dplyr
if(!require("dplyr")) {
  install.packages("dplyr")
} 
# Install tibble
if(!require("tibble")) {
  install.packages("tibble")
} 
# Install stringr
if(!require("stringr")) {
  install.packages("stringr")
} 
# Install libridate
if(!require("lubridate")) {
  install.packages("lubridate")
}
# Install readr
if(!require("readr")) {
  install.packages("readr")
}
# Install tidyr
if(!require("tidyr")) {
  install.packages("tidyr")
}

# Import required libraries
library(ggplot2)
library(dplyr)
library(tibble)
library(stringr)
library(lubridate)
library(readr)
library(tidyr)