########################################################################
# Name: autoexec.R                                                     #
# Description: Set Up Environment for Taxi Case Study                  #
# Creation Date: 01/06/2023                                            #
# Created by: James Wright                                             #
#             Graduate Programmer                                      #
#             Katalyze Data Ltd.                                       #
########################################################################

# Path to root folder /Taxi/
path <- "C:\\Users\\james.wright\\OneDrive - Amadeus Software\\Case Studies\\Taxi"





#------------------------ Do not unintentionally edit below this line ------------------------

### Set seed for reproducibility
set.seed(2023)

### Assign paths to file structure


## Paths to data

# Path to raw data
data_path_raw <- file.path(path, "R", "Data","Raw")
# Path to detailed/cleaned data
data_path_detail <- file.path(path, "R", "Data","Detailed")


## Paths to programs and reports

# Path to programs
programs_path <- file.path(path, "R", "Programs")
# Path to reports
reports_path <- file.path(path, "R", "Reports")



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
# Install roxygen2
if(!require("roxygen2")){
  install.packages("roxygen2")
}
# Install purrr
if(!require("purrr")){
  install.packages("purrr")
}

# Import required libraries
library(ggplot2)
library(dplyr)
library(tibble)
library(stringr)
library(lubridate)
library(readr)
library(tidyr)
library(roxygen2)
library(purrr)

# Import support utility functions
source(file.path(programs_path,"utilities.R"))