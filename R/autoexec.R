########################################################################
# Name: autoexec.R                                                     #
# Description: Set Up Environment for Taxi Case Study                  #
# Creation Date: 01/06/2023                                            #
# Created by: James Wright                                             #
#             Graduate Programmer                                      #
#             Katalyze Data Ltd.                                       #
########################################################################


#------------------------ Do not unintentionally edit below this line ------------------------

### Set seed for reproducibility
set.seed(2023)



### Assign paths to file structure


## Paths

# Path to raw data
data_path_raw <- file.path(path, "R", "Data","Raw")

# Path to detailed/cleaned data
data_path_clean <- file.path(path, "R", "Data","Clean")

# Path to reports
reports_path <- file.path(path, "R", "Reports")



### Install and import required packages and libraries


## Installs any packages if missing, otherwise setdiff is false and does nothing 
# Packages
packages <- c("rmarkdown", 
              "knitr", 
              "ggplot2",
              "dplyr",
              "tibble",
              "stringr",
              "lubridate",
              "readr",
              "tidyr",
              "purrr",
              "openxlsx",
              "scales",
              "roxygen2")

# Install
for (package in packages){
  if (!require(package,character.only = TRUE))
  {
    install.packages(package,dep=TRUE, quiet=TRUE)
    if(!require(package,character.only = TRUE)) {
      stop("Package not found")
    }
  }
}



## Import required libraries

# Plotting
library(ggplot2)
library(scales)

# Data Manipulation
library(dplyr)
library(tibble)
library(stringr)
library(lubridate)
library(tidyr)
library(purrr)

# Import/Export
library(readr)
library(openxlsx)

#Other
library(roxygen2)

## Import support utility functions package
source(file.path(programs_path,"utilities.R"), echo=FALSE)