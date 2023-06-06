########################################################################
# Name: autoexec.R                                                     #
# Description: Set Up Environment for Taxi Case Study                  #
# Creation Date: 01/06/2023                                            #
# Created by: James Wright                                             #
#             Graduate Programmer                                      #
#             Katalyze Data Ltd.                                       #
########################################################################




path <- getwd()



#------------------------ Do not unintentionally edit below this line ------------------------

### Set seed for reproducibility
set.seed(2023)



### Assign paths to file structure


## Paths to data

# Path to raw data
data_path_raw <- file.path(path, "Data","Raw")

# Path to detailed/cleaned data
data_path_clean <- file.path(path, "Data","Clean")


## Paths to programs and reports

# Path to programs
programs_path <- file.path(path, "Programs")

# Path to reports
reports_path <- file.path(path, "Reports")



### Install and import required packages and libraries


## Import support utility functions package
source(file.path(programs_path,"utilities.R"), echo=FALSE)


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
              "roxygen2",
              "purrr",
              "openxlsx",
              "scales")

# Install
for (lib in packages){
  test_library(lib)
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

# Other
library(roxygen2)

## Import support utility functions package
source(file.path(programs_path,"utilities.R"), echo=FALSE)