########################################################################
# Name: main.R                                                         #
# Description: Main Program To Run Taxi Case Study                     #
# Creation Date: 01/06/2023                                            #
# Created by: James Wright                                             #
#             Graduate Programmer                                      #
#             Katalyze Data Ltd.                                       #
########################################################################

# Path to root folder /Taxi/
path <- "C:\\Users\\james.wright\\OneDrive - Amadeus Software\\Case Studies\\Taxi"





#------------------------ Do not unintentionally edit below this line ------------------------



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



### Set seed for reproducibility
set.seed(2023)



### Process data and generate reports


## Set up environment
source(file.path(programs_path,"autoexec.R"))


## Process data
# Import and format data
source(file.path(programs_path,"data_import.R"))

# Data quality analysis
source(file.path(programs_path,"data_quality.R"))

# Transforming and cleaning data
source(file.path(programs_path,"data_processing.R"))


## Generate reports
