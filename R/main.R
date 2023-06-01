########################################################################
# Name: main.R                                                         #
# Description: Main Program To Run Taxi Case Study                     #
# Creation Date: 01/06/2023                                            #
# Created by: James Wright                                             #
#             Graduate Programmer                                      #
#             Katalyze Data Ltd.                                       #
########################################################################

# AUTOEXEC.R MUST BE RAN FIRST.

#------------------------ Do not unintentionally edit below this line ------------------------


### Process data and generate reports


## Process data

# Import and format data
source(file.path(programs_path,"data_import.R"), echo=FALSE)

# Transforming and cleaning data
source(file.path(programs_path,"data_processing.R"), echo=FALSE)


## Generate reports
