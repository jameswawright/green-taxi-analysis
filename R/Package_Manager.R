########################################################################
# Name: utilities.R                                                    #
# Description: Program of Utility Functions                            #
# Creation Date: 09/06/2023                                            #
# Created by: James Wright                                             #
#             Graduate Programmer                                      #
#             Katalyze Data Ltd.                                       #
########################################################################

#------------------------ RUN THIS FIRST ------------------------

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