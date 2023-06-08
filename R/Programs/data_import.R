########################################################################
# Name: data_import.R                                                  #
# Description: Import Raw Data, Transform, And Format Columns          #
# Creation Date: 01/06/2023                                            #
# Created by: James Wright                                             #
#             Graduate Programmer                                      #
#             Katalyze Data Ltd.                                       #
########################################################################

### Importing raw data - formatting manually with dplyr rather than col_types in readr for safety of name references

## Import and format taxi trip data

# Import data with upper case variable names
taxi_trips <- read_csv_better(data_path_raw, "taxi_trips.csv")

# Rename columns for consistent format between tables
names(taxi_trips)[names(taxi_trips) == 'PAYMENTTYPE'] <- 'PAYMENT_TYPE'

# Format types
# - Reformat VENDORID, RATECODEID, PAYMENTTYPE, TRIP_TYPE columns as character
# - Reformat PASSENGER_COUNT as integer
# - Reformat EHAIL_FEE as double - supposed to be $ amount - ALL VALUES MISSING
# - Reformat STORE_AND_FWD_FLAG as factor.
# - Keeping character columns as they are for cleaning phase at this stage, to identify NAs vs. alternative values
taxi_trips <- taxi_trips %>% 
  mutate_at(c("VENDORID", "RATECODEID", "PAYMENT_TYPE", "TRIP_TYPE"), 
                                       as.character) %>% 
  mutate_at(c("PASSENGER_COUNT"), as.integer) %>% 
  mutate_at(c("EHAIL_FEE"), as.double) %>% 
  mutate(STORE_AND_FWD_FLAG = factor(STORE_AND_FWD_FLAG,
                                     levels=c('Y','N'),
                                     labels=c('Store and Forward Trip', 'Not a Store and Forward Trip')))

## Import taxi time and location data and format column types

# Import data with upper case variable names
taxi_time_location <- read_csv_better(data_path_raw, "taxi_time_location.csv")

# Rename columns for consistency between tables and clarity
names(taxi_time_location)[names(taxi_time_location) == 'LOCATION_CODEID'] <- 'LOCATIONID'

# Format types
# - For completeness, adding LOCATION_DETAILS column to taxi_time_location table as it is missing but in appendix
# - Reformat LOCATIONID as character, the former a description the latter a category
# - Reformat TIME as a datetime
# - Reformat LOCATION_DETAILS as logical as it is a flag
taxi_time_location <- taxi_time_location %>% 
  mutate(LOCATION_DETAILS=NA) %>% 
  mutate_at(c("LOCATIONID"), as.character) %>% 
  mutate_at(c("TIME"), as.POSIXct) %>% 
  mutate_at(c("LOCATION_DETAILS"), as.logical)


## Import taxi zone look-up data and format column types

# Import data with upper case variable names
taxi_zone_lookup <- read_csv_better(data_path_raw, "taxi_zone_lookup.csv")

# Format LOCATIONID as character
taxi_zone_lookup <- taxi_zone_lookup %>% 
  mutate_at(c("LOCATIONID"), as.character)

