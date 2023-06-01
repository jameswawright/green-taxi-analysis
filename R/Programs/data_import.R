########################################################################
# Name: data_import.R                                                  #
# Description: Import Raw Data, Transform, And Format Columns          #
# Creation Date: 01/06/2023                                            #
# Created by: James Wright                                             #
#             Graduate Programmer                                      #
#             Katalyze Data Ltd.                                       #
########################################################################


#------------------------ Do not unintentionally edit below this line ------------------------



### Importing raw data


## Import taxi trip data

# Import data
taxi_trips <- read_csv(file.path(data_path_raw, "taxi_trips.csv"))

# Upper-case names of columns
names(taxi_trips) <- toupper(names(taxi_trips))

# Rename columns for consistent format between tables
names(taxi_trips)[names(taxi_trips) == 'PAYMENTTYPE'] <- 'PAYMENT_TYPE'

# Format VENDORID, RATECODEID, PAYMENTTYPE, TRIP_TYPE columns as character
taxi_trips <- taxi_trips %>% 
  mutate_at(c("VENDORID", "RATECODEID", "PAYMENT_TYPE", "TRIP_TYPE"), 
                                       as.character())

# Format PASSENGER_COUNT as integer
taxi_trips$PASSENGER_COUNT <- as.integer(taxi_trips$PASSENGER_COUNT)

# Format EHAIL_FEE as double - ALL VALUES MISSING
taxi_trips$EHAIL_FEE <- as.double(taxi_trips$EHAIL_FEE)

# Format STORE_AND_FWD_FLAG, RATECODEID, TRIPTYPE as factor
taxi_trips <- taxi_trips %>% 
  mutate_at(c("STORE_AND_FWD_FLAG", "RATECODEID", "TRIP_TYPE"), as.factor)


## Import taxi time and location data and format column types

# Import data
taxi_time_location <- read_csv(file.path(data_path_raw, "taxi_time_location.csv"))

# For completeness, adding LOCATION_DETAILS column to taxi_time_location table as it is missing but in appendix
taxi_time_location <- mutate(taxi_time_location, LOCATION_DETAILS=NA)

# Upper-case names of columns
names(taxi_time_location) <- toupper(names(taxi_time_location))

# Rename columns for consistency between tables and clarity
names(taxi_time_location)[names(taxi_time_location) == 'LOCATION_CODEID'] <- 'LOCATIONID'

# Format TIME column as date
taxi_time_location$TIME <- as.POSIXct(taxi_time_location$TIME)

#Format LOCATION_CODEID as character
taxi_time_location$LOCATIONID <- as.character(taxi_time_location$LOCATIONID)

# Format trip_type as factor
taxi_time_location$TRIP_TYPE <- as.factor(taxi_time_location$TRIP_TYPE)


## Import taxi zone look-up data and format column types

# Import Data
taxi_zone_lookup <- read_csv(file.path(data_path_raw, "taxi_zone_lookup.csv"))

# Upper-case names of columns
names(taxi_zone_lookup) <- toupper(names(taxi_zone_lookup))

# Rename columns for consistency between tables and clarity
names(taxi_zone_lookup)[names(taxi_zone_lookup) == 'ZONE'] <- 'ADMIN_ZONE'

# Format LOCATIONID as character
taxi_zone_lookup$LOCATIONID <- as.character(taxi_zone_lookup$LOCATIONID)