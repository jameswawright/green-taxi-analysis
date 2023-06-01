########################################################################
# Name: data_import.R                                                  #
# Description: Import Raw Data, Transform, And Format Columns          #
# Creation Date: 01/06/2023                                            #
# Created by: James Wright                                             #
#             Graduate Programmer                                      #
#             Katalyze Data Ltd.                                       #
########################################################################



### Importing raw data


## Import taxi trip data

# Import data
taxi_trips <- read_csv(file.path(data_path_raw, "taxi_trips.csv"))

# Upper-case names of columns
names(taxi_trips) <- toupper(names(taxi_trips))

# Format VENDORID, RATECODEID, PAYMENTTYPE, TRIP_TYPE columns as character
taxi_trips$VENDORID <- as.character(taxi_trips$VENDORID)
taxi_trips$RATECODEID <- as.character(taxi_trips$RATECODEID)
taxi_trips$PAYMENTTYPE <- as.character(taxi_trips$PAYMENTTYPE)
taxi_trips$TRIP_TYPE <- as.character(taxi_trips$TRIP_TYPE)

# Format PASSENGER_COUNT as integer
taxi_trips$PASSENGER_COUNT <- as.integer(taxi_trips$PASSENGER_COUNT)

# Format STORE_AND_FWD_FLAG, RATECODEID, TRIPTYPE as factor
taxi_trips$STORE_AND_FWD_FLAG <- as.factor(taxi_trips$STORE_AND_FWD_FLAG)
taxi_trips$RATECODEID <- as.factor(taxi_trips$RATECODEID)
taxi_trips$TRIP_TYPE <- as.factor(taxi_trips$TRIP_TYPE)


## Import taxi time and location data and format column types

# Import data
taxi_time_location <- read_csv(file.path(data_path_raw, "taxi_time_location.csv"))

# For completeness, adding LOCATION_DETAILS column to taxi_time_location table as it is missing but in appendix
taxi_time_location <- mutate(taxi_time_location, LOCATION_DETAILS=NA)

# Upper-case names of columns
names(taxi_time_location) <- toupper(names(taxi_time_location))

# Rename columns for consistency between tables
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

# Format LOCATIONID as character
taxi_zone_lookup$LOCATIONID <- as.character(taxi_zone_lookup$LOCATIONID)



### Create taxi_time_location_wide

## Transposing taxi_time_location to taxi_time_location_wide with UNIQUEID as row identifier, 
## with columns for pick-up and drop-off times and locations

# Making each UNIQUEID in taxi_time_location a row identifier
taxi_time_location_wide <- taxi_time_location %>% 
  pivot_wider(id_cols=c('UNIQUEID'),
              names_from = 'TRIP_TYPE',
              values_from = c('LOCATIONID', 'TIME'))

# Upper-case names of columns
names(taxi_time_location_wide) <- toupper(names(taxi_time_location_wide))

# Adding columns for pickup_year, pickup_date, pickup_weekday, pickup_hour
taxi_time_location_wide <- taxi_time_location_wide %>% 
  mutate(PICKUP_YEAR = year(TIME_PICKUP),
         PICKUP_DATE = date(TIME_PICKUP),
         PICKUP_WEEKDAY = wday(TIME_PICKUP, label = TRUE, abbr=FALSE),
         PICKUP_HOUR = hour(TIME_PICKUP))