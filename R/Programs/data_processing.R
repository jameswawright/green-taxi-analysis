########################################################################
# Name: data_processing.R                                              #
# Description: Processing, Transforming and Cleaning Data              #
# Creation Date: 01/06/2023                                            #
# Created by: James Wright                                             #
#             Graduate Programmer                                      #
#             Katalyze Data Ltd.                                       #
########################################################################


#------------------------ Do not unintentionally edit below this line ------------------------


### Data Quality Diagnostics


## Identify Duplicates and Clean from Data

# Find taxi_trip duplicates and create dataframe without duplicates (if duplicates exist) - 67 duplicates
taxi_trips_duplicated <- df_duplicated(taxi_trips)
if (exists("taxi_trips_duplicated")&&is.data.frame(get("taxi_trips_duplicated"))){
  taxi_trips_unduplicated <- df_unduplicated(taxi_trips)
} else{
  print(paste("NOTE: There are no duplicates in",deparse(substitute(taxi_trips)),"- dataframe left unchanged."))
}

# Find taxi_time_location duplicates and remove (if duplicates exist) - 0 duplicates, no need to recreate.
taxi_time_location_duplicated <- df_duplicated(taxi_time_location)
if (exists("taxi_time_location_duplicated")&&is.data.frame(get("taxi_time_location_duplicated"))){
  taxi_time_location_unduplicated <- df_unduplicated(taxi_time_location)
} else{
  print(paste("NOTE: There are no duplicates in",deparse(substitute(taxi_time_location)),"- dataframe left unchanged."))
}

# Find taxi_zone_lookup duplicates and remove - 0 duplicates, no need to recreate.
taxi_zone_lookup_duplicated <- df_duplicated(taxi_zone_lookup)
if (exists("taxi_zone_lookup_duplicated")&&is.data.frame(get("taxi_zone_lookup_duplicated"))){
  taxi_zone_lookup_unduplicated <- df_unduplicated(taxi_zone_lookup)
} else{
  print(paste("NOTE: There are no duplicates in",deparse(substitute(taxi_zone_lookup)),"- dataframe left unchanged."))
}



### Identify missing values and clean from data


## Analyse missing values in taxi_trips_unduplicated 

# Count number of missing values in taxi_trips_unduplicated and output as CSV in reports
taxi_trips_unduplicated_number_missing <- df_missing_count(taxi_trips_unduplicated, 
                                                    path=reports_path, 
                                                    filename="taxi_trips_number_missing")

# Automatically drop all columns containing exclusively missing data as there is no fair inference we can make
taxi_trips_unduplicated <- taxi_trips_unduplicated %>% 
  discard(~all(is.na(.))) %>% 
  keep(~!all(is.na(.)))


## Analyse missing values in taxi_time_location

# Count number of missing values in taxi_time_location and output as CSV in reports
taxi_time_location_number_missing <- df_missing_count(taxi_time_location, 
                                                      path=reports_path, 
                                                      filename="taxi_time_location_number_missing")

# Automatically drop all columns containing exclusively missing data as there is no fair inference we can make
taxi_time_location <- taxi_time_location %>% 
  discard(~all(is.na(.))) %>% 
  keep(~!all(is.na(.)))


## Analyse missing values in taxi_zone_lookup

# Count number of missing values in taxi_zone_lookup and output as CSV in reports
taxi_zone_lookup_number_missing <- df_missing_count(taxi_zone_lookup, 
                                                    path=reports_path, 
                                                    filename="taxi_zone_lookup_number_missing")

# Automatically drop all columns containing exclusively missing data as there is no possible fair inference we can make
taxi_zone_lookup <- taxi_zone_lookup %>% 
  discard(~all(is.na(.))) %>% 
  keep(~!all(is.na(.)))



### Identify extreme values



### Identify bias





### Summary Report of Data Issues



### Cleaning Data



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



### Saving Cleaned Data To Clean Folder
