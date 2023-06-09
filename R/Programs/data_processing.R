########################################################################
# Name: data_processing.R                                              #
# Description: Processing, Transforming and Cleaning Data              #
# Creation Date: 01/06/2023                                            #
# Created by: James Wright                                             #
#             Graduate Programmer                                      #
#             Katalyze Data Ltd.                                       #
########################################################################







#------------------------ Do not unintentionally edit below this line ------------------------



### Identify Duplicates and Clean from Data


## taxi_trips duplicates

# Find taxi_trips duplicates
taxi_trips_duplicates <- df_duplicated(taxi_trips)

# Create dataframe without duplicates (if duplicates exist) - 67 duplicates
if (nrow(taxi_trips_duplicates) > 0){
  taxi_trips_unduplicated <- df_unduplicated(taxi_trips)
} else{
  print(paste("There were no duplicates in",deparse(substitute(taxi_trips)),"- dataframe left unchanged."))
}


## taxi_time_location duplicates

# Find taxi_time_location duplicates
taxi_time_location_duplicates <- df_duplicated(taxi_time_location)

# Create dataframe without duplicates (if duplicates exist), 0  duplicates no need to recreate.
if (nrow(taxi_time_location_duplicates) > 0){
  taxi_time_location_unduplicated <- df_unduplicated(taxi_time_location)
} else{
  print(paste("There were no duplicates in",deparse(substitute(taxi_time_location)),"- dataframe left unchanged."))
}


## taxi_zone_lookup duplicates

# Find taxi_zone_lookup duplicates
taxi_zone_lookup_duplicates <- df_duplicated(taxi_zone_lookup) 

# Create dataframe without duplicates (if duplicates exist), 0 duplicates no need to unduplicate.
if (nrow(taxi_zone_lookup_duplicates) > 0){
  taxi_zone_lookup_unduplicated <- df_unduplicated(taxi_zone_lookup)
} else{
  print(paste("There were no duplicates in",deparse(substitute(taxi_zone_lookup)),"- dataframe left unchanged."))
}


## Save duplicated data rows for review

# taxi_trips: Missing values all occur simultaneously on a row if ehail_fee dropped - always trip_type=3, Newark
# taxi_time_location: No missing rows if location_details dropped
# taxi_zone_lookup: locationid=264,265 have no known details
write.xlsx(list("Taxi_Trips" = taxi_trips_duplicates, 
                "Taxi_Time_Location"= taxi_time_location_duplicates, 
                "Taxi_Zone_Lookup" = taxi_zone_lookup_duplicates), 
           file=file.path(reports_path, "duplicate_data.xlsx"))


### Handling missing values


## Taxi_Zone_Lookup missing values

# Make missing datatypes consistent in taxi_zone_lookup so that they can be found by R more easily
# - Assuming NV is a typo for NA by context of NA in row and full text in the remainder of the column.
taxi_zone_lookup[,"BOROUGH"][taxi_zone_lookup[, "BOROUGH"]=="Unknown"] <- NA
taxi_zone_lookup[,"ZONE"][taxi_zone_lookup[, "ZONE"]=="NV"] <- NA
taxi_zone_lookup[,"SERVICE_ZONE"][taxi_zone_lookup[, "SERVICE_ZONE"]=="N/A"] <- NA


## Taxi_Trips missing values

# Make missing datatypes consistent for reporting missing values purposes
# - Redefining unknown payment_type = 7 as "Unknown" for conventional label
taxi_trips_unduplicated[,"PAYMENT_TYPE"][taxi_trips_unduplicated[, "PAYMENT_TYPE"] == '7'] <- '5'

# We know that tip_amount only includes credit card tips, from which it follows all positive tip_amounts are credit_card payment type.
taxi_trips_unduplicated <- taxi_trips_unduplicated %>% 
  mutate(PAYMENT_TYPE = ifelse(TIP_AMOUNT > 0 & PAYMENT_TYPE == '5',
                               '1',
                               PAYMENT_TYPE))

# We know almost all congestion_surcharges occur in the inner city
#- NA congestion_surcharges will be allocated a value 2.75 for inner city '1'.
#- '3' Unknown trip_type will be given '1' if positive congestion_surcharge
taxi_trips_unduplicated <- taxi_trips_unduplicated %>% 
  mutate(TRIP_TYPE = ifelse(TRIP_TYPE=='3' & CONGESTION_SURCHARGE > 0 & !is.na(CONGESTION_SURCHARGE),
                               '1',
                               TRIP_TYPE)) %>% 
  mutate(CONGESTION_SURCHARGE = ifelse(is.na(CONGESTION_SURCHARGE) & TRIP_TYPE == '1',
                            2.75,
                            CONGESTION_SURCHARGE))


### Identify missing values and clean from data


## Analyse missing values in taxi_trips_unduplicated 

# Count number of missing values in taxi_trips_unduplicated and output as CSV in reports
taxi_trips_unduplicated_number_missing <- df_missing_count(taxi_trips_unduplicated)

# Automatically drop all columns containing exclusively missing data as there is no meaningful inference we can make from them other than data isn't gathered which we can infer from the missing count.
taxi_trips_unduplicated <- taxi_trips_unduplicated %>% 
  select_if(~ !all(is.na(.)))

# Store rows containing any missing values in any variable
taxi_trips_unduplicated_missing <- taxi_trips_unduplicated[rowSums(is.na(taxi_trips_unduplicated)) > 0, ]


## Analyse missing values in taxi_time_location

# Count number of missing values in taxi_time_location and output as CSV in reports
taxi_time_location_number_missing <- df_missing_count(taxi_time_location)

# Automatically drop all columns containing exclusively missing data as there is no fair inference we can make
taxi_time_location <- taxi_time_location %>% 
  select_if(~ !all(is.na(.)))

# Store rows containing any missing values
taxi_time_location_missing <- taxi_time_location[rowSums(is.na(taxi_time_location)) > 0, ]


## Analyse missing values in taxi_zone_lookup

# Count number of missing values in taxi_zone_lookup and output as CSV in reports
taxi_zone_lookup_number_missing <- df_missing_count(taxi_zone_lookup)

# Automatically drop all columns containing exclusively missing data as there is no possible fair inference we can make
taxi_zone_lookup <- taxi_zone_lookup %>% 
  select_if(~ !all(is.na(.)))

# Store rows containing any missing values
taxi_zone_lookup_missing <- taxi_zone_lookup[rowSums(is.na(taxi_zone_lookup)) > 0, ]



### Saving missing data to clean folder


## Produce excel spreadsheets of missing count and containing missing rows

# Save missing counts
# taxi_trips: STORE_AND_FWD_FLAG=32518, PASSENGER_COUNT=52518, RATECODEID=52518, PAYMENT_TYPE=32519, CONGESTION=52518, EHAIL_FEE=83691 (everything-drop)
# taxi_time_location: LOCATION_DETAILS = 167382 (everything-drop)
# taxi_zone_lookup: PICKUP_ZONE=1
write.xlsx(list("Taxi_Trips" = taxi_trips_unduplicated_number_missing, 
                "Taxi_Time_Location"= taxi_time_location_number_missing, 
                "Taxi_Zone_Lookup"=taxi_zone_lookup_number_missing), 
           file=file.path(reports_path, "missing_count.xlsx"))

# Save missing data rows for not entirely-missing columns 
# taxi_trips: Missing values all occur simultaneously on a row if ehail_fee dropped - always trip_type=3, Newark
# taxi_time_location: No missing rows if location_details dropped
# taxi_zone_lookup: locationid=264,265 have no known details
write.xlsx(list("Taxi_Trips" = taxi_trips_unduplicated_missing, 
                "Taxi_Time_Location"= taxi_time_location_missing, 
                "Taxi_Zone_Lookup"=taxi_zone_lookup_missing), 
           file=file.path(reports_path, "missing_data.xlsx"))



### Exploratory Data Analysis and Cleaning of Taxi_Trips_Unduplicated

# Drop entirely missing rows.
taxi_trips_unduplicated_cleaned <- taxi_trips_unduplicated[rowSums(is.na(taxi_trips_unduplicated)) != ncol(taxi_trips_unduplicated), ]


## Distributions of Trip Distances

# Plot distribution of trip_distances by trip_type, defining outliers as beyond 5*IQR as you expect a wider range of taxis
#- We see that trip_type=3 (other) has enormous outliers of up to 270,000 Miles which are clearly wrong and entered incorrectly
taxi_trips_unduplicated_cleaned %>% 
  ggplot(aes(STORE_AND_FWD_FLAG,
             TRIP_DISTANCE)) +
  geom_boxplot(coef=5,
               outlier.shape = 3, 
               outlier.size = 0.5,
               na.rm=TRUE) +
  labs(title="Boxplots of Trip Distance by Trip Type") +
  scale_x_discrete("Trip Type") +
  scale_y_continuous("Trip Distance", 
                     labels=label_number(suffix=" Miles", big.mark = ","),
                     n.breaks=10) +
  theme_minimal()

# Plot distribution of trip_distances by trip_type, defining outliers as beyond 5*IQR as you expect a wider range of taxis
#- A majority of non-extreme outlier data for Green taxi and Uber is up to 70 Miles  - we will filter the data to consider up to 70 Miles
boxplot_store_distance <- taxi_trips_unduplicated %>% 
  filter(TRIP_DISTANCE < 150) %>% 
  ggplot(aes(STORE_AND_FWD_FLAG, TRIP_DISTANCE)) +
  geom_boxplot(coef=5,
               outlier.shape = 3, 
               outlier.size = 0.5,
               na.rm=TRUE) +
  labs(title="Boxplots of Trip Distance by Forwarding Type (Up to 150 Miles)") +
  scale_x_discrete("Forwarding Type") +
  scale_y_continuous("Trip Distance", 
                     labels=label_number(suffix=" Miles", big.mark = ","),
                     n.breaks=10) +
  theme_minimal()

# Filter trip distances to those less than 70 miles as this is where competition for new york taxis would be generally restricted to
taxi_trips_unduplicated_cleaned <- taxi_trips_unduplicated_cleaned %>% 
  filter(TRIP_DISTANCE < 70 | is.na(TRIP_DISTANCE))

# Plot new distribution of trip_distances by trip_type, defining outliers as beyond 5*IQR as you expect a wider range of taxis
boxplot_type_distance <- taxi_trips_unduplicated_cleaned %>% 
  ggplot(aes(factor(TRIP_TYPE, 
                    levels = c('1', '2', '3'), 
                    labels=c("Inner City","Outer City", "Other")),
             TRIP_DISTANCE)) +
  geom_boxplot(coef=5,
               outlier.shape = 3, 
               outlier.size = 0.5,
               na.rm=TRUE) +
  labs(title="Boxplots of Trip Distance by Trip Type (Up to 70 Miles)") +
  scale_x_discrete("Trip Type") +
  scale_y_continuous("Trip Distance", 
                     labels=label_number(suffix=" Miles", big.mark = ","),
                     n.breaks=5) +
  theme_minimal()


## Distribution of Average Fare

# Density Plot of trip_distance vs fare_amount
# A few outliers of over $200.
bin2d_distance_fare <- taxi_trips_unduplicated_cleaned %>% 
  ggplot(aes(TRIP_DISTANCE, FARE_AMOUNT)) + 
  geom_bin2d(bins=100,
             na.rm=TRUE)+
  labs(title="Trip Distance versus Fare Amount (Up to 70 Miles)", fill="Count") +
  scale_x_continuous("Trip Distance", 
                     n.breaks=5,
                   labels=label_number(suffix=" Miles", big.mark = ",")) +
  scale_y_continuous("Fare Amount", 
                     labels=label_number(prefix="$", big.mark = ","),
                     n.breaks=10) +
  theme_minimal()

# Filtering data to only include fares of trips between -$200 and $200 and this is where the density of fairs is clustered. 
# - We only filter by fare not other costs as tips can be discretionary outliers, while fares should be somewhat fixed per mile.
taxi_trips_unduplicated_cleaned <- taxi_trips_unduplicated_cleaned %>% 
  filter((FARE_AMOUNT < 200 & FARE_AMOUNT > -200) | is.na(FARE_AMOUNT)) 

# Density Plot of trip_distance vs fare_amount after filtering
taxi_trips_unduplicated_cleaned %>% 
  ggplot(aes(TRIP_DISTANCE, FARE_AMOUNT)) + 
  geom_bin2d(bins=100,
             na.rm=TRUE)+
  labs(title="Trip Distance versus Fare Amount (Up to 70 Miles)", fill="Count") +
  scale_x_continuous("Trip Distance", 
                     n.breaks=5,
                     labels=label_number(suffix=" Miles", big.mark = ",")) +
  scale_y_continuous("Fare Amount", 
                     labels=label_number(prefix="$", big.mark = ","),
                     n.breaks=10) +
  theme_minimal()


## Distribution of passenger count

# Plot distribution of passenger count
# - A majority of passengers are 1, with the rest of a non-negligible amount all between 0 and 6. We will filter for 6 or less passengers.
# - There are some journeys with 0 passengers 
bar_passenger <- taxi_trips_unduplicated_cleaned %>% 
  ggplot(aes(PASSENGER_COUNT, 
             fill=factor(VENDORID, 
                         levels = c('1', '2', '3'),
                         labels = c("Green Taxi", "Uber", "Other")))) +
  geom_bar(position="Dodge",
           na.rm=TRUE) +
  labs(title="Number of Passengers on Trip Distribution", 
       fill="Vendor") +
  scale_fill_manual(values = c("Green Taxi"='green',
                               "Uber"='black')) +
  scale_x_continuous("Number of Passengers", 
                     n.breaks=10) +
  scale_y_continuous("Frequency", 
                     labels=label_number(big.mark = ","),
                     n.breaks=5) +
  theme_minimal()

# Filtering data to only include 0-6 passengers as this would be the limit of a typical taxi capacity.
taxi_trips_unduplicated_cleaned <- taxi_trips_unduplicated_cleaned %>% 
  filter(PASSENGER_COUNT<7 | is.na(PASSENGER_COUNT))


### Convert categorical taxi_trips_unduplicated_cleaned columns to factors for space-saving, readability, and reporting purposes.
taxi_trips_unduplicated_cleaned <- taxi_trips_unduplicated_cleaned %>% 
  mutate(VENDORID = factor(VENDORID, 
                           levels = c('1', '2', '3'),
                           labels = c("Green Taxi", "Uber", "Other"))) %>% 
  mutate(TRIP_TYPE = factor(TRIP_TYPE, 
                            levels = c('1', '2', '3'), 
                            labels=c("Inner City","Outer City", "Other"))) %>% 
  mutate(PAYMENT_TYPE = factor(PAYMENT_TYPE, 
                               levels=c('1','2','3','4','5','6'),
                               labels=c("Credit Card", "Cash", "No Charge", "Dispute", "Unknown", "Voided Trip"))) %>% 
  mutate(RATECODEID = factor(RATECODEID, 
                             levels=c('1','2','3','4','5','6'),
                             labels=c("Standard rate", "JFK", "Newark", "Nassau or Westchester", "Negotiated Fare", "Group Ride"))) %>% 
  mutate(STORE_AND_FWD_FLAG = factor(STORE_AND_FWD_FLAG,
                                     levels=c('Y','N'),
                                     labels=c('Store and Forward Trip', 'Not a Store and Forward Trip')))



### For reporting purposes setting NA in taxi_zone_lookup as Unknown
taxi_zone_lookup[,"BOROUGH"][is.na(taxi_zone_lookup[, "BOROUGH"])] <- "Unknown"
taxi_zone_lookup[,"ZONE"][is.na(taxi_zone_lookup[, "ZONE"])] <- "Unknown"
taxi_zone_lookup[,"SERVICE_ZONE"][is.na(taxi_zone_lookup[, "SERVICE_ZONE"])] <- "Unknown"



### Exporting cleaned data frames into clean data folder


## Export cleaned taxi_time_location 
write_csv(taxi_time_location,
          file.path(data_path_clean, "taxi_time_location_cleaned.csv"), 
          col_names=TRUE)


## Export cleaned taxi_zone_lookup
write_csv(taxi_zone_lookup,
          file.path(data_path_clean, "taxi_time_location_cleaned.csv"), 
          col_names=TRUE)


## Export cleaned taxi_trips
write_csv(taxi_trips_unduplicated_cleaned,
          file.path(data_path_clean, "taxi_trips_cleaned.csv"), 
          col_names=TRUE)



### Create taxi_time_location_wide


## Transposing taxi_time_location to taxi_time_location_wide with UNIQUEID as row identifier, with columns for pick-up and drop-off times and locations

# Making each UNIQUEID in taxi_time_location a row identifier
taxi_time_location_wide <- taxi_time_location %>% 
  pivot_wider(id_cols=c('UNIQUEID'),
              names_from = 'TRIP_TYPE',
              values_from = c('LOCATIONID', 'TIME'))

# Upper-case names of columns and using regex to change other column names from _PICKUP/_DROPOFF to be consistent with PICKUP_/DROPOFF_ below
# - (.*) prefix_characters, _ separator, ([^_]*) suffix_characters (longest final string that does not contain _)
# - \\2_\\1 reverse prefix and suffix as a substitution
names(taxi_time_location_wide) <- toupper(names(taxi_time_location_wide)) %>%
  sub("^(.*)_([^_]*)$", "\\2_\\1",.)
  
# Adding columns for pickup_year, pickup_date, pickup_weekday, pickup_hour
taxi_time_location_wide <- taxi_time_location_wide %>% 
  mutate(PICKUP_YEAR = year(PICKUP_TIME),
         PICKUP_DATE = date(PICKUP_TIME),
         PICKUP_WEEKDAY = wday(PICKUP_TIME, label = TRUE, abbr=FALSE),
         PICKUP_HOUR = hour(PICKUP_TIME))



### Preparing the data for profiling and analytics


## Merging taxi_trips and taxi_time_location_wide

# Merge and filter to create taxi_trips_nyc only including observations from 2021, using inner_join as it is the only way to guarantee year 2021
# - As only late-June to early-August data is available with the bulk of the data from July, we further filtered the data set to only consider July as this makes extrapolation to a year easier (knowledge of a lone month).
taxi_trips_nyc <-  taxi_time_location_wide %>% 
  filter(PICKUP_YEAR==2021 & month(PICKUP_DATE)==7) %>% 
  inner_join(taxi_trips_unduplicated_cleaned, by="UNIQUEID")



## Join to taxi_zone_lookup so that the location information can be included for any pickup recorded
taxi_trips_nyc <- taxi_zone_lookup %>% 
  rename_at(vars(c("LOCATIONID", "ZONE","BOROUGH", "SERVICE_ZONE")),
            ~c("PICKUP_LOCATIONID", "PICKUP_ZONE", "PICKUP_BOROUGH", "PICKUP_SERVICE_ZONE")) %>% 
  right_join(taxi_trips_nyc, by="PICKUP_LOCATIONID")



### Saving Cleaned Data To Clean Folder


## Produce csv of taxi_trips_nyc

write_csv(taxi_trips_nyc, 
          file=file.path(data_path_clean,"taxi_trips_nyc.csv"), 
          col_names=TRUE)