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
if (nrow(taxi_time_location_duplicates)>0){
  taxi_time_location_unduplicated <- df_unduplicated(taxi_time_location)
} else{
  print(paste("There were no duplicates in",deparse(substitute(taxi_time_location)),"- dataframe left unchanged."))
}

## taxi_zone_lookup duplicates

# Find taxi_zone_lookup duplicates
taxi_zone_lookup_duplicates <- df_duplicated(taxi_zone_lookup)

# Create dataframe without duplicates (if duplicates exist), 0  duplicates no need to recreate.
if (nrow(taxi_zone_lookup_duplicates)){
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



### Summary Statistics of Datasets

## Summary statistics of taxi_trips_unduplicated

# - No Missing UNIQUEID, 83691 values.
# - No Missing VENDORID, 1=7619, 2=4354, 3=32518 - Unbalanced distribution to consider later.
# - 32518 Missing STORE_AND_FWD_FLAG, N=50977, Y=196
# - RATECODEID has 8=32518, which isn't a valid assignment in appendix, will treat as NA next.
# - 32518 Missing PASSENGER_COUNT, LQ=1, UQ=1, Median=1. Max=32 - extreme values to be considered.
# - No Missing TRIP_DISTANCE, Mean=194.35Mi, LQ=1.35, UQ=6.20. Min trip_distance = 0 Mi, needs to be reviewed by cases to allocate appropriately. Max. trip distance=260517.93 Mi, large values to be filtered.
# - No Missing FARE_AMOUNT, Median=$16, LQ = $9, UQ=$26.83. Min=-$150 - need to check for disputes. Max=$480, large values to be filtered.
# - No Missing EXTRA, Mean=$1.16, Median=$0.50 LQ=$0, UQ=$2.75. Max=$8.25.
# - No Missing MTA_TAX. Min=-$0.50 - need to check for disputes.
# - No Missing TIP_AMOUNT. Min=-$1.14 - need to check for disputes. LQ=$0, Median=$0, Mean=$1.059, UQ=$1.66. Max=$87.71 - need to check extreme values in context.
# - No Missing TOLLS_AMOUNT. Min=$0, LQ=$0, UQ=$0, mean=$0.62. Max.=$30.05 - need to check for errors.
# - No Missing IMPROVEMENT_SURCHARGE. min=-$0.30, need to check corresponds to disputes.
# - No Missing TOTAL_AMOUNT. min=-$150.30 - disputes. LQ=$11.75, Median=$19.80 UQ=$31.30. Max=$480.31 - need to filter extreme values.
# - PAYMENT_TYPE 7=32518 which isn't an option, need to allocate NA. 1=29990, 2=20831,3=307,4=44,5=1 - uneven distribution.
# - No Missing TRIP_TYPE. 1=49413, 2=1760, 3=32518 - Uneven distribution, need to check allocation.
# - CONGESTION_SURCHARGE 32518 Missing. min=-$2.75, need to check disputes. max=$2.75.
print("Summary Statistics of Taxi_Trips_Unduplicated:")
print(summary(taxi_trips_unduplicated))


## Summary statistics of taxi_time_location

# - We see every uniqueID has a drop-off and pick-up (83691*2=167382)
print("Summary Statistics of Taxi_Time_Location:")
print(summary(taxi_time_location[,c("UNIQUEID", "TRIP_TYPE")]))



### Add consistency to missing values


## Taxi_Zone_Lookup

# Make missing datatypes consistent in taxi_zone_lookup so that they can be found by R more easily
# - Assuming NV is a typo for NA by context of NA in row and full text in the remainder of the column.
taxi_zone_lookup[,"BOROUGH"][taxi_zone_lookup[, "BOROUGH"]=="Unknown"] <- NA
taxi_zone_lookup[,"ZONE"][taxi_zone_lookup[, "ZONE"]=="NV"] <- NA
taxi_zone_lookup[,"SERVICE_ZONE"][taxi_zone_lookup[, "SERVICE_ZONE"]=="N/A"] <- NA


## Taxi_Trips

# Plot distribution of payment_type
taxi_trips_unduplicated %>% 
  ggplot(aes(PAYMENT_TYPE, fill=VENDORID)) +
  geom_bar(na.rm=TRUE, position = "dodge") +
  labs(title="Most Frequent Payment Type by Vendor") +
  scale_x_discrete("Payment Type") +
  scale_y_discrete("Frequency", 
                     labels=label_number(big.mark = ",")) +
  theme_minimal()

# Make missing datatypes consistent for reporting missing values purposes
# - Redefining unknown payment_type = 5 as NA for conventional label
taxi_trips_unduplicated[,"PAYMENT_TYPE"][taxi_trips_unduplicated[, "PAYMENT_TYPE"] == '5'] <- NA
# - Redefining unknown payment_type = 7 as NA as these values all correspond to VendorID 3 which always has NA columns, so assuming this is just funky-labelled NA.
taxi_trips_unduplicated[,"PAYMENT_TYPE"][taxi_trips_unduplicated[, "PAYMENT_TYPE"] == '7'] <- NA

# Plot distribution of payment_type after redefining values 5 and 7 as NA. 
taxi_trips_unduplicated %>% 
  ggplot(aes(PAYMENT_TYPE, fill=VENDORID)) +
  geom_bar(na.rm=TRUE, position = "dodge") +
  labs(title="Most Frequent Payment Type by Vendor") +
  scale_x_discrete("Payment Type") +
  scale_y_discrete("Frequency", 
                   labels=label_number(big.mark = ",")) +
  theme_minimal()

# Plot distribution of ratecodeid. 
taxi_trips_unduplicated %>% 
  ggplot(aes(RATECODEID, fill=VENDORID)) +
  geom_bar(position = "dodge") +
  labs(title="Most Frequent Rate by Vendor") +
  scale_x_discrete("Rate") +
  scale_y_discrete("Frequency", 
                   labels=label_number(big.mark = ",")) +
  theme_minimal()



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


## Distributions of Trip Distances

# Plot distribution of trip_distances by trip_type, defining outliers as beyond 5*IQR as you expect a wider range of taxis
#- We see that trip_type=3 (other) has enormous outliers of up to 270,000 Miles which are clearly wrong - we will filter the data to consider up to 40 Miles
taxi_trips_unduplicated %>% 
  ggplot(aes(TRIP_TYPE,
             TRIP_DISTANCE)) +
  geom_boxplot(coef=5,
               outlier.shape = 3, outlier.size = 0.5) +
  labs(title="Boxplots of Trip Distance by Trip Type") +
  scale_x_discrete("Trip Type") +
  scale_y_continuous("Trip Distance", 
                     labels=label_number(suffix=" Miles", big.mark = ","),
                     n.breaks=10) +
  theme_minimal()

# Filter trip distances to those less than 40 miles as this is where competition for new york taxis would be generally restricted to
taxi_trips_unduplicated_cleaned <- taxi_trips_unduplicated %>% 
  filter(TRIP_DISTANCE < 40 | is.na(TRIP_DISTANCE))

# Plot new distribution of trip_distances by trip_type, defining outliers as beyond 5*IQR as you expect a wider range of taxis
boxplot_type_distance <- taxi_trips_unduplicated_cleaned %>% 
  ggplot(aes(TRIP_TYPE,
             TRIP_DISTANCE)) +
  geom_boxplot(coef=5,
               outlier.shape = 3, outlier.size = 0.5) +
  labs(title="Boxplots of Trip Distance by Trip Type (Up to 50 Miles)") +
  scale_x_discrete("Trip Type") +
  scale_y_continuous("Trip Distance", 
                     labels=label_number(suffix=" Miles", big.mark = ","),
                     n.breaks=5) +
  theme_minimal()


## Distribution of Average Fare

# Density Plot of trip_distance vs fare_amount
taxi_trips_unduplicated %>% 
  ggplot(aes(TRIP_DISTANCE, FARE_AMOUNT)) + 
  geom_bin2d(bins=100)+
  labs(title="Trip Distance versus Fare Amount", colour="Count") +
  scale_x_continuous("Trip Distance", 
                     n.breaks=5,
                   labels=label_number(suffix=" Miles", big.mark = ",")) +
  scale_y_continuous("Fare Amount", 
                     labels=label_number(prefix="$", big.mark = ","),
                     n.breaks=10) +
  theme_minimal()

# Filtering data to only include fares of trips between -$100 and $100 and this is where the density of fairs is clustered. 
# - We only filter by fare not other costs as tips can be discretionary outliers, while fares should be somewhat fixed per mile.
taxi_trips_unduplicated_cleaned <- taxi_trips_unduplicated_cleaned %>% 
  filter((FARE_AMOUNT < 100 & FARE_AMOUNT > -100) | is.na(FARE_AMOUNT)) 

# Density Plot of trip_distance vs fare_amount after filtering
bin2d_distance_fare <- taxi_trips_unduplicated_cleaned %>% 
  ggplot(aes(TRIP_DISTANCE, FARE_AMOUNT)) + 
  geom_bin2d(bins=100)+
  labs(title="Trip Distance versus Fare Amount", fill="Count") +
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
  ggplot(aes(PASSENGER_COUNT, fill=VENDORID)) +
  geom_bar(position="Dodge") +
  labs(title="Number of Passengers on Trip Distribution", 
       fill="Vendor") +
  scale_x_continuous("Number of Passengers", 
                     n.breaks=10) +
  scale_y_continuous("Frequency", 
                     labels=label_number(big.mark = ","),
                     n.breaks=5) +
  theme_minimal()

# Filtering data to only include 0-6 passengers as this would be the limit of a typical taxi capacity.
taxi_trips_unduplicated_cleaned <- taxi_trips_unduplicated_cleaned %>% 
  filter(PASSENGER_COUNT<7 | is.na(PASSENGER_COUNT))

# Plot new distribution of passenger count
taxi_trips_unduplicated_cleaned %>% 
  ggplot(aes(PASSENGER_COUNT, fill=VENDORID)) +
  geom_bar(position="Dodge") +
  labs(title="Number of Passengers on Trip Distribution (Up to 7 Passengers)", 
       fill="Vendor") +
  scale_x_continuous("Number of Passengers",
                     n.breaks=6) +
  scale_y_continuous("Frequency", 
                     labels=label_number(big.mark = ","),
                     n.breaks=10) +
  theme_minimal()

## Add Total_Amount_Plus_Congestion
taxi_trips_unduplicated_cleaned$TOTAL_AMOUNT_PLUS_CONGESTION <- rowSums(taxi_trips_unduplicated_cleaned[,c("TOTAL_AMOUNT","CONGESTION_SURCHARGE")])






### Exporting cleaned data frames into clean data folder


## Export cleaned taxi_time_location 



## Export cleaned taxi_trips



## Export cleaned taxi_zone_lookup





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
taxi_trips_nyc <-  taxi_time_location_wide %>% 
  filter(PICKUP_YEAR==2021) %>% 
  inner_join(taxi_trips_unduplicated_cleaned, by="UNIQUEID")


## Join to taxi_zone_lookup so that the location information can be included for any pickup recorded
taxi_trips_nyc <- taxi_zone_lookup %>% 
  rename_at(vars(c("LOCATIONID", "ZONE","BOROUGH", "SERVICE_ZONE")),
            ~c("PICKUP_LOCATIONID", "PICKUP_ZONE", "PICKUP_BOROUGH", "PICKUP_SERVICE_ZONE")) %>% 
  right_join(taxi_trips_nyc, by="PICKUP_LOCATIONID")
  


### Saving Cleaned Data To Clean Folder


## Produce csv of taxi_trips_nyc

write_csv(taxi_trips_nyc, file=file.path(data_path_clean,"taxi_trips_nyc.csv"), col_names=TRUE)

