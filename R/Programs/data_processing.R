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



### Cleaning taxi_zone_lookup

# For reporting purposes, turn missing entries in taxi_zone_lookup to Unknown
taxi_zone_lookup[taxi_zone_lookup$LOCATIONID %in% c(264,265), 
                 c("BOROUGH", "ZONE", "SERVICE_ZONE")] <- "Unknown"



### Exploratory Data Analysis and Cleaning of Taxi_Trips_Unduplicated


## Distributions of Trip Distances

# Plot distribution of trip_distances by trip_type, defining outliers as beyond 10*IQR as you expect a wider range of taxis
#- We see that trip_type=3 (other) has enormous outliers of up to 270,000 Miles which are clearly wrong - we will filter the data to consider up to 50 Miles
taxi_trips_unduplicated %>% 
  ggplot(aes(TRIP_TYPE,
             TRIP_DISTANCE)) +
  geom_boxplot(coef=10,
               outlier.shape = 3, outlier.size = 0.5) +
  labs(title="Boxplots of Trip Distance by Trip Type") +
  scale_x_discrete("Trip Type") +
  scale_y_continuous("Trip Distance", 
                     labels=label_number(suffix=" Miles", big.mark = ","),
                     n.breaks=10) +
  theme_minimal()

# Filter trip distances to those less than 50 miles.
taxi_trips_unduplicated_cleaned <- taxi_trips_unduplicated %>% 
  filter(TRIP_DISTANCE < 50 | is.na(TRIP_DISTANCE))

# Plot new distribution of trip_distances by trip_type, defining outliers as beyond 10*IQR as you expect a wider range of taxis
taxi_trips_unduplicated_cleaned %>% 
  ggplot(aes(TRIP_TYPE,
             TRIP_DISTANCE)) +
  geom_boxplot(coef=10,
               outlier.shape = 3, outlier.size = 0.5) +
  labs(title="Boxplots of Trip Distance by Trip Type (Up to 50 Miles)") +
  scale_x_discrete("Trip Type") +
  scale_y_continuous("Trip Distance", 
                     labels=label_number(suffix=" Miles", big.mark = ","),
                     n.breaks=5) +
  theme_minimal()

# Plot distribution of trip distances by payment_type, defining outliers as beyond 10*IQR as you expect a wider range of taxis
#- We see there is no payment_type 6=voided trip
#- We see that NA payment_type has enormous outliers - we will filter the data to consider up to 50 Miles, "competitor" journeys.
# - We see there is no payment_type 6=voided trip, we will assume voided implies no distance travelled.
# - here is type NA which we can reclassify as unknown.
# - We see most payment_type have trips of distance 0, which we will assume are cancelled and relabel as voided
taxi_trips_unduplicated_cleaned %>% 
  ggplot(aes(PAYMENT_TYPE,
             TRIP_DISTANCE)) +
  geom_boxplot(coef=10,
               outlier.shape = 3, outlier.size = 0.5) +
  labs(title="Boxplots of Trip Distance by Payment Type") +
  scale_x_discrete("Payment Type") +
  scale_y_continuous("Trip Distance", 
                     labels=label_number(suffix=" Miles", big.mark = ","),
                     n.breaks=10) +
  theme_minimal()

# Reclassifying missing payment_type as '5' (unknown)
taxi_trips_unduplicated_cleaned <- taxi_trips_unduplicated_cleaned %>% 
  mutate(PAYMENT_TYPE = ifelse(is.na(PAYMENT_TYPE), 
                               '5', 
                               PAYMENT_TYPE))

# Plot new distribution of trip distances by payment_type, defining outliers as beyond 10*IQR as you expect a wider range of taxis
taxi_trips_unduplicated_cleaned %>% 
  ggplot(aes(PAYMENT_TYPE,
             TRIP_DISTANCE)) +
  geom_boxplot(coef=10,
               outlier.shape = 3, outlier.size = 0.5) +
  labs(title="Boxplots of Trip Distance (Up To 50 Miles) by Redefined Payment Type") +
  scale_x_discrete("Payment Type") +
  scale_y_continuous("Trip Distance", 
                     labels=label_number(suffix=" Miles", big.mark = ","),
                     n.breaks=5) +
  theme_minimal()


## Distribution of FARE_AMOUNT by PAYMENT_TYPE

# Box plot of fair_amount by payment_type, defining outliers as beyond 10*IQR as you expect a wider range of taxis
# - We see payment_type 3=No charge and 4=Dispute have non-zero and positive values respectively, but these should be only zero and negative.
# - 5=Unknown has negative values, these should be allocated to disputes.
# - Most of the distribution of journeys is skewed right, with a non-negligable fare counts below $200 for cash and credit types. We will filter only to include data under $200.
# - There are some journeys with negative fees (disputes), with non-negligable fare counts above -$25 for cash and credit types. We will filter only to include data above -$25.
taxi_trips_unduplicated_cleaned %>% 
  ggplot(aes(PAYMENT_TYPE, FARE_AMOUNT)) +
  geom_boxplot(coef=10,
               outlier.shape = 3, outlier.size = 0.5) +
  labs(title="Box Plot of Fare Amount by Payment Type") +
  scale_x_discrete("Payment Type") +
  scale_y_continuous("Fare Amount", 
                     labels=label_number(prefix="$", big.mark = ","),
                     n.breaks=10) +
  theme_minimal()

# Filtering data to only include trips between -$25 and $200. 
taxi_trips_unduplicated_cleaned <- taxi_trips_unduplicated_cleaned %>% 
  filter((FARE_AMOUNT < 200 & FARE_AMOUNT > -25) | is.na(FARE_AMOUNT)) 


taxi_trips_unduplicated_cleaned %>% 
  ggplot(aes(TRIP_DISTANCE, FARE_AMOUNT, colour=PAYMENT_TYPE)) +
  geom_point(alpha=0.1)

# THINK MORE CAREFULLY ABOUT FILTERING PAYMENT TYPES LIKE THIS ------------------


# Adjusting payment_types 
# - if fare_amount>0 and payment_type not 1,2 (credit,cash) then payment_type=5 (unknown)
# - if fare_amount = 0 then payment_type=3 (no charge)
# - if fare_amount<0 then payment_type=4 (dispute)
# - if trip_distance=0, assume it was a 6=voided_trip.
taxi_trips_unduplicated_cleaned <- taxi_trips_unduplicated_cleaned %>% 
  mutate(PAYMENT_TYPE = case_when(FARE_AMOUNT == 0 ~ "3",
                                  FARE_AMOUNT < 0 ~ "4",
                                  ((FARE_AMOUNT > 0 ) & (PAYMENT_TYPE!='1') & (PAYMENT_TYPE!='2') & (PAYMENT_TYPE!='6')) ~ "5",
                                  TRUE ~ PAYMENT_TYPE)) %>% 
  mutate(PAYMENT_TYPE=ifelse(TRIP_DISTANCE == 0, 
                             "6",
                             PAYMENT_TYPE))

# Box plot of fare_amount by payment_type after recategorisation of payment_type, defining outliers as beyond 10*IQR as you expect a wider range of taxis
print(
  taxi_trips_unduplicated_cleaned %>% 
    ggplot(aes(PAYMENT_TYPE, FARE_AMOUNT)) +
    geom_boxplot(coef=10,
                 outlier.shape = 3, outlier.size = 0.5) +
    labs(title="Box Plot of Fare Amount by Payment Type (After Recategorisation of Payment Types)") +
    scale_x_discrete("Payment Type") +
    scale_y_continuous("Fare Amount", 
                       labels=label_number(prefix="$", big.mark = ","),
                       n.breaks=5) +
    theme_minimal())


## Distribution of passenger count

# Plot distribution of passenger count
# - A majority of passengers are 1, with the rest of a non-negligible amount all between 0 and 7. We will filter for 7 or less passengers.
# - There are some journeys with 0 passengers 
taxi_trips_unduplicated_cleaned %>% 
  ggplot(aes(PASSENGER_COUNT)) +
  geom_bar(na.rm=TRUE) +
  labs(title="Number of Passengers on Trip Distribution") +
  scale_x_continuous("Number of Passengers", 
                     n.breaks=10) +
  scale_y_continuous("Frequency", 
                     labels=label_number(big.mark = ","),
                     n.breaks=5) +
  theme_minimal()

# Filtering data to only include 0-6 passengers as this would be the limit of a typical taxi competitor.
taxi_trips_unduplicated_cleaned <- taxi_trips_unduplicated_cleaned %>% 
  filter(PASSENGER_COUNT<7 | is.na(PASSENGER_COUNT))

# Plot new distribution of passenger count
taxi_trips_unduplicated_cleaned %>% 
  ggplot(aes(PASSENGER_COUNT)) +
  geom_bar(na.rm=TRUE) +
  labs(title="Number of Passengers on Trip Distribution (Up to 7 Passengers)") +
  scale_x_continuous("Number of Passengers") +
  scale_y_continuous("Frequency", 
                     labels=label_number(big.mark = ","),
                     n.breaks=5) +
  theme_minimal()


## Distribution of price-per-mile for ratecodeid

# Boxplot of fare_amount/trip_distance for each ratecodeid
# - We see a lot of the rate codes have price-per-mile of thousands of dollars which is not normal - we will filter only to include reasonable fairs per mile.
print(
  taxi_trips_unduplicated_cleaned %>% 
    filter(TRIP_DISTANCE > 1) %>% 
    ggplot(aes(RATECODEID, (FARE_AMOUNT/TRIP_DISTANCE))) +
    geom_boxplot() +
    labs(title="Boxplots of Fare Price-Per-Mile by Rate Code") +
    scale_x_discrete("Rate Code") +
    scale_y_continuous("Price-Per-Mile", 
                       labels=label_number(prefix="$", big.mark = ","),
                       n.breaks=10) +
    theme_minimal())















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

