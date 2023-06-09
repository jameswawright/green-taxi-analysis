########################################################################
# Name: data_analysis.R                                                #
# Description: Data Analysis Performed On Data.                        #
# Creation Date: 06/06/2023                                            #
# Created by: James Wright                                             #
#             Graduate Programmer                                      #
#             Katalyze Data Ltd.                                       #
########################################################################

### Add profit column to taxi_trips_nyc
# Assuming green taxi pays $0.03 per mile while petrol vehicles pay $0.20 per mile
taxi_trips_nyc <- taxi_trips_nyc %>% 
  mutate(TRIP_PROFIT = ifelse(VENDORID=="Green Taxi", 
         TOTAL_AMOUNT - TOLLS_AMOUNT - MTA_TAX - IMPROVEMENT_SURCHARGE - 0.03*TRIP_DISTANCE,
         TOTAL_AMOUNT - TOLLS_AMOUNT - MTA_TAX - IMPROVEMENT_SURCHARGE - 0.20*TRIP_DISTANCE))


### Average amount of money spent on taxi trips with the Green Taxi vendor across the week

# Calculate average amount of money spent by Green taxi across the week
#- Filter data for rows to only include positive amount spent and only to include vendor=1 (Green taxi),
#- and only known data from credit card (1) cash (2) so we can guarantee the sample computing the average are not refunds or disputes.
average_spent_week_green_taxi_2021 <- taxi_trips_nyc %>% 
  filter(VENDORID == "Green Taxi", TOTAL_AMOUNT > 0, PAYMENT_TYPE %in% c("Credit Card", "Cash")) %>% 
  group_by(PICKUP_WEEKDAY) %>% 
  summarise(AVERAGE_SPENT_WEEK = scales::dollar(mean(TOTAL_AMOUNT)))


## Output summary table

write_csv(average_spent_week_green_taxi_2021,
          file.path(reports_path, "average_spent_week_green_taxi_2021.csv"), 
          col_names=TRUE)

### Features effect on the tip amount for Green Taxi


## Pick-up hour

# Boxplot of tip amount by pick-up hour
#- Filtering data to only include credit card values as tip_amount only includes these.
#- Using coefficient of 5 as you expect more extreme outlier variability in tips naturally based on different peoples' inherent generosity.
tipamt_pickup_hour <- taxi_trips_nyc %>% 
  filter(TIP_AMOUNT > 0 & PAYMENT_TYPE == "Credit Card") %>% 
  ggplot(aes(factor(PICKUP_HOUR), TIP_AMOUNT)) +
  geom_boxplot(coef=5,
               outlier.shape = 3, 
               outlier.size = 0.5,
               na.rm=TRUE) +
  labs(title="Boxplots of Tip Amounts by Pick-Up Hour") +
  scale_x_discrete("Pick-Up Hour") +
  scale_y_continuous("Tip Amount", 
                     labels=scales::dollar,
                     n.breaks=20) +
  theme_minimal()

# Bar chart of tip amount, binned as tip or no tip, by pick-up hour
#- Filtering data to only include credit card values as tip_amount only includes these
notip_pickup_hour <- taxi_trips_nyc %>% 
  filter(PAYMENT_TYPE == "Credit Card") %>% 
  ggplot(aes(factor(PICKUP_HOUR), fill=cut(TIP_AMOUNT,
                                      breaks = c(-Inf, 0, Inf),
                                      labels = c("No Tip", "Tip")))) +
  geom_bar(position='fill',
           na.rm=TRUE) +
  labs(title="Bar chart of Tip Amounts by Pick-Up Hour",
       fill="Tip Given") +
  scale_x_discrete("Pick-Up Hour") +
  scale_y_continuous("Probability",
                     n.breaks = 10,
                     labels=scales::percent) +
  theme_minimal()


## Payment Type - redundant due to lack of relevant tip amount data for anything but credit cards

print("Cannot graph payment type as tip amount is assessed purely on credit card data.")


## Passenger count

# Boxplot of tip amount by passenger_count
#- Filtering data to only include credit card values as tip_amount only includes these.
#- Using coefficient of 5 as you expect more extreme outlier variability in tips naturally based on different peoples' inherent generosity.
#- Excluding other vendor as passenger count was unknown
tipamt_passenger_count <- taxi_trips_nyc %>% 
  filter(TIP_AMOUNT > 0 & PAYMENT_TYPE == "Credit Card" & VENDORID !="Other") %>% 
  ggplot(aes(factor(PASSENGER_COUNT), TIP_AMOUNT)) +
  geom_boxplot(coef=5,
               outlier.shape = 5, 
               outlier.size = 0.5,
               na.rm=TRUE) +
  labs(title="Boxplots of Tip Amounts by Passenger Count") +
  scale_x_discrete("Passenger Count") +
  scale_y_continuous("Tip Amount", 
                     labels=scales::dollar,
                     n.breaks=20) +
  theme_minimal()

# Bar chart of tip amount, binned as tip or no tip, by passenger_count
#- Filtering data to only include credit card values as tip_amount only includes these
#- Excluding other vendor as passenger count was unknown
notip_passenger_count <- taxi_trips_nyc %>% 
  filter(PAYMENT_TYPE == "Credit Card" & VENDORID !="Other") %>% 
  ggplot(aes(factor(PASSENGER_COUNT), fill=cut(TIP_AMOUNT,
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("No Tip", "Tip")))) +
  geom_bar(position='fill',
           na.rm=TRUE) +
  labs(title="Boxplots of Tip Amounts by Passenger Count",
       fill="Tip Given") +
  scale_x_discrete("Passenger Count") +
  scale_y_continuous("Probability",
                     n.breaks = 10,
                     labels=scales::percent) +
  theme_minimal()


## Pick-up Borough 

# Boxplot of tip amount by pick-up borough
#- Filtering data to only include credit card values as tip_amount only includes these.
#- Using coefficient of 5 as you expect more extreme outlier variability in tips naturally based on different peoples' inherent generosity.
tipamt_pickup_borough <- taxi_trips_nyc %>% 
  filter(TIP_AMOUNT > 0 & PAYMENT_TYPE == "Credit Card") %>% 
  ggplot(aes(factor(PICKUP_BOROUGH), TIP_AMOUNT)) +
  geom_boxplot(coef=5,
               outlier.shape = 5, 
               outlier.size = 0.5,
               na.rm=TRUE) +
  labs(title="Boxplots of Tip Amounts by Pick-Up Borough") +
  scale_x_discrete("Pick-Up Borough") +
  scale_y_continuous("Tip Amount", 
                     labels=scales::dollar,
                     n.breaks=10) +
  theme_minimal()

# Bar chart of tip amount, binned as tip or no tip, by pick-up borough
#- Filtering data to only include credit card values as tip_amount only includes these
notip_pickup_borough <- taxi_trips_nyc %>% 
  filter(PAYMENT_TYPE == "Credit Card") %>% 
  ggplot(aes(factor(PICKUP_BOROUGH), fill=cut(TIP_AMOUNT,
                                               breaks = c(-Inf, 0, Inf),
                                               labels = c("No Tip", "Tip")))) +
  geom_bar(position='fill',
           na.rm=TRUE) +
  labs(title="Bar chart of Tip Amounts by Pick-Up Borough",
       fill="Tip Given") +
  scale_x_discrete("Pick-Up Borough") +
  scale_y_continuous("Probability",
                     n.breaks = 10,
                     labels=scales::percent) +
  theme_minimal()


## Trip distance

# Boxplot of tip amount by trip_distance
#- Filtering data to only include credit card values as tip_amount only includes these.
#- Using coefficient of 5 as you expect more extreme outlier variability in tips naturally based on different peoples' inherent generosity.
tipamt_trip_distance <- taxi_trips_nyc %>% 
  filter(TIP_AMOUNT > 0 & TRIP_DISTANCE > 0 & TRIP_DISTANCE < 40 & PAYMENT_TYPE == "Credit Card") %>% 
  ggplot(aes(TRIP_DISTANCE, TIP_AMOUNT)) +
  geom_point(size=0.5, 
             alpha = 0.55, 
             shape=3,
             na.rm=TRUE) + 
  geom_smooth(formula = y ~ x,
              method='lm', 
              colour='palevioletred', 
              size=1.25,
              na.rm=TRUE) +
  labs(title="Scatterplot of Tip Amounts by Pick-Up Borough") +
  scale_x_continuous("Trip Distance",
                     labels=label_number(suffix = " Miles")) +
  scale_y_continuous("Tip Amount", 
                     labels=scales::dollar,
                     n.breaks=10) +
  theme_minimal()

# Bar chart of tip amount, binned as tip or no tip, by trip_distance
#- Filtering data to only include credit card values as tip_amount only includes these
notip_trip_distance <- taxi_trips_nyc %>% 
  filter(PAYMENT_TYPE == "Credit Card" & !is.na(TRIP_DISTANCE)) %>% 
  ggplot(aes(cut(TRIP_DISTANCE,
                 breaks=c(-Inf, 5, 20, 40),
                 labels=c("0-5 Miles", "5-20 Miles", "20-40 Miles")), 
             fill=cut(TIP_AMOUNT,
                      breaks = c(-Inf, 0, Inf),
                      labels = c("No Tip", "Tip")))) +
  geom_bar(position='fill',
           na.rm=TRUE) +
  labs(title="Bar Chart of Tip Amounts by Trip Distance",
       fill="Tip Given") +
  scale_x_discrete("Trip Distance") +
  scale_y_continuous("Probability",
                     n.breaks = 10,
                     labels=scales::percent) +
  theme_minimal()





### Vendor that accumulated the most profit in 2021

# Compute total amount by vendorid and format
accumulated_profit_2021 <- taxi_trips_nyc %>%
  group_by(VENDORID) %>%
  summarise(TOTAL_PROFIT = scales::dollar(sum(TRIP_PROFIT)))

## Output summary table

write_csv(accumulated_profit_2021,
          file.path(reports_path, "accumulated_profit_2021.csv"), 
          col_names=TRUE)

###  Total number of rides, excluding voided trips, of Green Taxi and its competitors

# Compute total number of rides
GT_U_O_total_rides_2021 <- taxi_trips_nyc %>% 
  filter(PAYMENT_TYPE!="Voided Trip") %>%
  count(VENDORID)

## Output summary table

### How the profit amount has changed over 2021 for both the Green Taxi and Uber companies, both time series on same graph


## Plot of Time series plot of profit change over 2021 for Green taxi and Uber

GT_U_profit_2021 <- taxi_trips_nyc %>%
  filter(VENDORID %in% c("Green Taxi","Uber")) %>%
  group_by(VENDORID, PICKUP_DATE) %>% 
  summarise(TOTAL_PROFIT=sum(TRIP_PROFIT)) %>% 
  arrange(PICKUP_DATE, .by_group = TRUE) %>%
  ggplot(aes(PICKUP_DATE, TOTAL_PROFIT, colour=VENDORID)) +
  geom_line(na.rm = TRUE) +
  scale_color_manual(values = c("Green Taxi"='green',
                                "Uber"='black')) +
  labs(title="Daily Profit by Vendor in 2021*", 
       subtitle="*Only showing the period where data is available for both vendors",
       colour="Vendor",
       x="Pick-Up Date") + 
  scale_x_date(limits=as.Date(c("2021-07-01","2021-07-31"))) +
  scale_y_continuous("Profit",
                     labels=label_number(prefix=" $", big.mark = ","),
                     n.breaks=10) +
  theme_minimal() + 
  theme(legend.position = "top")

### Trip type that has brought the most revenue to Green Taxi

## Bar chart of total revenue for Green taxi by trip type
GT_revenue_2021 <- taxi_trips_nyc %>% 
  filter(VENDORID == "Green Taxi") %>%
  group_by(TRIP_TYPE) %>% 
  summarise(TOTAL_REVENUE=sum(TOTAL_AMOUNT)) %>%
  ggplot(aes(TRIP_TYPE, TOTAL_REVENUE)) +
  geom_col(fill='green',
           na.rm = TRUE) +
  labs(title="Green Taxi Revenue by Trip Type in July 2021",
       colour="Vendor",
       x="Pick-Up Date") + 
  scale_y_continuous("Profit",
                     labels=label_number(prefix=" $", big.mark = ","),
                     n.breaks=20) +
  theme_minimal()

### Pick-Up Borough Analysis

## Bar chart of pick up frequency count in each borough by vendor
#- Most popular for Green taxi
#- Under performing significantly relative to competitors
borough_pickup_2021_total <- taxi_trips_nyc %>% 
  ggplot(aes(PICKUP_BOROUGH, fill=VENDORID)) +
  geom_bar(position = 'dodge', 
           na.rm=TRUE) + 
  scale_fill_manual(values = c("Green Taxi"='green',
                                "Uber"='black',
                                "Other"='orange')) + 
  labs(title="Number of Pick-Ups in Each Borough by Vendor in July 2021",
       fill="Vendor",
       x="Pick-Up Borough") + 
  scale_y_continuous("Frequency",
                     n.breaks=20) +
  theme_minimal()

## Bar chart of proportion of pick-ups in each borough by vendor
borough_pickup_2021_proportion <- taxi_trips_nyc %>% 
  ggplot(aes(PICKUP_BOROUGH, fill=VENDORID)) +
  geom_bar(position = 'fill', 
           na.rm=TRUE) + 
  scale_fill_manual(values = c("Green Taxi"='green',
                               "Uber"='black',
                               "Other"='orange')) + 
  labs(title="Proportion of Pick-Ups in Each Borough by Vendor in July 2021",
       fill="Vendor",
       x="Pick-Up Borough") +
  scale_y_continuous("Market Share",
                     labels=scales::percent) +
  theme_minimal() 

###  Peak hours for the taxi services for different taxi vendors

## Frequency Polygon of taxi service hours by vendor

service_hours_2021 <- taxi_trips_nyc %>% 
  ggplot(aes(PICKUP_HOUR, colour=VENDORID)) +
  geom_freqpoly(bins=24, 
                na.rm=TRUE) +
  labs(title="Peak Hours by Vendor in July 2021", 
       colour="Vendor") +
  scale_color_manual(values = c("Green Taxi"='green',
                                "Uber"='black',
                                "Other"='orange')) + 
  scale_x_continuous("Pick-Up Hour",
                     n.breaks=12,
                     limits=c(0,23),
                     labels=label_number(accuracy=1, suffix = ":00")) +
  scale_y_continuous("Frequency",
                     labels=label_number(big.mark = ","),
                     n.breaks=20) +
  theme_minimal() + 
  theme(legend.position = "top")


## Bar chart to find most active in evening
active_evening_2021 <- taxi_trips_nyc %>% 
  ggplot(aes(cut(PICKUP_HOUR, breaks=c(-Inf, 9, 19, Inf), labels=c('Morning (Midnight-9:00)', 'Daytime (9:00-18:00)', 'Evening (18:00-Midnight)')), 
             fill=VENDORID)) +
  geom_bar(position='fill',
           na.rm=TRUE) +
  labs(title="Time of Day Popularity by Vendor in July 2021", 
       fill="Vendor") +
  scale_fill_manual(values = c("Green Taxi"='green',
                                "Uber"='black',
                                "Other"='orange')) + 
  scale_x_discrete("Time of Day") +
  scale_y_continuous("Market Share of Vendor Trips",
                     labels=scales::percent) +
  theme_minimal()



### Find  taxi provider has the biggest proportion of disputed trips out of their total number of taxi trips


## Bar chart for proportion of disputed trips out of total number of trips by vendor - recoding non-disputes as no dispute for comparison
disputed_proportion <- taxi_trips_nyc %>% 
  ggplot(aes(VENDORID, fill=recode(PAYMENT_TYPE,
                                   "Dispute" = "Dispute",
                                   "Credit Card" = "No Dispute",
                                   "Cash" = "No Dispute",
                                   "No Charge" = "No Dispute",
                                   "Unknown" = "No Dispute",
                                   "Voided Trip" = "No Dispute"))) +
  geom_bar(position='fill') + 
  labs(title="Proportion of Trips Being Disputes for Each Company in July 2021", 
       fill="Disputes") +
  scale_x_discrete("Vendor") + 
  scale_y_continuous("Proportion of Disputes", 
                     labels = percent,
                     n.breaks = 10) +
  theme_minimal()