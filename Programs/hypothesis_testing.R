########################################################################
# Name: hypothesis_testing.R                                           #
# Description: Hypothesis Testing Feature Importance                   #
# Creation Date: 09/06/2023                                            #
# Created by: James Wright                                             #
#             Graduate Programmer                                      #
#             Katalyze Data Ltd.                                       #
########################################################################

########## Please 'run all' chunks in Reporting.RMD before this :) #############

# Generalized Additive Models library needed
library(gam)

# Set significance level - chosen as you would expect tip amount medians to be inherently volatile, so you must be more strict in what is considered exceptional
alpha = 0.05

# Note: In tipamt_taxi_trips we have filtered to only consider non-zero tip values, 
# as we considered the feature prediction power of tip or no tip separately by looking at information gain relative 
# to guessing based on the population tip vs no tip or a coin toss.
# For the sake of this section, assume we have already conditioned on expecting a tip to be given, and therefore want to know what it will be.




#### Features effect on the tip amount for Green Taxi



### Test for normality in population data set


## Applying Shapiro-Wilk Test

#H0: Normally Distributed
#H1: Not Normally Distributed
shapiro.test(tipamt_taxi_trips$TIP_AMOUNT)
#- P-Value < 0.10, sufficient evidence to reject normality in the population of tip amounts.

# QQPlot to verify above rejection of normality
qqnorm(tipamt_taxi_trips$TIP_AMOUNT)
#- Not close to y=x.

# Verified further by right-skew of plot
tipamt_taxi_trips %>% 
  ggplot(aes(TIP_AMOUNT)) +
  geom_density() +
  labs(title="Distribution of Green Taxi Tip Amounts",
       fill="Tip Given") +
  scale_x_continuous("Tip Amount",
                   labels=scales::dollar) +
  scale_y_continuous("Frequency") +
  theme_minimal()

#- Hence, we must use non-parametric tests.



### Pick-up hour


## Summary Statistics
#- Tells us there are too low counts in hours 1-4, possibly 23, for hypothesis testing reliably
print(tipamt_stats_pickup_hour, n=nrow(tipamt_stats_pickup_hour))


## Drop low count columns less than 30 observations from tipamt_taxi_trips before conducting test
tipamt_taxi_trips_pickup_hour_n30 <- tipamt_stats_pickup_hour %>%
  filter(count_tip > 30) %>% 
  inner_join(tipamt_taxi_trips, 
             by="PICKUP_HOUR")


## Kruskal-Wallis ANOVA

# H0: The median tip amount for each pick-up hour is the same.
# H1: There exists at least one hour where the median tip amount given is different from the other hours.
kruskal.test(TIP_AMOUNT ~ PICKUP_HOUR, data = tipamt_taxi_trips_pickup_hour_n30)
#- P-value=0.04 greater than alpha=0.05, so there is sufficient evidence to suggest a difference between at least one pick up hour median tip and the other hours tip medians.



### Passenger count


## Summary Statistics
#- Tells us there are too low observations for passenger counts 0 and 7, possibly 4, for hypothesis testing reliably
print(tipamt_stats_passenger_count, n=nrow(tipamt_stats_passenger_count))


## Drop low count columns less than 30 observations from tipamt_taxi_trips before conducting test
tipamt_taxi_trips_passenger_count_n30 <- tipamt_stats_passenger_count %>%
  filter(count_tip > 30) %>% 
  inner_join(tipamt_taxi_trips, 
             by="PASSENGER_COUNT")


## Kruskal-Wallis ANOVA

# H0: The median tip amount for each passenger count is the same.
# H1: There exists at least one amount of passengers where the median tip amount given is different from the other passenger numbers.
kruskal.test(TIP_AMOUNT ~ PASSENGER_COUNT, data = tipamt_taxi_trips_passenger_count_n30)
#- P-value=0.037 greater than alpha=0.05, so there is sufficient evidence to suggest a difference between at least one passenger number median tip and the other passenger number tip medians.



### Pick-up Borough 


## Summary Statistics
#- Tells us there are too low observations for the Unknown borough for hypothesis testing reliably
print(tipamt_stats_pickup_borough, n=nrow(tipamt_stats_pickup_borough))


## Drop low count columns less than 30 observations from tipamt_taxi_trips before conducting test
tipamt_taxi_trips_pickup_borough_n30 <- tipamt_stats_pickup_borough %>%
  filter(count_tip > 30) %>% 
  inner_join(tipamt_taxi_trips, 
             by="PICKUP_BOROUGH")


## Kruskal-Wallis ANOVA

# H0: The median tip amount for each borough is the same.
# H1: There exists at least one borough where the median tip amount given is different from the other boroughs.
kruskal.test(TIP_AMOUNT ~ PICKUP_BOROUGH, data = tipamt_taxi_trips_pickup_borough_n30)
#- P-value=0.000008623 less than alpha=0.05, so there is sufficient evidence to suggest a difference between at least one median tip and the other tip medians.



### Trip distance


## Test simple linear regression assumptions

# Test residuals 
#- Create a linear regression model of tip amount on trip distance and test residuals are normally distributed
model <- lm(TIP_AMOUNT ~ TRIP_DISTANCE, data=tipamt_taxi_trips)
#- Applying Shapiro-Wilk Test
#--H0: Normally Distributed
#--H1: Not Normally Distributed
shapiro.test(model$residuals)
#- P-Value=0.00000022 < 0.10, sufficient evidence to reject normality of residuals.

#- We therefore cannot use simple linear regression.

## Non-parametric regression - Generalised Additive Model
#- https://en.wikipedia.org/wiki/Generalized_additive_model
#- https://en.wikipedia.org/wiki/Basis_function
#H0: All coefficients of the basis function expansion in TRIP_DISTANCE are 0
#H1: At least one coefficient of the basis functions is non-zero
gam_model <- gam(TIP_AMOUNT ~ TRIP_DISTANCE, data=tipamt_taxi_trips)
summary(gam_model)
#- We see a p-value < 0.05 in ANOVA for Parametric Effects, meaning that there is sufficient evidence to suggest a relationship between 
#- TRIP_DISTANCE and TIP_AMOUNT as there is a non-zero coefficient of at least one basis function in TRIP_DISTANCE


# Plot GAM model
tipamt_taxi_trips %>% 
  ggplot(aes(TRIP_DISTANCE, TIP_AMOUNT)) +
  geom_point(na.rm=TRUE) +
  geom_smooth(method='gam', 
              na.rm=TRUE) +
  labs(title="Scatterplot of Tip Amounts by Distance") +
  scale_x_continuous("Trip Distance",
                     labels=label_number(suffix = " Miles"),
                     limits=c(0,40)) +
  scale_y_continuous("Tip Amount", 
                     labels=scales::dollar,
                     n.breaks=10) +
  theme_minimal()
