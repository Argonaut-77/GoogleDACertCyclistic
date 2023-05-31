# Import packages needed
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(geosphere)

# Set working directory
setwd("C:/Users/<name>/Desktop/Google Data Analytics Capstone Project/Week 2/Track 1 Details - Bike Share or Wellness/Case Study 1 - Bike Share/Divvy Bike Trip Data - Cleaned CSV Files")

# Read in data
jan <- read_csv("cleaned-202101-divvy-tripdata.csv")

feb <- read_csv("cleaned-202102-divvy-tripdata.csv")

mar <- read_csv("cleaned-202103-divvy-tripdata.csv")

apr <- read_csv("cleaned-202104-divvy-tripdata.csv")

may <- read_csv('cleaned-202105-divvy-tripdata.csv')

jun <- read_csv("cleaned-202106-divvy-tripdata.csv")

jul <- read_csv("cleaned-202107-divvy-tripdata.csv")

aug <- read_csv("cleaned-202108-divvy-tripdata.csv")

sep <- read_csv("cleaned-202109-divvy-tripdata.csv")

oct <- read_csv('cleaned-202110-divvy-tripdata.csv')

nov <- read_csv("cleaned-202111-divvy-tripdata.csv")

dec <- read_csv("cleaned-202112-divvy-tripdata.csv")

# Combine data into one dataframe
all_trips_2021 <- bind_rows(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)

# Rename columns
all_trips_2021 <- all_trips_2021 %>%
                  rename(member_type = member_casual,
                         bike_type = rideable_type)

# Convert time and date columns to timestamp datatype
all_trips_2021 <- all_trips_2021 %>%
                    mutate(started_at = mdy_hm(started_at))

all_trips_2021 <- all_trips_2021 %>%
                    mutate(ended_at = mdy_hm(ended_at))

# Calculate and add ride length column, in minutes
all_trips_2021$ride_duration_min <- difftime(all_trips_2021$ended_at, all_trips_2021$started_at, units = "mins")
all_trips_2021$ride_duration_min <- as.numeric(as.character(all_trips_2021$ride_duration_min))
is.numeric(all_trips_2021$ride_duration_min)

# Select outliers of less than one minute
short_trips_2021 <- all_trips_2021[!(all_trips_2021$ride_duration_min>1),]

# Select rental outliers of more than 24 hours.
long_trips_2021 <- all_trips_2021[!(all_trips_2021$ride_duration_min<1440),]


# Eliminate ride records that are one minute or less or longer than 24 hours
all_trips_2021 <- all_trips_2021 %>%
                  filter(all_trips_2021$ride_duration_min > 1 &
                           all_trips_2021$ride_duration_min <= 1440)

# Eliminate any maintenance rentals
all_trips_2021 <- all_trips_2021 %>%
  filter(!grepl('Hubbard Bike-checking', start_station_id)) %>%
  filter(!grepl('Hubbard Bike-checking', end_station_id)) %>%
  filter(!grepl('DIVVY CASSETTE REPAIR', start_station_name)) %>%
  filter(!grepl('DIVVY CASSETTE REPAIR', end_station_name)) %>%
  filter(!grepl('Lyft Driver Center', start_station_name)) %>%
  filter(!grepl('Lyft Driver Center', end_station_name)) %>%
  filter(!grepl('Base - 2132 W', start_station_name)) %>%
  filter(!grepl('Base - 2132 W', end_station_name)) %>%
  filter(!grepl('WEST CHI-WATSON', start_station_name)) %>%
  filter(!grepl('WEST CHI-WATSON', end_station_name)) %>%
  filter(!grepl('351', start_station_name)) %>%
  filter(!grepl('351', end_station_name))


# Add columns of granular date data from started at data column
all_trips_2021$start_date <- as.Date(all_trips_2021$started_at)#The default format is yyyy-mm-dd
all_trips_2021$start_month <- format(as.Date(all_trips_2021$start_date), "%m")
all_trips_2021$start_day <- format(as.Date(all_trips_2021$start_date), "%d")
all_trips_2021$start_year <- format(as.Date(all_trips_2021$start_date), "%Y")
all_trips_2021$start_day_of_week <- format(as.Date(all_trips_2021$start_date), "%A")
all_trips_2021$start_hour <- format(all_trips_2021$started_at, "%H")

# Add columns of granular date data from ended at data column
all_trips_2021$end_date <- as.Date(all_trips_2021$ended_at)#The default format is yyyy-mm-dd
all_trips_2021$end_month <- format(as.Date(all_trips_2021$end_date), "%m")
all_trips_2021$end_day <- format(as.Date(all_trips_2021$end_date), "%d")
all_trips_2021$end_year <- format(as.Date(all_trips_2021$end_date), "%Y")
all_trips_2021$end_day_of_week <- format(as.Date(all_trips_2021$end_date), "%A")
all_trips_2021$end_hour <- format(all_trips_2021$ended_at, "%H")


# Arrange the days of the week in proper order
all_trips_2021$start_day_of_week <- ordered(all_trips_2021$start_day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
all_trips_2021$end_day_of_week <- ordered(all_trips_2021$end_day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


# Remove any records missing longitude or latitude info
all_trips_2021 <- all_trips_2021 %>%
  drop_na(start_lat, start_lng,
          end_lat, end_lng)

# Round lat and lng coordinates to 4 decimal places
all_trips_2021 <- all_trips_2021 %>%
  mutate(
    rounded_start_lat = round(start_lat, 4),
    rounded_start_lng = round(start_lng, 4),
    rounded_end_lat   = round(end_lat, 4),
    rounded_end_lng   = round(end_lng, 4)
  )

# Combine starting and ending lat and lng into one column
all_trips_2021 = all_trips_2021 %>%
  mutate(
    start_lat_lng = paste(rounded_start_lat, rounded_start_lng, sep = ', '),
    end_lat_lng   = paste(rounded_end_lat, rounded_end_lng, sep = ', ')
  )

# Remove the rounded lat and lng columns
all_trips_2021 = subset(all_trips_2021, select = -c(rounded_start_lat,
                                                              rounded_start_lng,
                                                              rounded_end_lat,
                                                              rounded_end_lng))

# Check the values of membership column
unique(all_trips_2021$member_type)

## Cleaning is done. Analysis begins.

# Calculate descriptive statistics of ride length column
mean(all_trips_2021$ride_duration_min)
median(all_trips_2021$ride_duration_min)
max(all_trips_2021$ride_duration_min)
min(all_trips_2021$ride_duration_min)

# Summary table for the descriptive statistics of ride length
summary(all_trips_2021$ride_duration_min)

# Descriptive statistics of ride length by member type
aggregate(all_trips_2021$ride_duration_min ~ all_trips_2021$member_type, FUN = mean)
aggregate(all_trips_2021$ride_duration_min ~ all_trips_2021$member_type, FUN = median)
aggregate(all_trips_2021$ride_duration_min ~ all_trips_2021$member_type, FUN = max)
aggregate(all_trips_2021$ride_duration_min ~ all_trips_2021$member_type, FUN = min)



## Total rides table and charts

# Summary table of number of rides and average ride duration by member type
total_rides_by_member_type <- all_trips_2021 %>%
  group_by(member_type) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_duration_min)) %>%
  arrange(desc(number_of_rides))

# Bar chart of the number of rides, by member type
ggplot(data = total_rides_by_member_type, aes(x = member_type,
                                              y = number_of_rides,
                                              fill = member_type)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values=c("#2A5783", "#AE123A")) +
  labs(title = "Total number of rides, by member type",
       subtitle = "For 2021",
       x = "Member type",
       y = "Number of rides",
       caption = "Data courtesy of Motivate International Inc.",
       fill = "Member Type")

# Bar chart of the average ride duration, by member type
ggplot(data = total_rides_by_member_type, aes(x = member_type,
                                              y = average_duration,
                                              fill = member_type)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values=c("#2A5783", "#AE123A")) +
  labs(title = "Average Ride Duration, by member type",
       subtitle = "For 2021",
       x = "Member type",
       y = "Ride Duration (in minutes)",
       caption = "Data courtesy of Motivate International Inc.",
       fill = "Member Type")


## Month table and charts

# Summary table of average ride duration and number of rides by member type, by month
month_breakdown_table <- all_trips_2021 %>% 
  mutate(month = month(started_at, label = TRUE)) %>%  
  group_by(member_type, month) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_duration_min)) %>% 		
  arrange(member_type, month)	


# Grouped bar chart of the number of rides by member type, by month
ggplot(data = month_breakdown_table, aes(fill = member_type,
                                         x = month,
                                         y = number_of_rides))+
  geom_bar(position = "dodge",
           stat = "identity") +
  scale_y_continuous(labels = comma,
                     n.breaks = 10) +
  labs(title = "Number of rides by member type, by month",
       subtitle = "For 2021",
       x = "Month",
       y = "Number of rides",
       fill = "Member Type",
       caption = "Data courtesy of Motivate International Inc.")+
  scale_fill_manual(values=c("#2A5783", "#AE123A"))



## Day of the week table and graph(s)

# Table of the average ride duration and number of rides by member type, by day of the week
weekday_breakdown_by_member_type <- all_trips_2021 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_type, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_duration_min)) %>% 		
  arrange(member_type, weekday)	

# Grouped bar chart of the number of rides by member type, by day of week
ggplot(data = weekday_breakdown_by_member_type, aes(fill = member_type,
                                                    x = weekday,
                                                    y = number_of_rides))+
  geom_bar(position = "dodge",
           stat = "identity") +
  scale_y_continuous(labels = comma,
                     n.breaks = 10) +
  labs(title = "Number of rides by member type, by weekday",
       subtitle = "For 2021",
       x = "Day of the week",
       y = "Number of rides",
       fill = "Member Type",
       caption = "Data courtesy of Motivate International Inc.")+
  scale_fill_manual(values=c("#2A5783", "#AE123A"))

# Grouped bar chart of the average ride duration by member type, by day of week
ggplot(data = weekday_breakdown_by_member_type, aes(fill = member_type,
                                                    x = weekday,
                                                    y = average_duration)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values=c("#2A5783", "#AE123A")) +
  labs(title = "Average ride duration by member type, by weekday",
       subtitle = "For 2021",
       x = "Day of the week",
       y = "Average Duration - in minutes",
       fill = "Member Type",
       caption = "Data courtesy of Motivate International Inc.")

##Hourly table and chart(s)

# Breakdown table of number of trips per hour, by member type
hourly_breakdown_by_member_type <- all_trips_2021 %>% 
  mutate(hour = hour(started_at)) %>%  
  group_by(member_type, hour) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_duration_min)) %>% 		
  arrange(member_type, hour)	

# Create 12 hour labels for line graph
hour_labels <- c("12am", "1am", "2am", "3am", "4am", "5am", "6am", "7am", "8am", "9am", "10am", "11am",
                 "12pm", "1pm", "2pm", "3pm", "4pm", "5pm", "6pm", "7pm", "8pm", "9pm", "10pm", "11pm")

# Line graph of number of rides per hour, by member type
ggplot(data = hourly_breakdown_by_member_type, aes(x = hour,
                                                   y = number_of_rides)) +
  geom_line(aes(color=member_type), size = 1.2) +
  scale_color_manual(name = 'Member Type', labels = c('Casual', 'Member'),
                     values = c("#2A5783", "#AE123A")) +
  geom_point(size = 2) +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(limits = hour_labels) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Number of rides per hour, by member type",
       subtitle = "For 2021",
       x = "Hour of day",
       y = "Number of rides",
       caption = "Data courtesy of Motivate International Inc.")

## Bike type breakdowns

# Summary table of bike type usage
summary_bike_type_usage <- all_trips_2021 %>% 
  group_by(bike_type) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_duration_min)) %>% 		
  arrange(bike_type)

# Summary table of bike type usage, by member type
bike_type_usage_by_member_type <- all_trips_2021 %>% 
  group_by(member_type, bike_type) %>%  
  drop_na() %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_duration_min)) %>% 		
  arrange(member_type, bike_type)	

# Summary table of bike type usage, by weekday, by member type
weekday_by_member_bike_types <- all_trips_2021 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_type, bike_type, weekday) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_duration_min)) %>% 		
  arrange(member_type, bike_type, weekday)

# Column chart of bike type usage, by member type
ggplot(data = bike_type_usage_by_member_type, aes(fill = reorder(bike_type, number_of_rides),
                                                  x = member_type,
                                                  y = number_of_rides))+
  geom_bar(position = "dodge",
           stat = "identity") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values=c("#2A5783", "#9AD1D0")) +
  labs(title = "Bike type usage, by member type",
       subtitle = "For 2021",
       x = "Member Type",
       y = "Number of Rides",
       fill = "Bike Type",
       caption = "Data courtesy of Motivate International Inc.")

# Grouped column charts of number of rides by weekday, by member_type
ggplot(data = weekday_by_member_bike_types, aes(fill = reorder(bike_type, number_of_rides),
                                                x = weekday,
                                                y = number_of_rides))+
  geom_bar(position = "dodge",
           stat = "identity") +
  facet_wrap(~member_type) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values=c("#2A5783", "#9AD1D0")) +
  labs(title = "Weekday Bike Type usage, by member type",
       subtitle = "For 2021",
       x = "Day of the week",
       y = "Number of Rides",
       fill = "Bike Type",
       caption = "Data courtesy of Motivate International Inc.")


# Table summarizing rides that were longer than a day
total_outlier_rides <- long_trips_2021 %>%
  group_by(member_type) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_duration_min)) %>%
  arrange(desc(number_of_rides))

# Export cleaned dataset as single CSV file
write.csv(all_trips_2021,"cleaned_all_2021_trips.csv", row.names = FALSE)
