#Loading Packages
library(dplyr)
library(tidyverse)
library(ggplot2)

setwd("D:\\Shabana_R_Project")#collecting data(step_1)
q1_2020<- read.csv("Divvy_Trips_2020_Q1.csv")
q4_2019<-read.csv("Divvy_Trips_2019_Q4.csv")


#combining in a single file(step_2)
colnames(q1_2020)
colnames(q4_2019)


#renaming column as in q1_2020
(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))



# Inspect the dataframes and look for inconguencies
str(q1_2020)
str(q4_2019)


#converting ride_id and rideable type as characters
 q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
 

# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(q4_2019, q1_2020)


# Remove lat, long,and as this data was dropped beginning in 2020
# Assuming your data frame is named 'your_data'
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "tripduration"))



#cleanup and add data(step_3)
#inspecting the new table
colnames(all_trips)  
nrow(all_trips)
dim(all_trips)  
head(all_trips)  
str(all_trips)  
summary(all_trips)  


#reassigning desired values going with the current 2020 table(all_trips)
#in the member_casual column changing "subscriber" with "member" and "customer" with "casual"
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

#checking that table was reassigned
table(all_trips$member_casual)


#Adding columns that list date, day, month and year for each ride(allows us to aggregate for each month, year, and day)
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")


#adding ride_length calculations in all_trips in seconds
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)


str(all_trips)



#converting ride_length from factor to numeric(so that we run calculations on data)
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)


    
#remove bad data(creating new data frame)
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

#conducting descriptive analysis
#descriptive analysis on ride length
mean(all_trips_v2$ride_length) 
median(all_trips_v2$ride_length) 
max(all_trips_v2$ride_length) 
min(all_trips_v2$ride_length) 

summary(all_trips_v2$ride_length)#we can adopt either the above step or below step



#comparing members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)


# Seeing the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


#fixing days of the week in orderly basis
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


#average ride time by each day for members vs casual riders
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)



# analyzing ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts


#visualising the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")


#visualising average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")





  
  
  








