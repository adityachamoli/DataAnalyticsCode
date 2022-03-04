install.packages("tidyverse")
install.packages("tidyr")
install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("plotrix")
install.packages("janitor")
install.packages("modeest")

library(tidyverse) #package for data manipulation, exploration and visualization
library(tidyr) #package for data cleaning 
library(dplyr) #package for data manipulation
library(readr) #package for importing data
library(ggplot2) #package for applying visual properties
library(plotrix) #package for plotting
library(janitor) #package for examining and cleaning dirty data
library(modeest) #package for finding mode

cyclistic_df <- read_csv("Divvy_Trips_2020_Q1.csv") #reading Cyclistic data from comma seperated values (csv) file

cyclistic_df %>% dim() #dimensions of cyclistic_df dataframe

cyclistic_df %>% colnames() #column names of cyclistic_df dataframe

str(cyclistic_df) #internal structure of cyclistic_df dataframe

glimpse(cyclistic_df) #glimpse of cyclistic_df dataframe

head(cyclistic_df) #first 6 columns of cyclistic_df dataframe

View(cyclistic_df) #viewing cyclistic_df dataframe

cyclistic_df %>% clean_names() #checking cyclistic_df dataframe for clean column names

summary(cyclistic_df) #statistical summary of cyclistic_df dataframe

cyclistic_df <- cyclistic_df %>% mutate(ride_length = ended_at - started_at) #adding ride_length column to cyclistic_df dataframe
cyclistic_df <- cyclistic_df %>% mutate(day_of_week = weekdays(started_at)) #adding day_of_week column to cyclistic_df dataframe
cyclistic_df$ride_length <- abs(cyclistic_df$ride_length) #taking absolute value of ride_length column 

cyclistic_df["member_casual"][cyclistic_df["member_casual"] == "member"] <- "annual" #replacing member value of member_casual column with annual

cyclistic_df <- cyclistic_df %>% arrange(start_station_id,end_station_id) #sorting cyclistic_df dataframe records by start_station_id and end_station_id in ascending order

cyclistic_df <- na.omit(cyclistic_df) #dropping records with NA or NULL values in cyclistic_df dataframe
cyclistic_df <- cyclistic_df[!duplicated(cyclistic_df),] #dropping duplicated records from cyclistic_df dataframe

IQR(cyclistic_df$distance_km) #Interquartile range of distance_km (IQR = Upper Quartile - Lower Quartile)

cyclistic_df %>% dim() #dimension of cyclistic_df dataframe after cleaning the data

cyclistic_df %>% summarize(mean_ride_length = mean(ride_length)) #mean ride_length
cyclistic_df %>% summarize(max_ride_length = max(ride_length)) #maximum ride_length
mfv(cyclistic_df$ride_length) #mode of ride_length

lat1 <- cyclistic_df$start_lat
lng1 <- cyclistic_df$start_lng
lat2 <- cyclistic_df$end_lat
lng2 <- cyclistic_df$end_lng

R <- 6371
a1 <- lat1 * 0.0174
a2 <- lat2 * 0.0174
b1 <- (lat2 - lat1) * 0.0174
b2 <- (lng2 - lng1) * 0.0174
a <- sin(b1/2) * sin(b1/2) + cos(a1) * cos(a2) * sin(b2/2) * sin(b2/2)
b <- 2 * atan2(sqrt(a),sqrt(1-a))
d <- R * b

cyclistic_df <- cyclistic_df %>% mutate(distance_km = d) #adding distance_km column in cyclistic_df dataframe
cyclistic_df$distance_km <- abs(cyclistic_df$distance_km) #taking absolute value of distance_km

cyclistic_df %>% group_by(member_casual) %>% count() #count of casual members and members with annual memberships

cyclistic_df %>% group_by(member_casual) %>% summarize(min_ride_length = min(ride_length),max_ride_length = max(ride_length),mean_ride_length = mean(ride_length)) #minimum, maximum and mean ride_length for two casual_member categories

cyclistic_df %>% group_by(member_casual) %>% summarize(min_distance_km = min(distance_km),max_distance_km = max(distance_km),mean_distance_km = mean(distance_km)) #minimum, maximum and mean distance_km for two casual_member categories
 
cyclistic_df %>% distinct(start_station_id) %>% count() #count of distinct starting stations
cyclistic_df %>% distinct(end_station_id) %>% count() #count of distinct ending stations

cyclistic_df %>% group_by(start_station_name) %>% count() #count of records from each starting station
cyclistic_df %>% group_by(end_station_name) %>% count() #count of records from each ending station

df1 <- cyclistic_df %>% group_by(start_station_name) %>% summarise(mean_ride_length = mean(ride_length))
df1 %>% summarize(max_mean_ride_length = max(mean_ride_length))
df1 %>% filter(mean_ride_length == 2693828) #busiest starting station

df2 <- cyclistic_df %>% group_by(end_station_name) %>% summarise(mean_ride_length = mean(ride_length))
df2 %>% summarize(max_mean_ride_length = max(mean_ride_length))
df2 %>% filter(mean_ride_length == 857866.5) #busiest ending station

cyclistic_df %>% group_by(day_of_week) %>% count() #count of records from each day_of_week

cyclistic_df %>% group_by(day_of_week) %>% summarise(mean_ride_length = mean(ride_length)) #mean ride_length value for each day_of_week 
cyclistic_df %>% group_by(day_of_week) %>% summarise(mean_distance_km = mean(distance_km)) #mean distance_km value for each day_of_week

rl <- as.numeric(cyclistic_df$ride_length)
dkm <- cyclistic_df$distance_km

cov(rl,dkm) #covariance value between ride_length and distance_km
cor(rl,dkm) #cofficient of correlation value between ride_length and distance_km
