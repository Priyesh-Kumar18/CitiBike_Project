#Step 1: Prepare data, install and load all packages
#____________________
#Install packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("janitor")

#load packages
library(tidyverse)
library(lubridate)
library(janitor)
library(dplyr)
library(ggplot2)

#Import data and rename it
t1 <- read.csv("C:\\Users\\flyso\\Desktop\\Trips\\202201-divvy-tripdata.csv")
t2 <- read.csv("C:\\Users\\flyso\\Desktop\\Trips\\202202-divvy-tripdata.csv")
t3 <- read.csv("C:\\Users\\flyso\\Desktop\\Trips\\202203-divvy-tripdata.csv")
t4 <- read.csv("C:\\Users\\flyso\\Desktop\\Trips\\202204-divvy-tripdata.csv")
t5 <- read.csv("C:\\Users\\flyso\\Desktop\\Trips\\202205-divvy-tripdata.csv")
t6 <- read.csv("C:\\Users\\flyso\\Desktop\\Trips\\202206-divvy-tripdata.csv")
t7 <- read.csv("C:\\Users\\flyso\\Desktop\\Trips\\202207-divvy-tripdata.csv")
t8 <- read.csv("C:\\Users\\flyso\\Desktop\\Trips\\202208-divvy-tripdata.csv")
t9 <- read.csv("C:\\Users\\flyso\\Desktop\\Trips\\202209-divvy-publictripdata.csv")
t10 <- read.csv("C:\\Users\\flyso\\Desktop\\Trips\\202210-divvy-tripdata.csv")
t11 <- read.csv("C:\\Users\\flyso\\Desktop\\Trips\\202211-divvy-tripdata.csv")
t12 <- read.csv("C:\\Users\\flyso\\Desktop\\Trips\\202212-divvy-tripdata.csv")

#Step 2: Processs and Clean Data
#--------------------
#Merge merge 12 files into 1 file, remove empty rows/columns
Trips2022 <- rbind(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)
Trips2022 <- janitor::remove_empty(dat = Trips2022,which = c("cols"))
Trips2022 <- janitor::remove_empty(dat = Trips2022,which = c("rows"))

#View data to double check everything
colnames(Trips2022)
nrow(Trips2022)
dim(Trips2022)
head(Trips2022)
str(Trips2022)
summary(Trips2022)

#Convert Date/Time stamp to Date/Time
Trips2022$started_at <- lubridate::as_datetime(Trips2022$started_at)
Trips2022$ended_at <- lubridate::as_datetime(Trips2022$ended_at)

#Get start and end Hours
Trips2022$start_hr <- lubridate::hour(Trips2022$started_at)
Trips2022$end_hr <- lubridate::hour(Trips2022$ended_at)

#Get start and end date
Trips2022$start_date <- lubridate::date(Trips2022$started_at)
Trips2022$end_date <- lubridate::date(Trips2022$ended_at)

#Add day of the week column
Trips2022$day_of_week <- format(as.Date(Trips2022$start_date), "%A")

#Calculate Ride Length
Trips2022$ride_length_Min <- difftime(Trips2022$ended_at,Trips2022$started_at,units = c("mins"))
Trips2022$ride_length_Min <- as.numeric(Trips2022$ride_length_Min)

#Remove Ride lengths less than or equal to 0 / Make a new table for all clean data
Trips2022_cl <- Trips2022[!(Trips2022$start_station_name == "HQ QR" | Trips2022$ride_length<0),]

#make a new csv file with all the clean data
write.csv(Trips2022_cl, "Tripsroject.csv", row.names = FALSE)

#Step 3: Analyze
#--------------------
#calcualte mean, median, max, min by members and causal
aggregate(Trips2022_cl$ride_length ~ Trips2022_cl$member_casual, FUN = mean)
aggregate(Trips2022_cl$ride_length ~ Trips2022_cl$member_casual, FUN = median)
aggregate(Trips2022_cl$ride_length ~ Trips2022_cl$member_casual, FUN = max)
aggregate(Trips2022_cl$ride_length ~ Trips2022_cl$member_casual, FUN = min)

#Order day of the week in new dataset
Trips2022_cl$day_of_week <- ordered(Trips2022_cl$day_of_week, levels=c("Sunday", "Monday", "Tuesday",
                                                                     "Wednesday", "Thursday", "Friday", "Saturday"))

#calculate average
aggregate(Trips2022_cl$ride_length_Min ~ Trips2022_cl$member_casual +
  Trips2022_cl$day_of_week, FUN = mean)

#calculate average duration sorted by rider type, then day of the week
Trips2022_cl %>%
  mutate(day_of_week = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, day_of_week ) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()       #calculates the number of rides and average duration
    ,average_duration = mean(ride_length_Min)) %>%      # calculates the average duration
  arrange(member_casual, day_of_week)