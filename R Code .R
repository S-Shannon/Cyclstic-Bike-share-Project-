##Import previous 12 years of data

df1<-read_csv("Desktop/Capstone Project 2023/Raw Data/202201-divvy-tripdata.csv")
df2<-read_csv("Desktop/Capstone Project 2023/Raw Data/202202-divvy-tripdata.csv")
df3<-read_csv("Desktop/Capstone Project 2023/Raw Data/202203-divvy-tripdata.csv")
df4<-read_csv("Desktop/Capstone Project 2023/Raw Data/202204-divvy-tripdata.csv")
df5<-read_csv("Desktop/Capstone Project 2023/Raw Data/202205-divvy-tripdata.csv")
df6<-read_csv("Desktop/Capstone Project 2023/Raw Data/202206-divvy-tripdata.csv")
df7<-read_csv("Desktop/Capstone Project 2023/Raw Data/202207-divvy-tripdata.csv")
df8<-read_csv("Desktop/Capstone Project 2023/Raw Data/202208-divvy-tripdata.csv")
df9<-read_csv("Desktop/Capstone Project 2023/Raw Data/202209-divvy-tripdata.csv")
df10<-read_csv("Desktop/Capstone Project 2023/Raw Data/202210-divvy-tripdata.csv")
df11<-read_csv("Desktop/Capstone Project 2023/Raw Data/202211-divvy-tripdata.csv")
df12<-read_csv("Desktop/Capstone Project 2023/Raw Data/202212-divvy-tripdata.csv")

##check data for inconsistencies 

str(df1)
str(df2)
str(df3)
str(df4)
str(df5)
str(df6)
str(df7)
str(df8)
str(df9)
str(df10)
str(df11)
str(df12)

## no inconsistencies, merging all into one dataframe

all_trips <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)

##Removing latitude and longitude rows, they're irrelevant for my analysis

all_trips <- all_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng))

##Inspect Data 

colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  
tail(all_trips) #See the last 6 rows of data frame.  
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

## Clean & prep data for analysis 

all_trips$ended_at <- lubridate::mdy_hm(all_trips$ended_at)
all_trips$started_at <- lubridate::mdy_hm(all_trips$started_at)

failed_rows <- all_trips %>% filter(is.na(started_at) | is.na(ended_at))

all_trips$date <- as.Date(all_trips$started_at) 
all_trips$month <- format(as.Date(all_trips$date), "%m")

all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")

all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
str(all_trips)

if (any(all_trips$ride_length < 0, na.rm = TRUE)) {print("Negative values (including NAs) found in the ride_length column.")} 
else {print("No negative values found in the ride_length column.")}

## Checking for null values 

total_null_count <- sum(is.na(all_trips))

## Remove Null values and create new dataframe 

all_trips_v2 <- all_trips[complete.cases(all_trips), ]

## Conduct a descriptive analysis
## Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) 
median(all_trips_v2$ride_length) 

max(all_trips_v2$ride_length) 
min(all_trips_v2$ride_length)

## Time to compare members vs casual users 

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

## Members vs casual average ride time by day 

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

## days are not in order, going to clean it up 

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

## analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

## Let's visualize the number of rides by rider type

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

## Let's create a visualization for average duration

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

# Create a csv file that we will visualize in Excel, Tableau, or my presentation software

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

write.csv(counts, file = '~/Desktop/Capstone Project 2023/R Cleaned Data/avg_ride_length.csv')








