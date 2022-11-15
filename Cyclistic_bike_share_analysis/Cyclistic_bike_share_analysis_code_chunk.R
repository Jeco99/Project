# Install the library to use
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("janitor")
#install.packages("lubridate")

# Load for libraries
library(tidyverse)
library(ggplot2)
library(janitor) # clean the data
library(lubridate)

#avoiding the scientific notation
options(scipen=999)

#read the data
df1 <- read.csv("D:/R/Cyclistic_bike_share_analysis/dataset/202111-divvy-tripdata.csv")
df2 <- read.csv("D:/R/Cyclistic_bike_share_analysis/dataset/202112-divvy-tripdata.csv")
df3 <- read.csv("D:/R/Cyclistic_bike_share_analysis/dataset/202201-divvy-tripdata.csv")
df4 <- read.csv("D:/R/Cyclistic_bike_share_analysis/dataset/202202-divvy-tripdata.csv")
df5 <- read.csv("D:/R/Cyclistic_bike_share_analysis/dataset/202203-divvy-tripdata.csv")
df6 <- read.csv("D:/R/Cyclistic_bike_share_analysis/dataset/202204-divvy-tripdata.csv")
df7 <- read.csv("D:/R/Cyclistic_bike_share_analysis/dataset/202205-divvy-tripdata.csv")
df8 <- read.csv("D:/R/Cyclistic_bike_share_analysis/dataset/202206-divvy-tripdata.csv")
df9 <- read.csv("D:/R/Cyclistic_bike_share_analysis/dataset/202207-divvy-tripdata.csv")
df10 <- read.csv("D:/R/Cyclistic_bike_share_analysis/dataset/202208-divvy-tripdata.csv")
df11 <- read.csv("D:/R/Cyclistic_bike_share_analysis/dataset/202209-divvy-publictripdata.csv")
df12 <- read.csv("D:/R/Cyclistic_bike_share_analysis/dataset/202210-divvy-tripdata.csv")

#Need to know the number of column in each data so I can merge them all.
#dim(df1)
#dim(df2)
#dim(df3)
#dim(df4)
#dim(df5)
#dim(df6)
#dim(df7)
#dim(df8)
#dim(df9)
#dim(df10)
#dim(df11)
#dim(df12)

#Merging all the data
df <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)

#creating a copy
cyclistic <- df

#summary of cyclistic
summary(cyclistic)

#removing the NA
cyclistic <- na.omit(cyclistic)
# removing the duplicate rows
cyclistic <- distinct(cyclistic)

#extracting the month, day , year in the started_at column
cyclistic$date <- as.POSIXct(cyclistic$started_at, format = "%Y-%m-%d %H:%M:%S")
cyclistic$year <- format(cyclistic$date, format = "%Y")
cyclistic$month <- format(cyclistic$date, format = "%m")
cyclistic$day <- format(cyclistic$date, format = "%d")
cyclistic$hour <- format(cyclistic$date, format = "%H")
cyclistic$ride_length <- difftime(cyclistic$ended_at, cyclistic$started_at, units = "mins")
cyclistic$weekdays <- strftime(cyclistic$date, "%A") # convert to word days

cyclistic <- cyclistic %>% mutate(month_name = case_when(
              month == "11" ~ "November",
              month == "12" ~ "December",
              month == "01" ~ "January",
              month == "02" ~ "February",
              month == "03" ~ "March",
              month == "04" ~ "April",
              month == "05" ~ "May",
              month == "06" ~ "June", 
              month == "07" ~ "July",
              month == "08" ~ "August", 
              month == "09" ~ "September",
              month == "10" ~ "October"
))

cyclistic <- cyclistic %>% mutate(season = case_when(
              month == "11" ~ "Fall",
              month == "12" ~ "Winter",
              month == "01" ~ "Winter",
              month == "02" ~ "Winter",
              month == "03" ~ "Spring",
              month == "04" ~ "Spring",
              month == "05" ~ "Spring",
              month == "06" ~ "Summer", 
              month == "07" ~ "Summer",
              month == "08" ~ "Summer", 
              month == "09" ~ "Fall",
              month == "10" ~ "Fall"
))

cyclistic <- cyclistic %>% mutate(time_day = case_when(
             hour == "00" ~ "Night",
             hour == "01" ~ "Night",
             hour == "02" ~ "Night",
             hour == "03" ~ "Night",
             hour == "04" ~ "Night",
             hour == "05" ~ "Night",
             hour == "06" ~ "Morning",
             hour == "07" ~ "Morning",
             hour == "08" ~ "Morning",
             hour == "09" ~ "Morning",
             hour == "10" ~ "Morning",
             hour == "11" ~ "Morning",
             hour == "12" ~ "Afternoon",
             hour == "13" ~ "Afternoon",
             hour == "14" ~ "Afternoon",
             hour == "15" ~ "Afternoon",
             hour == "16" ~ "Afternoon",
             hour == "17" ~ "Afternoon",
             hour == "18" ~ "Evening",
             hour == "19" ~ "Evening",
             hour == "20" ~ "Evening",
             hour == "21" ~ "Evening",
             hour == "22" ~ "Evening",
             hour == "23" ~ "Evening"
))


#dropping the unnecessary columns
cyclistic <- cyclistic %>% select(-c(ride_id, month, day, 
                                     date, hour, started_at, 
                                     ended_at, start_station_name,
                                     start_station_id, end_station_name,
                                     end_station_id, start_lat, start_lng, 
                                     end_lat, end_lng))
head(cyclistic)

cyclistic_clean <- cyclistic

# calculate the mean of ride_length
cyclistic_avgRide <- mean(cyclistic_clean$ride_length)
cyclistic_avgRide

# calculate the max ride_length
cyclistic_maxRide <- max(cyclistic_clean$ride_length)
cyclistic_maxRide


# calculate the average ride_length for each members and casual riders
cyclistic_avgRide_byMemberCasual <- cyclistic_clean %>% 
            group_by(member_casual) %>% 
            summarise_at(vars(ride_length), mean)
cyclistic_avgRide_byMemberCasual

mean(cyclistic_avgRide_byMemberCasual$ride_length)

#Count based on the rideable type
rideableType_count <- cyclistic_clean %>% group_by(rideable_type) %>% count()
rideableType_count

mean(rideableType_count$n)

#Group the member and casual on the rideable type the calculate the average of ride length
casualCount <- cyclistic_clean %>% 
  group_by(rideable_type) %>% 
  filter(member_casual == "casual") %>%
  summarise_at(vars(ride_length), mean)
casualCount

mean(casualCount$ride_length)

memberCount <- cyclistic_clean %>% 
  group_by(rideable_type) %>% 
  filter(member_casual == "member") %>%
  summarise_at(vars(ride_length), mean)
memberCount

mean(memberCount$ride_length)


#Calculate the total rides by the member and casual
memberCasual_Count <- cyclistic_clean %>% group_by(member_casual) %>% count()
memberCasual_Count

mean(memberCasual_Count$n)



#Season Calculation


#What is the average ride length during each of season?
avgRideLength_season <- cyclistic_clean %>% 
  group_by(season) %>% 
  summarise_at(vars(ride_length), mean)
avgRideLength_season

mean(avgRideLength_season$ride_length)

seasonCasual <- cyclistic_clean %>% 
  group_by(season) %>%
  filter(member_casual == "casual") %>%
  summarise_at(vars(ride_length), mean)
seasonCasual

mean(seasonCasual$ride_length)

seasonMember <- cyclistic_clean %>% 
  group_by(season) %>%
  filter(member_casual == "member") %>%
  summarise_at(vars(ride_length), mean)
seasonMember

mean(seasonMember$ride_length)

season_Avg <- data.frame(seasonCasual, seasonMember$ride_length)
season_Avg <- season_Avg %>% rename(
  "season" = season,
  "casual_avg_rideLength" = ride_length,
  "member_avg_rideLength" = seasonMember.ride_length
)

season_Avg


#Count the total rides based on the season
seasonCount <- cyclistic_clean %>% 
  group_by(season) %>%
  count()

seasonCount

mean(seasonCount$n)

#Count the total rides based on the season and group by casual_member

#Spring
springCount <- cyclistic_clean %>% 
  group_by(member_casual) %>%
  filter(season == "Spring") %>%
  count()
springCount

#Fall
fallCount <- cyclistic_clean %>% 
  group_by(member_casual) %>%
  filter(season == "Fall") %>%
  count()
fallCount

#Summer
summerCount <- cyclistic_clean %>% 
  group_by(member_casual) %>%
  filter(season == "Summer") %>%
  count()
summerCount

#Winter
winterCount <- cyclistic_clean %>% 
  group_by(member_casual) %>%
  filter(season == "Winter") %>%
  count()
winterCount


#Make a pivot table for total ride of each season by member_casual
season_df <- data.frame(springCount$member_casual, 
                        springCount$n, fallCount$n,
                        summerCount$n, winterCount$n)
season_df <- season_df %>% 
  rename("Member_Casual" = springCount.member_casual,
         "Spring" = springCount.n,
         "Fall" = fallCount.n,
         "Summer" = summerCount.n,
         "Winter" = winterCount.n)

season_df <- season_df %>% mutate( total = rowSums(season_df[2:5]),
                                   meanRow = rowMeans(season_df[2:5]))
season_df

mean(season_df$Spring)
mean(season_df$Fall)
mean(season_df$Summer)
mean(season_df$Winter)


# Time of Day Calculation

#What is the average ride length during each of time_day?
avgRideLength_timeDay <- cyclistic_clean %>% 
  group_by(time_day) %>% 
  summarise_at(vars(ride_length), mean)
avgRideLength_timeDay

mean(avgRideLength_timeDay$ride_length)

timeDay_casual <- cyclistic_clean %>% 
  group_by(time_day) %>%
  filter(member_casual == "casual") %>%
  summarise_at(vars(ride_length), mean)
timeDay_casual

mean(timeDay_casual$ride_length)

timeDay_member <- cyclistic_clean %>% 
  group_by(time_day) %>%
  filter(member_casual == "member") %>%
  summarise_at(vars(ride_length), mean)
timeDay_member

mean(timeDay_member$ride_length)

timeDay_Avg <- data.frame(timeDay_casual, timeDay_member$ride_length)
timeDay_Avg <- timeDay_Avg %>% rename(
        "Time of day" = time_day,
        "casual_avg_rideLength" = ride_length,
        "member_avg_rideLength" = timeDay_member.ride_length
)
timeDay_Avg

#Count the total rides based on the time of day 
timeday_Count <- cyclistic_clean %>% group_by(time_day) %>% count()
timeday_Count

mean(timeday_Count$n)


#Count the total rides based on the time of day and group by casual_member
#Night
nightCount <- cyclistic_clean %>% 
  group_by(member_casual) %>%
  filter(time_day == "Night") %>%
  count()
nightCount

#Evening
eveningCount <- cyclistic_clean %>% 
  group_by(member_casual) %>%
  filter(time_day == "Evening") %>%
  count()
eveningCount

#Morning
morningCount <- cyclistic_clean %>% 
  group_by(member_casual) %>%
  filter(time_day == "Morning") %>%
  count()
morningCount

#Afternoon
afternoonCount <- cyclistic_clean %>% 
  group_by(member_casual) %>%
  filter(time_day == "Afternoon") %>%
  count()
afternoonCount

#Make a pivot table for total ride of each time of day by member_casual
timeDay_df <- data.frame(nightCount$member_casual,nightCount$n, morningCount$n, afternoonCount$n, eveningCount$n)

timeDay_df <- timeDay_df %>% 
  rename("Member_Casual" = nightCount.member_casual,
         "Night" = nightCount.n,
         "Morning" = morningCount.n,
         "Afternoon" = afternoonCount.n,
         "Evening" = eveningCount.n)

timeDay_df <- timeDay_df %>% mutate( total = rowSums(timeDay_df[2:5]),
                                     meanRow = rowMeans(timeDay_df[2:5]))
timeDay_df

mean(timeDay_df$Night)
mean(timeDay_df$Morning)
mean(timeDay_df$Afternoon)
mean(timeDay_df$Evening)


# Month Calculation

#What is the average ride length during each of month?
month_cyclistic <- cyclistic_clean %>% 
  group_by(month_name) %>% 
  summarise_at(vars(ride_length), mean)

mean(month_cyclistic$ride_length)

month_fact <- month_cyclistic %>%
  mutate(
    Month = factor(month_name, levels = month.name)
  ) %>%
  arrange(Month)

month_fact <- month_fact %>% select(-c(Month))
month_fact

month_Casual <- cyclistic_clean %>% 
  group_by(month_name) %>% 
  filter(member_casual == "casual") %>%
  summarise_at(vars(ride_length), mean)

month_Casual <- month_Casual %>%
  mutate(
    Month = factor(month_name, levels = month.name)
  ) %>%
  arrange(Month)

month_Casual <- month_Casual %>% select(-c(Month))
month_Casual

month_Member <- cyclistic_clean %>% 
  group_by(month_name) %>% 
  filter(member_casual == "member") %>%
  summarise_at(vars(ride_length), mean)

month_Member <- month_Member %>%
  mutate(
    Month = factor(month_name, levels = month.name)
  ) %>%
  arrange(Month)

month_Member <- month_Member %>% select(-c(Month))
month_Member

monthAvg <- data.frame(month_Casual, month_Member$ride_length)
monthAvg <- monthAvg %>% rename(
  "Month" = month_name,
  "casual_avg_rideLength" = ride_length,
  "member_avg_rideLength" = month_Member.ride_length
)
monthAvg

mean(monthAvg$casual_avg_rideLength)
mean(monthAvg$member_avg_rideLength)


#Count the total rides based on the month
month_total_ride <- cyclistic_clean %>% group_by(month_name) %>% count()

mean(month_total_ride$n)

month_total <- month_total_ride %>%
  mutate(
    Month = factor(month_name, levels = month.name)
  ) %>%
  arrange(Month)

month_totalRide <- month_total %>% select(-c(Month))
month_totalRide

mean(month_totalRide$n)

#Count the total rides based on the month and group by casual_member
januaryCount <- cyclistic_clean %>% group_by(member_casual) %>% 
  filter(month_name == "January") %>%
  count()

januaryCount

februaryCount <- cyclistic_clean %>% group_by(member_casual) %>% 
  filter(month_name == "February") %>%
  count()
februaryCount

marchCount <- cyclistic_clean %>% group_by(member_casual) %>% 
  filter(month_name == "March") %>%
  count()
marchCount

aprilCount <- cyclistic_clean %>% group_by(member_casual) %>% 
  filter(month_name == "April") %>%
  count()
aprilCount

mayCount <- cyclistic_clean %>% group_by(member_casual) %>% 
  filter(month_name == "May") %>%
  count()
mayCount

juneCount <- cyclistic_clean %>% group_by(member_casual) %>% 
  filter(month_name == "June") %>%
  count()
juneCount

julyCount <- cyclistic_clean %>% group_by(member_casual) %>% 
  filter(month_name == "July") %>%
  count()
julyCount

augustCount <- cyclistic_clean %>% group_by(member_casual) %>% 
  filter(month_name == "August") %>%
  count()
augustCount

septemberCount <- cyclistic_clean %>% group_by(member_casual) %>% 
  filter(month_name == "September") %>%
  count()
septemberCount

octoberCount <- cyclistic_clean %>% group_by(member_casual) %>% 
  filter(month_name == "October") %>%
  count()
octoberCount

novemberCount <- cyclistic_clean %>% group_by(member_casual) %>% 
  filter(month_name == "November") %>%
  count()
novemberCount

decemberCount <- cyclistic_clean %>% group_by(member_casual) %>% 
  filter(month_name == "December") %>%
  count()
decemberCount

#Make a pivot table for total ride of each month by member_casual
month_df <- data.frame(januaryCount$member_casual, 
                        januaryCount$n, februaryCount$n,
                        marchCount$n,aprilCount$n,
                        mayCount$n, juneCount$n, julyCount$n,
                        augustCount$n, septemberCount$n, octoberCount$n,
                        novemberCount$n, decemberCount$n)
month_df <-month_df %>% 
  rename("Member_Casual" = januaryCount.member_casual,
         "January" = januaryCount.n,
         "February" = februaryCount.n,
         "March" = marchCount.n,
         "April" = aprilCount.n,
         "May" = mayCount.n, 
         "June" = juneCount.n, 
         "July" = julyCount.n,
         "August" = augustCount.n,
         "September" = septemberCount.n,
         "October" = octoberCount.n,
         "November" = novemberCount.n, 
         "December" = decemberCount.n)

month_df <- month_df %>% mutate( total = rowSums(season_df[2:5]),
                                   meanRow = rowMeans(season_df[2:5]))
month_df

mean(month_df$January)
mean(month_df$February)
mean(month_df$March)
mean(month_df$April)
mean(month_df$May)
mean(month_df$June)
mean(month_df$July)
mean(month_df$August)
mean(month_df$September)
mean(month_df$October)
mean(month_df$November)
mean(month_df$December)

# Day of week Calculation

# calculate the average ride_length for users by day_of_week
week.name <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
               "Friday", "Saturday", "Sunday")
week_RideLength <- cyclistic_clean %>% 
  group_by(weekdays) %>% 
  summarise_at(vars(ride_length), mean)
week_RideLength

week_avg_RideLength <- week_RideLength %>%
  mutate(
    Weekday = factor(weekdays, levels = week.name),
  ) %>%
  arrange(Weekday)
week_avg_RideLength

week_avg <- week_avg_RideLength %>% select(-c(Weekday))
week_avg

mean(week_avg$ride_length)

casualWeekday <- cyclistic_clean %>% 
  group_by(weekdays) %>%
  filter(member_casual == "casual") %>%
  summarise_at(vars(ride_length), mean) 

casualWeekday 


memberWeekday <- cyclistic_clean %>% 
  group_by(weekdays) %>%
  filter(member_casual == "member") %>%
  summarise_at(vars(ride_length), mean)

memberWeekday

weekdayAvg <- data.frame(casualWeekday, memberWeekday$ride_length)
weekdayAvg <- weekdayAvg %>% rename(
  "Weekdays" = weekdays,
  "casual_avg_rideLength" = ride_length,
  "member_avg_rideLength" = memberWeekday.ride_length
)

weekdayAvg

mean(weekdayAvg$casual_avg_rideLength)
mean(weekdayAvg$member_avg_rideLength)

#Count the total rides based on the day of weeks
week.name <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
               "Friday", "Saturday", "Sunday")
week_count <- cyclistic_clean %>% group_by(weekdays) %>% count()

week_count <- week_count %>%
  mutate(
    Weekday = factor(weekdays, levels = week.name)
  ) %>%
  arrange(Weekday)

weekCount_total <- week_count %>% select(-c(Weekday))
weekCount_total


#Count the total rides based on the day of weeks by casual_member
sundayCount <- cyclistic_clean %>% group_by(member_casual) %>% 
  filter(weekdays == "Sunday") %>%
  count()
sundayCount

mondayCount <- cyclistic_clean %>% group_by(member_casual) %>% 
  filter(weekdays == "Monday") %>%
  count()
mondayCount

tuesdayCount <- cyclistic_clean %>% group_by(member_casual) %>% 
  filter(weekdays == "Tuesday") %>%
  count()
tuesdayCount

wednesdayCount <- cyclistic_clean %>% group_by(member_casual) %>% 
  filter(weekdays == "Wednesday") %>%
  count()
wednesdayCount

thursdayCount <- cyclistic_clean %>% group_by(member_casual) %>% 
  filter(weekdays == "Thursday") %>%
  count()
thursdayCount

fridayCount <- cyclistic_clean %>% group_by(member_casual) %>% 
  filter(weekdays == "Friday") %>%
  count()
fridayCount

saturdayCount <- cyclistic_clean %>% group_by(member_casual) %>% 
  filter(weekdays == "Saturday") %>%
  count()
saturdayCount

#Make a pivot table for total ride of each day of week by member_casual
week_df <- data.frame(sundayCount$member_casual, 
                       sundayCount$n,
                       mondayCount$n,
                       tuesdayCount$n,
                       wednesdayCount$n,
                       thursdayCount$n,
                      fridayCount$n,
                       saturdayCount$n)
week_df <-week_df %>% 
  rename("Member_Casual" = sundayCount.member_casual,
         "Sunday" = sundayCount.n,
         "Monday" = mondayCount.n,
         "Tuesday" = tuesdayCount.n,
         "Wednesday" = wednesdayCount.n,
         "Thursday" = thursdayCount.n,
         "Friday" = fridayCount.n,
         "Saturday" = saturdayCount.n
         )

week_df <- week_df %>% mutate( total = rowSums(season_df[2:5]),
                                 meanRow = rowMeans(season_df[2:5]))
week_df

mean(week_df$Sunday)
mean(week_df$Monday)
mean(week_df$Tuesday)
mean(week_df$Wednesday)
mean(week_df$Thursday)
mean(week_df$Friday)
mean(week_df$Saturday)

write.csv(cyclistic_clean, file = "cyclistic_clean.csv")
