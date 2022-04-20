# Reads in csv
taxi <- read.csv("Taxi_Trips_-_2019.csv")
head(taxi)

#Gets rid of unneeded columns
reduce_col <- taxi[c(3,5,6,9,10,17)]
head(reduce_col)

#Get rid of very short trips and very long trips
reduce_miles <- reduce_col[which(reduce_col$Trip.Miles >= 0.5 & reduce_col$Trip.Miles <= 100),]
head(reduce_miles)

#Get rid of very short time and long time trips
reduce_time <- reduce_miles[which(reduce_miles$Trip.Seconds >= 60 & reduce_miles$Trip.Seconds <= 18000),]
head(reduce_time)

#Get rid of any with NA community area
reduce_area <- na.omit(reduce_time)

summary(reduce_area)
tail(reduce_area)

remove(reduce_col)
remove(reduce_miles)
remove(reduce_time)

#Added New Date, Date, and Hour column
library(lubridate)
n_date <- parse_date_time(reduce_area$Trip.Start.Timestamp,
                orders = 'mdY IMS %p', truncated = 3) #PARSE DATE FROM TIME STAMP
reduce_area$new_date <- n_date


#ADD COLUMNS FOR DATE AND HOUR
reduce_area$Date <- date(reduce_area$new_date)
reduce_area$Hour <- hour(reduce_area$new_date)

data <- reduce_area[1:6]
head(data)

write.csv(data, "modified_taxi.csv")

#Split file into 7 files?
library(NCmisc)
setwd("/Users/waynekao/CS424/Project 3/Data/")
file.split("modified_taxi.csv", size = 700000, verbose = TRUE,suf = "part", win = TRUE)


#Read files?
