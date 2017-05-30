
library(ggplot2)
library(caret)
library(tidyr)
library(dplyr)
library(lattice)
library(lubridate)
library(ggmap) #remember to cite ggmap
library(timeDate)
library(chron)
library(plyr)


#Comment out the other user.
setwd("/Users/tormagnusmichaelsen/Documents/BDA/Project/BDA17-Bike-Share-Analysis")
#setwd("D:/Sondre/Dokumenter/UCSD/IRGN452 BigDataAnalytics/Project/BDA17-Bike-Share-Analysis/")

trips<-read.csv("data/trip.csv") #Set nrows for testing purposes
weather<-read.csv("data/weather.csv")
stations<-read.csv("data/station.csv")

distance.matrix <- read.csv("gmapsData/gmapsDistMatrix.csv")
time.matrix <- read.csv("gmapsData/gmapsTimeMatrix.csv")

#Remove rows where station id is not in station matrix
trips<-trips[(trips$from_station_id %in% stations$station_id & trips$to_station_id %in% stations$station_id ),]
#Transform time data to dates, first to characters

#-----------convert to characters----------- (don't need if stringsAsFactors = FALSE)
trips$startDate<- as.character(trips$starttime) #to characters
trips$endDate<- as.character(trips$stoptime) #to characters

#------------add startdate and enddate as POSIXct
trips$startDate <- as.POSIXct(trips$startDate,format="%m/%d/%Y %H:%M") #to dates
trips$endDate <- as.POSIXct(trips$endDate,format="%m/%d/%Y %H:%M") #to dates
#now format = Y-m-d H:M:S

#------------add onlySTime-------
trips$onlySTime<-strftime(trips$startDate, format="%H:%M:%S")
trips$onlySTime<-as.POSIXct((trips$startDate), format="%H:%M:%S")
trips$onlySTime<-format(trips$onlySTime, "%H:%M:%S")


#------------add day of week, from startDate
trips$sDay<- wday(trips$startDate, label = TRUE)
trips$eDay<- wday(trips$endDate, label = TRUE)

#------------add month, from startDate
trips$sMonth<-month(trips$startDate, label = TRUE)
trips$eMonth<-month(trips$endDate, label = TRUE)

#------------add year, from startDate
trips$sYear<-year(trips$startDate)
trips$eYear<-year(trips$endDate)

#------------add seconds since jan 1 1970, from startDate
trips$sSeconds<-as.numeric(trips$startDate)
trips$eSeconds<-as.numeric(trips$endDate)

#-------------add hour from startDate
trips$sHour<-hour(trips$startDate)
trips$eHour<-hour(trips$endDate)

#-----------add minute from startDate
trips$sMinute<-minute(trips$startDate)
trips$eMinute<-minute(trips$endDate)

#---------add holiday and weekday as false/true from startDate
trips$weekday<-isWeekday(trips$startDate,wday = 1:5)
trips$holiday<-is.holiday(trips$startDate)


#-----------add onlyDate as factorized version of date, to use in comparison with weather data. Removes hour and minute mark

trips$onlyDate<-as.Date(trips$startDate,tz = "PST8PDT")
#trips$onlyDate<-factor(trips$onlyDate)


#------------add age in years for users
trips$age<-2017 - trips$birthyear


#--------------merge weatherdata into trips-------
weather$Date<-factor(weather$Date)
weather$onlyDate <- as.Date(as.POSIXct(weather$Date,format="%m/%d/%Y",tz = "PST8PDT")) #to dates
trips<-merge(trips,weather, by.y = "onlyDate", by.x = "onlyDate")

#-------------merge stationcoordinates into trips---------
#syntax table1$val2 <- table2$val2[match(table1$pid, table2$pid)]

trips$from_long<-as.factor(stations$long[match(trips$from_station_id,stations$station_id)])
trips$from_lat<-as.factor(stations$lat[match(trips$from_station_id,stations$station_id)])

trips$to_long<-as.factor(stations$long[match(trips$to_station_id,stations$station_id)])
trips$to_lat<-as.factor(stations$lat[match(trips$to_station_id,stations$station_id)])

#--------generate new array to store all combinations of trips with coordinates
#first create matrix with all possible combinations, then add coordinates to this

tripcombs<-combn(stations$station_id, 2)
tripcombs<-t(tripcombs)
tripcombs<-as.data.frame.matrix(tripcombs)


tripcombs$from_long<-stations$long[match(tripcombs$V1,stations$station_id)]
tripcombs$from_lat<-stations$lat[match(tripcombs$V1,stations$station_id)]

tripcombs$to_long<-stations$long[match(tripcombs$V2,stations$station_id)]
tripcombs$to_lat<-stations$lat[match(tripcombs$V2,stations$station_id)]




#-----------Relations between weather and trips-------
#need to compare factors in weather$date and 
#trips$date, and plot number of trips under different weatherconditions, and 
#only compare weekdays with weekdays and weekends with weekends

#need to defactorize weather$date first

weather$Date<-(as.character(weather$Date))
weather$Date<-(as.Date(weather$Date,format = "%m/%d/%Y"))

# Add ID variable to stations
stations$ID <- 1:nrow(stations)

#
# @param origin: station id of origin station
# @param destination: station id of destination station
# @return time: time in seconds, distance is in meters
getGoogleInfo<- function(origin, destination, df){
  O <- stations$station_id == origin
  rowIndex <- stations[O, ]$ID
  D <- stations$station_id == destination
  columnIndex <- stations[D, ]$ID + 2
  res <- df[rowIndex, columnIndex]
  return(res)
}


#-------------Predict number of a usergroup at a given station in a timerange--------
#first need to create an array with time-intervals, 24hours, factorize
#then create new variable, and graph by that variable

#could also be intersting to look into when people rent bicycles (after whole hours)

#---------By agegroups-------
trips$Agecat1<-cut(trips$age, c(19,25,35,45,55,63,81), right = FALSE)


#Add time and distance columns to dataset
trips[,"timeEst"] <- apply(trips[,c("from_station_id", "to_station_id")], 1, function(x) getGoogleInfo(x[1],x[2], time.matrix))
trips[,"distanceEst"] <- apply(trips[,c("from_station_id", "to_station_id")], 1, function(x) getGoogleInfo(x[1],x[2], distance.matrix))

trips[,"timeDiff"] <- trips$tripduration-trips$timeEst

#dataprep for eda
#need library(plyr) for this, conflicting with "lubridate"
library(plyr)
from_stations<-count(trips, c('from_long', 'from_lat'))

to_stations<-count(trips, c('to_long', 'to_lat'))
names(from_stations)[3]<-"departures"

from_stations$from_long<-as.numeric(as.character(from_stations$from_long))
from_stations$from_lat<-as.numeric(as.character(from_stations$from_lat))

from_stations$departures<-as.numeric(from_stations$departures)
#end dataprep for eda


write.csv(trips, "data/trips_processed.csv")
write.csv(weather, "data/weather_processed2.csv")






