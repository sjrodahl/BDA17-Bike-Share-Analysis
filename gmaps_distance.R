## Fetching distance from Google Maps
library(readr)
library(gmapsdistance)
setwd("D:/Sondre/Dokumenter/UCSD/IRGN452 BigDataAnalytics/Project/BDA17-Bike-Share-Analysis/")
stations.df <- read.csv("data/station.csv" , stringsAsFactors=FALSE)

gmapsKey <- read_file('gmapsAPIkey.txt')

#####Extract long/lat

stations.lats <-stations.df$lat
stations.longs <- stations.df$long
stations.coordinates <- paste(stations.lats, stations.longs, sep="+")
stations.coordinates.top3 <-head(stations.coordinates, 3)


#### Distance matrix returns a list of 3 data frames: Time (in seconds), Distance (in meters) and Status ("OK")
distance.matrix.coord <- gmapsdistance(origin = stations.coordinates.top3,
                                 destination = stations.coordinates.top3,
                                 key= gmapsKey,
                                 mode="bicycling")

# Import some trips

trips<-read.csv("data/trip.csv", nrows=1000)

# Add ID variable to stations.df
stations.df$ID <- 1:nrow(stations.df)


#
# @param origin: station id of origin station
# @param destination: station id of destination station
# @return matrix: [time, distance] where time is in seconds, distance is in meters
getTimeAndDist<- function(origin, destination){
  O <- stations.df$station_id == origin
  rowIndex <- stations.df[O, ]$ID
  D <- stations.df$station_id == destination
  columnIndex <- stations.df[D, ]$ID + 1
  time <- distance.matrix.coord$Time[rowIndex, columnIndex]
  dist <- distance.matrix.coord$Distance[rowIndex, columnIndex]
  retList <- list(time=time, dist=dist)
  return(retList)
}

time1 <- getTimeAndDist(stations.df$station_id[1], stations.df$station_id[2])
