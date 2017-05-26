## Adding distance and time estimates from google distance matrix

setwd("D:/Sondre/Dokumenter/UCSD/IRGN452 BigDataAnalytics/Project/BDA17-Bike-Share-Analysis/")

stations.df <- read.csv('data/station.csv')
distance.matrix <- read.csv("gmapsData/gmapsDistMatrix.csv")
time.matrix <- read.csv("gmapsData/gmapsTimeMatrix.csv")
trips <- read.csv("data/trip.csv", nrows = 1000)

# Add ID variable to stations.df
stations.df$ID <- 1:nrow(stations.df)

#
# @param origin: station id of origin station
# @param destination: station id of destination station
# @return time: time in seconds, distance is in meters
getGoogleInfo<- function(origin, destination, df){
  O <- stations.df$station_id == origin
  rowIndex <- stations.df[O, ]$ID
  D <- stations.df$station_id == destination
  columnIndex <- stations.df[D, ]$ID + 2
  res <- df[rowIndex, columnIndex]
  return(res)
}

trips[,"timeEst"] <- apply(trips[,c("from_station_id", "to_station_id")], 1, function(x) getGoogleInfo(x[1],x[2], time.matrix))
trips[,"distanceEst"] <- apply(trips[,c("from_station_id", "to_station_id")], 1, function(x) getGoogleInfo(x[1],x[2], distance.matrix))

