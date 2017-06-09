## Fetching distance from Google Maps

### This script does not need to be run again. Results are stored in csv-files

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
distance.matrix.walk.coord <- gmapsdistance(origin = stations.coordinates,
                                 destination = stations.coordinates,
                                 key= gmapsKey,
                                 mode="walking")


#Write csv-file
write.csv(distance.matrix.coord, file = 'distanceMatrix.csv')

#write csv file for each dataframe

write.csv(distance.matrix.coord$Time, file = 'gmapsTimeMatrix.csv')
write.csv(distance.matrix.coord$Distance, file = 'gmapsDistMatrix.csv')
write.csv(distance.matrix.coord$Status, file = 'gmapsStatusMatrix.csv')
# Import some trips

#trips<-read.csv("data/trip.csv", nrows=1000)




#time1 <- getTimeAndDist(stations.df$station_id[1], stations.df$station_id[2])
