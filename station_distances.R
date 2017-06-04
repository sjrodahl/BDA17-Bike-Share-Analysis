library(dplyr)
library(ggmap)
library(leaflet)
limit = 400

setwd("D:/Sondre/Dokumenter/UCSD/IRGN452 BigDataAnalytics/Project/BDA17-Bike-Share-Analysis/")
trips <- read.csv("data/trip.csv", stringsAsFactors = FALSE, sep = ";")
stations<-read.csv("data/station.csv")
#Remove trips that are not from standard stations:
trips<-trips[(trips$from_station_id %in% stations$station_id & trips$to_station_id %in% stations$station_id ), , drop = FALSE]


distance.matrix <- read.csv("gmapsData/gmapsDistMatrix.csv")
distance.matrix[distance.matrix==0] <- 10000

columnMins <- apply(distance.matrix, 2, min)
min.df <- as.data.frame(columnMins)
min.df <- min.df[-c(1,2),,drop=FALSE]
min.df$columnMins <- as.numeric(as.character(min.df$columnMins))
close<- min.df< limit
which(close)
minDist<- min(min.df$columnMins)
which(min.df$columnMins==minDist)


close_station <- subset(min.df, close)
close_station_koord <- row.names(close_station)

#remove "Distance."
close_station_coord <- substr(close_station_koord, 10, nchar(close_station_koord))

st_coords_split <- strsplit(close_station_coord, "[.][.]") # Split coordinates

closeLats <- lapply(st_coords_split, `[[`, 1)
closeStations <- stations[stations$lat %in% closeLats,]
stations$isClose <- stations$lat %in% closeLats

#Add number of departures from each station
byTrip<-group_by(trips,from_station_id)
SumByTrip<-dplyr::summarize(byTrip, departures = n())
#Factorize from_station_id
SumByTrip$from_station_id<-as.factor(SumByTrip$from_station_id)

stations$nDepartures <- dplyr::summarise(byTrip, departures = n())


#-------Plot

lat<-c(47.58, 47.68)
lon<-c(-122.25, -122.38)
seattle_impr<-get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 13, maptype = "satellite", source = "google")

map_fromPoints<-ggmap(seattle_impr) + 
  geom_point(data = stations, 
             aes(x = long,y = lat, size = nDepartures$departures, color = isClose)) +
  labs(title = "Station departures categorized by neighbor vincinity\n", color = "Nearest neighboring station is:\n", size = "Number of departures:\n")+
  scale_color_manual(labels = c("<=400 m (1300 ft) away", ">400m (1300ft) away"), values = c("blue", "red"))

