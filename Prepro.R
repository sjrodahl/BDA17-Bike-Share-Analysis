#--------Run before EDA-files



#install gpclib
#install openStreetMap?
#install readr
#install scales
library(maptools)
library(RColorBrewer)
library(classInt)
library(maps)
library(OpenStreetMap)


setwd("/Users/tormagnusmichaelsen/Documents/BDA/Project/BDA17-Bike-Share-Analysis")

trips<-read.csv("data/trips_processed.csv")
weather<-read.csv("data/weather_processed2.csv")


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



##not included

#---------------------------Map routes-----------------------
# map_routes<-ggmap(seattle_impr)+
# geom_point(data = stations,
#               aes(x =long, y = lat),
#             colour = "orange",
#               alpha =0.7) +
#   geom_curve(data = SumByTrip,
#               aes(x = from_long,y = from_lat, xend = to_long, yend = to_lat, size = departures), 
#               color = "white") +
#    coord_cartesian()+
#  ggtitle('linesegment over trips.png')+
# 
# route(from, to, mode = c("driving", "walking", "bicycling", "transit"),
#   structure = c("legs", "route"), output = c("simple", "all"),
#   alternatives = FALSE, messaging = FALSE, sensor = FALSE,
#   override_limit = FALSE)
#---------------------------End map routes---------------------------


numbers <- seq(from=0, to=0.2, by=0.01)
rands<-sample(numbers, size=1653, replace=TRUE)
tripcombs$rand<-rands
#tripcombs$rand<-as.factor(tripcombs$rand)


#plotting routes in map
# routes<-route(SumByTripsUnique$from_station, SumByTripsUnique$to_station, mode = "bicycling", structure = "route")

SumByTripsUnique$fromCoords <- paste(SumByTripsUnique$from_lat,SumByTripsUnique$from_long, sep = "+")

SumByTripsUnique$toCoords <- paste(SumByTripsUnique$to_lat,SumByTripsUnique$to_long, sep = "+")

# routes<-lapply(1:5,
#        function(x) {
#            Sys.sleep(0.5)
#            ggmap::route(from = SumByTripsUnique$fromCoords[[x]],
#                          to = SumByTripsUnique$toCoords[[x]],
#                          mode = "bicycling",
#                          output = "simple")
#            } ) 
# 
