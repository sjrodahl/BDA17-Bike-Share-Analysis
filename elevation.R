# elevation

library(googleway)
library(ggmap)
library(ggplot2)
#Comment out the other user.
#setwd("/Users/tormagnusmichaelsen/Documents/BDA/Project/BDA17-Bike-Share-Analysis")
setwd("D:/Sondre/Dokumenter/UCSD/IRGN452 BigDataAnalytics/Project/BDA17-Bike-Share-Analysis/")
trips <- read.csv("data/trips_processed.csv")
stations<-read.csv("data/station.csv")
api_key <- readLines("google_elevation_api_key.txt")

stations$elevation <- 0
for (i in 1:nrow(stations)){
  df_loc <- data.frame(lat=stations$lat[i], lon=stations$long[i])
  elev <- google_elevation(df_locations = df_loc, key = api_key)
  stations$elevation[i] <- elev[[1]][[1]]
}

lat<-c(47.58, 47.68)
lon<-c(-122.25, -122.38)
seattle_impr<-get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 13, maptype = "satellite", source = "google")

map_with_all_stations <- ggmap(seattle_impr)+
  geom_point(data=stations, stat="identity", aes(x=long, y=lat,color=elevation) ,size = 4)+
  scale_color_continuous(name="Elevation")+
  scale_color_gradient(low="green", high="red")+
  labs(title="Station location and total number of departures", subtitle=".")+
  theme(axis.title = element_text(size=18), axis.text = element_text(size=14, face="bold"), title = element_text(size=22), legend.text = element_text(size=14))




