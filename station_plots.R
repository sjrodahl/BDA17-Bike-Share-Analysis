# Trying to find empty stations. 

library(dplyr)
library(ggmap)


setwd("D:/Sondre/Dokumenter/UCSD/IRGN452 BigDataAnalytics/Project/BDA17-Bike-Share-Analysis/")

trips<-read.csv("data/trip.csv", stringsAsFactors = FALSE, sep = ";") #Set nrows for testing purposes
stations <- read.csv("data/station.csv")

n=1

count_from_stations <- group_by(trips, from_station_name) %>% dplyr::summarise(departures= n()) %>% arrange(desc(departures))
stations_with_dep <- merge(count_from_stations, stations, by.x = "from_station_name", by.y = "name") %>% arrange(desc(departures))
n_most_popular <- head(stations_with_dep, n)
rest <- tail(stations_with_dep, -n)

lat<-c(47.58, 47.68)
lon<-c(-122.25, -122.38)
seattle_impr<-get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 13, maptype = "satellite", source = "google")

map_with_all_stations <- ggmap(seattle_impr)+
  geom_point(data=stations_with_dep, stat="identity", aes(x=long, y=lat, size=departures), color="orange")+
  scale_color_discrete(guide=FALSE) +
  scale_size_continuous(name="Total departures")+
  labs(title="Station location and total number of departures", subtitle=".")+
  theme(axis.title = element_text(size=18), axis.text = element_text(size=14, face="bold"), title = element_text(size=22), legend.text = element_text(size=14))


map_combined <- ggmap(seattle_impr) + 
  geom_point(data=n_most_popular, stat="identity", aes(x=long, y=lat, size = departures), color="blue")+ 
  geom_point(data=rest, stat="identity", aes(x=long, y=lat, size = departures), color="orange")+
  scale_colour_discrete(guide = FALSE) +
  scale_size_continuous(name="Total departures")+
  labs(title="Station location and total number of departures.", subtitle="Top stations in blue.")+
  theme(axis.title = element_text(size=18), axis.text = element_text(size=14, face="bold"), title = element_text(size=22), legend.text = element_text(size=14))

  

pier69 <- trips[trips$from_station_name==stations_with_dep$from_station_name[1],]
pier69_member <- pier69[pier69$usertype=="Member",]

nrow(pier69_member)
