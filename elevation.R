# elevation

library(googleway)
library(ggmap)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(scales)

#Comment out the other user.
#setwd("/Users/tormagnusmichaelsen/Documents/BDA/Project/BDA17-Bike-Share-Analysis")
setwd("D:/Sondre/Dokumenter/UCSD/IRGN452 BigDataAnalytics/Project/BDA17-Bike-Share-Analysis/")
trips <- read.csv("data/trips_processed.csv")
stations<-read.csv("data/station.csv")
api_key <- readLines("google_elevation_api_key.txt")

#Add elevation data to stations
stations$elevation <- 0
for (i in 1:nrow(stations)){
  df_loc <- data.frame(lat=stations$lat[i], lon=stations$long[i])
  elev <- google_elevation(df_locations = df_loc, key = api_key)
  stations$elevation[i] <- elev[[1]][[1]]
}

#Add number of arrivals and departures from stations:
sumDepartures <- group_by(trips,from_station_id, add= FALSE) %>% dplyr::summarise(n())
sumArrivals <- group_by(trips, to_station_id, add=FALSE) %>% dplyr::summarise(n())

stations$num_departures <- sumDepartures[match(stations$station_id, sumDepartures$from_station_id),"n()"]
stations$num_arrivals <- sumArrivals[match(stations$station_id, sumArrivals$to_station_id),"n()"]
stations$bike_inflow <- stations$num_arrivals$`n()`-stations$num_departures$`n()`
stations$bike_inflow_cat <- stations$bike_inflow>=0

lat<-c(47.58, 47.68)
lon<-c(-122.25, -122.38)
seattle_impr<-get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 13, maptype = "satellite", source = "google")

map_with_all_stations <- ggmap(seattle_impr)+
  geom_point(data=stations, stat="identity", aes(x=long, y=lat,color=elevation, size = bike_inflow, shape = bike_inflow_cat))+
  scale_size_continuous(name= "Net inflow of bikes")+
  scale_shape_discrete(name="Net inflow of bikes", labels = c("Negative - more bikes leave", "Positive - more bikes arrive"))+
  scale_color_gradient(name = "Elevation\n(meters above sea level)", low="green", high="red")+
  labs(title="Station location and total number of departures", subtitle=".")+
  theme(axis.title = element_text(size=18), axis.text = element_text(size=14, face="bold"), title = element_text(size=22), legend.text = element_text(size=14))


# Case study: E Pine St & 16th Ave 
pine_trips <- trips[trips$from_station_name == "E Pine St & 16th Ave" | trips$to_station_name== "E Pine St & 16th Ave",]
pine_trips$departingFrom <- pine_trips$from_station_name=="E Pine St & 16th Ave"
pine_trips_2015_10_20 <- pine_trips[pine_trips$onlyDate=="2015-10-20",]
pine_trips_members <- pine_trips[pine_trips$usertype=="Member",]
num_member_trips <- nrow(pine_trips_members)
pine_trips_members_uphill <- pine_trips[pine_trips_members$to_station_name =="E Pine St & 16th Ave",]
pine_trips_members_downhill <- pine_trips[pine_trips_members$from_station_name=="E Pine St & 16th Ave", ]
###### Hourly plot of E Pine St & 16th Ave 

hourly_up <- group_by(pine_trips_members_uphill, sHour)
hourly_down <-group_by(pine_trips_members_downhill, sHour)

per_hour_up<- dplyr::summarize(hourly_up, departures = n())
per_hour_down <- dplyr::summarize(hourly_down, departures = n())


#fixed bug x-ticks not showing by factorizing sHour
hourlyPlot<-ggplot() +
  #geom_point(data = per_hour_up, aes(x=sHour, y=departures)) +
  geom_line(data = per_hour_up, aes(x=sHour, y=departures/689)) +
  geom_line(data=per_hour_down, aes(x=sHour, y=departures/689))+
  xlab('Hour')+
  ylab('rentals')+
  ggtitle('Hourly trips across days')+
  theme(axis.title = element_text(size=18), 
        axis.text = element_text(size=14, face="bold"), 
        title = element_text(size=22))
# not a very good plot...

# Looking at just one day.
pine_trips_2015_10_20$ID <- seq.int(nrow(pine_trips_2015_10_20))
pine_trips_2015_10_20$cumulative = 0
pine_trips_2015_10_20$cumulative[1] = ifelse(pine_trips_2015_10_20$departingFrom[[1]], -1, 1)
for(i in 2:nrow(pine_trips_2015_10_20)){
  if (pine_trips_2015_10_20$departingFrom[[i]]){
    pine_trips_2015_10_20$cumulative[i]=pine_trips_2015_10_20$cumulative[i-1]-1
  }
  else{
    pine_trips_2015_10_20$cumulative[i]=pine_trips_2015_10_20$cumulative[i-1]+1
  }
}

ggplot(data = pine_trips_2015_10_20, aes(x=ID, y = cumulative)) + 
  geom_line(stat="identity", aes(group=1), size = 2)+
  labs(title="Bike movement from E Pine St & 16th Ave", subtitle= "On 20th of October, 2015", x="Tripnumber", y ="Bike number difference since start of day")+
  theme(axis.title = element_text(size=18), 
        axis.text = element_text(size=14, face="bold"), 
        title = element_text(size=22))+
  theme_hc(bgcolor = "darkunica") +
  scale_colour_hc("darkunica")
