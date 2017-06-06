
#--------------------define map--------------------
#package needed #install_version
#("ggplot2", version = "2.1.0", repos = "http://cran.us.r-project.org")
lat<-c(47.58, 47.68)
lon<-c(-122.25, -122.38)
seattle_impr<-get_map(location = c(lon = mean(lon), 
                      lat = mean(lat)), 
                      zoom = 13, 
                      maptype = "satellite", 
                      source = "google")

seattle_toner<-get_map(location = c(lon = mean(lon), 
                                   lat = mean(lat)), 
                      zoom = 13, 
                      maptype = "toner-lite", 
                      source = "google")




#---------------Trips north and south--------------
#add north or south depending on positive or negative difference in latitude, 
#as character since dplyr::filter didn't work with logical values(?)

north<-dplyr::filter(trips_df, direction=="North")
south<-dplyr::filter(trips_df, direction=="South")

mapNorthSouth<-ggmap(seattle_impr,extent = "normal") + 
  geom_segment(data = north, 
               aes(x = from_long,
                   y = from_lat, 
                   xend = to_long, 
                   yend = to_lat),
               alpha = north$alpha1, 
               color = "#0c61f4")  + 
  
  geom_segment(data = south, 
               aes(x = from_long,
                   y = from_lat, 
                   xend = to_long, 
                   yend = to_lat),
               alpha = south$alpha1, 
               color = "#c1370d")  + coord_cartesian()

#---------------Generic map over all trips------------
#first group by pairs of from long and lat
byTrip<-group_by(trips_df,from_station_id, to_station_id)
SumByTrip<-dplyr::summarize(byTrip, departures = n())

#need ti factorize to and from station_id's
SumByTrip$from_station_id<-as.factor(SumByTrip$from_station_id)
SumByTrip$to_station_id<-as.factor(SumByTrip$to_station_id)

#need to add station coordinates since summarize removes this
SumByTrip$from_long<-(stations$long[match(SumByTrip$from_station_id,stations$station_id)])
SumByTrip$from_lat<-(stations$lat[match(SumByTrip$from_station_id,stations$station_id)])

SumByTrip$to_long<-(stations$long[match(SumByTrip$to_station_id,stations$station_id)])
SumByTrip$to_lat<-(stations$lat[match(SumByTrip$to_station_id,stations$station_id)])

#add opacity 
SumByTrip$opacity<-SumByTrip$departures/max(SumByTrip$departures)
alphaMin<-min(SumByTrip$opacity)
alphaMax<-max(SumByTrip$opacity)
SumByTripsUnique <- SumByTrip[!(SumByTrip$from_station_id==SumByTrip$to_station_id),]
map_linesTrips<-ggmap(seattle_toner,extent = "normal") + 
  geom_segment(stat = "identity", data = SumByTripsUnique, 
             aes(x = from_long,
                 y = from_lat, 
                 xend = to_long, 
                 yend = to_lat, 
                 alpha = opacity), 
                 color = "#201154")+
                 coord_cartesian() + scale_alpha_identity() 
             


#----------------map with stations, size by number of departures
map_fromPoints<-ggmap(seattle_toner) + 
  geom_point(data = from_stations, 
             aes(x = from_long,
                 y = from_lat, 
                 size = 4, 
             color = departures),  
             alpha = 0.8) +
  scale_colour_gradient(low = "blue", high="red")+
  ggtitle('map over total rentals from stations.png')+
  ggsave('plots/map over total rentals from stations.png')


#-----------------map with trips by agecat1----------------
#Map over movement for agegroups, need to group by agecategories, might want to 
#only look at a time interval, can do manually or assign a timetype to trips_df
#filter out non-members?
#also need to add target for timeslots
workHours<-c(7,8,9,10,15,16,17,18,19)
#add variable for morning or afternoon or middle of day?
morning<-c(5,6,7,8,9)
afternoon<-c(16,17,18,19) ##### this lines over is not included atm

byAgecat1<-group_by(trips_df,from_station_id, to_station_id,Agecat1) %>% 
  dplyr::filter(usertype=="Member") %>% filter(sHour %in% workHours )
SumByAgeCat1<-dplyr::summarize(byAgecat1, departures = n())
#add coordinates
SumByAgeCat1$from_long<-(stations$long[match(SumByAgeCat1$from_station_id,stations$station_id)])
SumByAgeCat1$from_lat<-(stations$lat[match(SumByAgeCat1$from_station_id,stations$station_id)])

SumByAgeCat1$to_long<-(stations$long[match(SumByAgeCat1$to_station_id,stations$station_id)])
SumByAgeCat1$to_lat<-(stations$lat[match(SumByAgeCat1$to_station_id,stations$station_id)])
#remove trips to and from same stations
SumByAgeCatUnique1 <- SumByAgeCat1[!(SumByAgeCat1$from_station_id==SumByAgeCat1$to_station_id),]
#create opacityvalue in sumbycatunique
SumByAgeCatUnique1$opacity<-SumByAgeCatUnique1$departures/max(SumByAgeCatUnique1$departures)
alphaMin<-min(SumByAgeCatUnique1$opacity)
alphaMax<-max(SumByAgeCatUnique1$opacity)
#map
mapAgeGroups<-ggmap(seattle_toner,extent = "normal") + 
  geom_segment(stat="identity",data = SumByAgeCatUnique1, 
               aes(x = from_long,
                   y = from_lat, 
                   xend = to_long, 
                   yend = to_lat, 
                   color = Agecat1, 
                   alpha = opacity))  + coord_cartesian() + scale_alpha_identity()
#if subset: data = subset(SumByAgeCatUnique, Agecat1 == 25-34,
#if all agecats is wanted: data = SumByAgeCatUnique,

#----------------------Map with trips by agecat2----------------
byAgecat2<-group_by(trips_df,from_station_id, to_station_id,Agecat2) %>% 
  dplyr::filter(usertype=="Member") %>% filter(sHour %in% workHours )
SumByAgeCat2<-dplyr::summarize(byAgecat2, departures = n())
#add coordinates
SumByAgeCat2$from_long<-(stations$long[match(SumByAgeCat2$from_station_id,stations$station_id)])
SumByAgeCat2$from_lat<-(stations$lat[match(SumByAgeCat2$from_station_id,stations$station_id)])

SumByAgeCat2$to_long<-(stations$long[match(SumByAgeCat2$to_station_id,stations$station_id)])
SumByAgeCat2$to_lat<-(stations$lat[match(SumByAgeCat2$to_station_id,stations$station_id)])
#remove trips to and from same stations
SumByAgeCatUnique2 <- SumByAgeCat2[!(SumByAgeCat2$from_station_id==SumByAgeCat2$to_station_id),]
#create opacityvalue in sumbycatunique
SumByAgeCatUnique2$opacity<-SumByAgeCatUnique2$departures/max(SumByAgeCatUnique2$departures)
alphaMin<-min(SumByAgeCatUnique2$opacity)
alphaMax<-max(SumByAgeCatUnique2$opacity)
#map
mapAgecat2<-ggmap(seattle_toner,extent = "normal") + 
  geom_segment(stat="identity",data = SumByAgeCatUnique2, 
               aes(x = from_long,
                   y = from_lat, 
                   xend = to_long, 
                   yend = to_lat, 
                   color = Agecat2, 
                   alpha = opacity))  + coord_cartesian() + scale_alpha_identity() +
  scale_color_manual(values=c("#0434f2", "#06f702", "#fc0a2e"))+
  ggsave("plots/agecat2.png")

#if subset: data = subset(SumByAgeCatUnique, Agecat1 == 25-34,
#if all agecats is wanted: data = SumByAgeCatUnique,




#---------------map with stations colored after which group the belong---------

#dataprep
DeparturesByStationGroup<-group_by(trips_df, fromStationGroup)
sumDeparturesByStationGroup<-dplyr::summarize(DeparturesByStationGroup, departures = n())

#map with stations colored by grouping
map_fromPoints<-ggmap(seattle_impr) + 
  geom_point(data = from_stations, 
             aes(x = from_long,y = from_lat, size = departures, color = fromStationGroup))+
  ggtitle('map over total rentals from stations by stationgroups.png')+
  ggsave('plots/map over total rentals from stations by stationgroups.png')


#-----------------------maps for different agecats--------------------
mapAgeGroups1<-ggmap(seattle_impr,extent = "normal") + 
geom_segment(stat="identity",data=subset(SumByAgeCatUnique, Agecat1 == "19-24"), 
aes(
 x = from_long,
 y = from_lat, 
xend = to_long, 
  yend = to_lat, 
alpha = opacity), color = "#359aff"
)  + coord_cartesian() + scale_alpha_identity() 

mapAgeGroups2<-ggmap(seattle_impr,extent = "normal") + 
  geom_segment(data=subset(SumByAgeCatUnique, Agecat1 == "25-34"), 
  aes(
  x = from_long,
      y = from_lat, 
     xend = to_long, 
   yend = to_lat, 
alpha = opacity), color = "#359aff"
)  + coord_cartesian() + scale_alpha_identity() 



mapAgeGroups4<-ggmap(seattle_impr,extent = "normal") + 
  geom_segment(data=subset(SumByAgeCatUnique, Agecat1 == "45-54"), 
 aes(
x = from_long,
y = from_lat, 
xend = to_long, 
 yend = to_lat, 
alpha = opacity), color = "#359aff"
)  + coord_cartesian() + scale_alpha_identity() 

mapAgeGroups5<-ggmap(seattle_impr,extent = "normal") + 
  geom_segment(data=subset(SumByAgeCatUnique, Agecat1 == "55-62"), 
aes(
x = from_long,
y = from_lat, 
xend = to_long, 
yend = to_lat, 
alpha = opacity), color = "#359aff"
)  + coord_cartesian() + scale_alpha_identity() 

mapAgeGroups6<-ggmap(seattle_impr,extent = "normal") + 
  geom_segment(stat="identity",data=subset(SumByAgeCatUnique, Agecat1 == "63-81"), 
aes(
x = from_long,
y = from_lat, 
xend = to_long, 
yend = to_lat, 
alpha = opacity), color = "#359aff"
)  + coord_cartesian() + scale_alpha_identity() 
  #scale_alpha_discrete(range = c(0.1,1), limits=alphaMin:alphaMax)








