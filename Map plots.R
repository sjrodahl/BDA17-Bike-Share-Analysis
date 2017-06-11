

# trips<-read.csv("data/trips_processed.csv", stringsAsFactors = FALSE, sep = ";") #Set nrows for testing purposes
# weather<-read.csv("data/weather_processed2.csv")
# stations<-read.csv("data/station.csv")
# 
# trips_df<-tbl_df(trips)
# trips_members<-filter(trips_df, usertype=="Member")
# trips_members_df<-tbl_df(trips_members)

workHours<-c(7,8,9,10,15,16,17,18,19)
otherHours<-c(1,2,3,4,5,6,11,12,13,14,20,21,22,23,24)


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
#want to visualize difference between north and south in morning and afternoon

north<-dplyr::filter(trips_df, direction=="North") #now north and south are trips_df split in two
south<-dplyr::filter(trips_df, direction=="South")
#group by stations, filter by workhours, count trips from each station
northByTrip<-group_by(north,from_station_id, to_station_id)
southByTrip<-group_by(south,from_station_id, to_station_id)

SumNorthByTrip<-dplyr::summarize(northByTrip, departures = n())
SumSouthByTrip<-dplyr::summarize(southByTrip, departures = n())


#add opacity to north and south
SumNorthByTrip$opacity<-SumNorthByTrip$departures/max(SumNorthByTrip$departures)
SumSouthByTrip$opacity<-SumSouthByTrip$departures/max(SumSouthByTrip$departures)

northAlphaMin<-min(SumNorthByTrip$opacity)
northAlphaMax<-max(SumNorthByTrip$opacity)

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
               color = "#c1370d")  + coord_cartesian()+
  ggtitle('title.png')+
  ggsave('filename.png')

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

#add opacity to sumbyTrip
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
                 color = "#f96318")+
                 coord_cartesian() + scale_alpha_identity() +
  theme(axis.title = element_text(size=18), 
        axis.text = element_text(size=14, face="bold"), 
        title = element_text(size=22))+
  ggtitle('All trips in dataset')+
  ggsave('All trips in dataset.png')
             


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
                   alpha = opacity))  + coord_cartesian() + scale_alpha_identity()+
  ggtitle('Triplegs by agegroups, 7-10am and 3-7pm')+
  ggsave('Triplegs_workhours1.png')
#if subset: data = subset(SumByAgeCatUnique, Agecat1 == 25-34,
#if all agecats is wanted: data = SumByAgeCatUnique,

#----------------------Map with trips by agecat2 for workhours----------------
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
  geom_point(data = from_stations, 
             aes(x = from_long,
                 y = from_lat
                 ),
             alpha = 0.8) +
  theme(axis.title = element_text(size=14), 
        axis.text = element_text(size=10, face="bold"), 
        title = element_text(size=14, vjust = 0.5))+
  ggtitle("Triplegs by agegroups, 7-10am and 3-7pm")+
  ggsave("plots/Triplegs_workhours2.png")

#----------------------Map with trips by agecat2 for non-workhours---------------- 
byAgecat2nonWork<-group_by(trips_df,from_station_id, to_station_id,Agecat2) %>% 
  dplyr::filter(usertype=="Member") %>% filter(sHour %in% otherHours )
SumByAgeCat2nonWork<-dplyr::summarize(byAgecat2nonWork, departures = n())
#add coordinates
SumByAgeCat2nonWork$from_long<-(stations$long[match(SumByAgeCat2nonWork$from_station_id,stations$station_id)])
SumByAgeCat2nonWork$from_lat<-(stations$lat[match(SumByAgeCat2nonWork$from_station_id,stations$station_id)])

SumByAgeCat2nonWork$to_long<-(stations$long[match(SumByAgeCat2nonWork$to_station_id,stations$station_id)])
SumByAgeCat2nonWork$to_lat<-(stations$lat[match(SumByAgeCat2nonWork$to_station_id,stations$station_id)])
#remove trips to and from same stations
SumByAgeCatUnique2nonWork <- SumByAgeCat2nonWork[!(SumByAgeCat2nonWork$from_station_id==SumByAgeCat2nonWork$to_station_id),]
#create opacityvalue in sumbycatunique
SumByAgeCatUnique2nonWork$opacity<-SumByAgeCatUnique2nonWork$departures/max(SumByAgeCatUnique2nonWork$departures)
alphaMin<-min(SumByAgeCatUnique2nonWork$opacity)
alphaMax<-max(SumByAgeCatUnique2nonWork$opacity)
#map
mapAgecat2nonWork<-ggmap(seattle_toner,extent = "normal") + 
  geom_segment(stat="identity",data = SumByAgeCatUnique2nonWork, 
               aes(x = from_long,
                   y = from_lat, 
                   xend = to_long, 
                   yend = to_lat, 
                   color = Agecat2, 
                   alpha = opacity))  + coord_cartesian() + scale_alpha_identity() +
  scale_color_manual(values=c("#0434f2", "#06f702", "#fc0a2e"))+
  ggtitle("Plot over trips for hours other than 7-10am and 3-7pm")+
  ggsave("plots/agecat2 nonWork.png")

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
  ggtitle('Map over total rentals from stations by stationgroups.png')+
  ggsave('plots/map over total rentals from stations by stationgroups.png')


#-----------------------maps for different agecats, satellite--------------------
mapAgeGroups1<-ggmap(seattle_impr,extent = "normal") + 
geom_segment(stat="identity",data=subset(SumByAgeCatUnique1, Agecat1 == "19-24"), 
aes(
 x = from_long,
 y = from_lat, 
xend = to_long, 
  yend = to_lat, 
alpha = opacity), color = "#f7020e"
)  + coord_cartesian() + scale_alpha_identity() +
  ggtitle('Total number of trips, age 19-24')+
  ggsave('Total number of trips, age 19-24.png')

mapAgeGroups2<-ggmap(seattle_impr,extent = "normal") + 
  geom_segment(stat="identity",data=subset(SumByAgeCatUnique1, Agecat1 == "25-34"), 
  aes(
  x = from_long,
      y = from_lat, 
     xend = to_long, 
   yend = to_lat, 
alpha = opacity), color = "#f7020e"
)  + coord_cartesian() + scale_alpha_identity() +
  ggtitle('Total number of trips, age 25-34')+
  ggsave('Total number of trips, age 25-34.png')

mapAgeGroups3<-ggmap(seattle_impr,extent = "normal") + 
  geom_segment(stat="identity",data=subset(SumByAgeCatUnique1, Agecat1 == "35-44"), 
               aes(
                 x = from_long,
                 y = from_lat, 
                 xend = to_long, 
                 yend = to_lat, 
                 alpha = opacity), color = "#f7020e"
  )  + coord_cartesian() + scale_alpha_identity() +
  ggtitle('Total number of trips, age 35-44')+
  ggsave('Total number of trips, age 35-44.png')


mapAgeGroups4<-ggmap(seattle_impr,extent = "normal") + 
  geom_segment(stat="identity",data=subset(SumByAgeCatUnique1, Agecat1 == "45-54"), 
 aes(
x = from_long,
y = from_lat, 
xend = to_long, 
 yend = to_lat, 
alpha = opacity), color = "#f7020e"
)  + coord_cartesian() + scale_alpha_identity() +
  ggtitle('Total number of trips, age 45-54')+
  ggsave('Total number of trips, age 45-54.png')

mapAgeGroups5<-ggmap(seattle_impr,extent = "normal") + 
  geom_segment(stat="identity",data=subset(SumByAgeCatUnique1, Agecat1 == "55-62"), 
aes(
x = from_long,
y = from_lat, 
xend = to_long, 
yend = to_lat, 
alpha = opacity), color = "#f7020e"
)  + coord_cartesian() + scale_alpha_identity() +
  ggtitle('Total number of trips, age 55-62')+
  ggsave('Total number of trips, age 55-62.png')

mapAgeGroups6<-ggmap(seattle_impr,extent = "normal") + 
  geom_segment(stat="identity",data=subset(SumByAgeCatUnique1, Agecat1 == "63-81"), 
aes(
x = from_long,
y = from_lat, 
xend = to_long, 
yend = to_lat, 
alpha = opacity), color = "#f7020e"
)  + coord_cartesian() + scale_alpha_identity() +
  ggtitle('Total number of trips, age 63-81')+
  ggsave('Total number of trips, age 63-81.png')
  #scale_alpha_discrete(range = c(0.1,1), limits=alphaMin:alphaMax)



#-----------------------maps for different agecats, satellite--------------------
tonermapAgeGroups1<-ggmap(seattle_toner,extent = "normal") + 
  geom_segment(stat="identity",data=subset(SumByAgeCatUnique1, Agecat1 == "19-24"), 
               aes(
                 x = from_long,
                 y = from_lat, 
                 xend = to_long, 
                 yend = to_lat, 
                 alpha = opacity), color = "#f7020e"
  )  + coord_cartesian() + scale_alpha_identity() +

ggtitle('Total number of trips, age 19-24')+
  ggsave('Total number of trips, age 19-24.png')

tonermapAgeGroups2<-ggmap(seattle_toner,extent = "normal") + 
  geom_segment(stat="identity",data=subset(SumByAgeCatUnique1, Agecat1 == "25-34"), 
               aes(
                 x = from_long,
                 y = from_lat, 
                 xend = to_long, 
                 yend = to_lat, 
                 alpha = opacity), color = "#f7020e"
  )  + coord_cartesian() + scale_alpha_identity() +
ggtitle('Total number of trips, age 25-34')+
  ggsave('Total number of trips, age 25-34.png')

tonermapAgeGroups3<-ggmap(seattle_toner,extent = "normal") + 
  geom_segment(stat="identity",data=subset(SumByAgeCatUnique1, Agecat1 == "35-44"), 
               aes(
                 x = from_long,
                 y = from_lat, 
                 xend = to_long, 
                 yend = to_lat, 
                 alpha = opacity), color = "#f7020e"
  )  + coord_cartesian() + scale_alpha_identity() +

ggtitle('Total number of trips, age 35-44')+
  ggsave('Total number of trips, age 35-44.png')

tonermapAgeGroups4<-ggmap(seattle_toner,extent = "normal") + 
  geom_segment(stat="identity",data=subset(SumByAgeCatUnique1, Agecat1 == "45-54"), 
               aes(
                 x = from_long,
                 y = from_lat, 
                 xend = to_long, 
                 yend = to_lat, 
                 alpha = opacity), color = "#f7020e"
  )  + coord_cartesian() + scale_alpha_identity() +
  ggtitle('Total number of trips, age 45-54')+
  ggsave('Total number of trips, age 45-54.png')


tonermapAgeGroups5<-ggmap(seattle_toner,extent = "normal") + 
  geom_segment(stat="identity",data=subset(SumByAgeCatUnique1, Agecat1 == "55-62"), 
               aes(
                 x = from_long,
                 y = from_lat, 
                 xend = to_long, 
                 yend = to_lat, 
                 alpha = opacity), color = "#f7020e"
  )  + coord_cartesian() + scale_alpha_identity() +
  ggtitle('Total number of trips, age 55-62')+
  ggsave('Total number of trips, age 55-62.png')

tonermapAgeGroups6<-ggmap(seattle_toner,extent = "normal") + 
  geom_segment(stat="identity",data=subset(SumByAgeCatUnique1, Agecat1 == "63-81"), 
               aes(
                 x = from_long,
                 y = from_lat, 
                 xend = to_long, 
                 yend = to_lat, 
                 alpha = opacity), color = "#f7020e"
  )  + coord_cartesian() + scale_alpha_identity() +
  ggtitle('Total number of trips, age 63-81')+
  ggsave('Total number of trips, age 63-81.png')
#scale_alpha_discrete(range = c(0.1,1), limits=alphaMin:alphaMax)


plot <- multiplot(mapAgeGroups1, mapAgeGroups2, mapAgeGroups3, mapAgeGroups4, mapAgeGroups5, mapAgeGroups6, cols = 3)+

#ggtitle('title.png')+
ggsave('plots/multiple plot trips by agegroups, satellite.png')


plot2 <- multiplot(tonermapAgeGroups1, tonermapAgeGroups2, tonermapAgeGroups3, tonermapAgeGroups4, tonermapAgeGroups5, tonermapAgeGroups6, cols = 3)+

#ggtitle('title.png')+
  ggsave('plots/multiple plot trips by agegroups, toner-lite.png')




