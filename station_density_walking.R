#Station density - stations closer than a five minute walk to the next one

setwd("D:/Sondre/Dokumenter/UCSD/IRGN452 BigDataAnalytics/Project/BDA17-Bike-Share-Analysis/")
stations <- read.csv("data/station.csv" , stringsAsFactors=FALSE)
distance.matrix<-read.csv('gmapsWalkTimeMatrix.csv') 

distance.matrix[distance.matrix==0] <- 10000

limit = 300
 
columnMins <- apply(distance.matrix, 2, min)
min.df <- as.data.frame(columnMins)
min.df <- min.df[-c(1,2),,drop=FALSE]
row.names(min.df) <- substr(rownames(min.df), 6, nchar(rownames(min.df))) #Remove "Time." from rownames
min.df$columnMins <- as.numeric(as.character(min.df$columnMins))
min.df$close<- min.df$columnMins< limit

st_coords_split <- strsplit(rownames(min.df), "[.][.]") # Split coordinates
#Only need latitude to uniqely match with station
min.df$lat <- lapply(st_coords_split, `[[`, 1)

min.df.mergeable <- min.df[c("lat", "close", "columnMins")]
stations <- merge(stations, min.df.mergeable, by="lat")


#-------Plot

lat<-c(47.58, 47.68)
lon<-c(-122.25, -122.38)
seattle_impr<-get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 13, maptype = "satellite", source = "google")

map_fromPoints<-ggmap(seattle_impr) + 
  geom_point(data = stations, 
             aes(x = long,y = lat, color = ifelse(columnMins<=300, "green", ifelse(columnMins<=600, "yellow", "red"))), size=5) +
  labs(title = "Stations and their walking distance to nearest neighbor\n", color = "Nearest neighboring station is:\n")+
  scale_color_manual(labels = c("<= 5 minutes away",  "> 10 minutes away"," > 5 minutes away"), values=c("green" ,"red", "yellow"))+
  theme(axis.title = element_text(size=18), axis.text = element_text(size=14, face="bold"), title = element_text(size=22), legend.text = element_text(size=14))

map_fromPoints

