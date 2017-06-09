#sondres plots
library(data.table)
library(dplyr)
library(ggmap)

setwd("D:/Sondre/Dokumenter/UCSD/IRGN452 BigDataAnalytics/Project/BDA17-Bike-Share-Analysis/")

trips<-read.csv("data/trips_processed.csv") #Set nrows for testing purposes
station <- read.csv("data/station.csv")
#

count_from_st_by_day <- tally(group_by(trips, onlyDate, from_station_id), sort = FALSE )
count_to_st_by_day <- tally(group_by(trips, onlyDate, to_station_id), sort = FALSE )

count_merged <- merge(count_from_st_by_day, count_to_st_by_day, by.x=c("onlyDate", "from_station_id"), by.y =c("onlyDate", "to_station_id"))
names(count_merged)[3] <- "num_from"
names(count_merged)[4] <- "num_to"
count_merged[, "bike_gain"] <- count_merged$num_to - count_merged$num_from
count_merged[,"year"] <- year(count_merged$onlyDate)
count_merged[,"month"] <- month(count_merged$onlyDate)
count_merged[, "day"] <- mday(count_merged$onlyDate)

ave_bike_gain_per_station <- aggregate(bike_gain~from_station_id,count_merged, FUN=mean)

plot(ave_bike_gain_per_station)
