
#Comment out the other user.
#setwd("/Users/tormagnusmichaelsen/Documents/BDA/Project/BDA17-Bike-Share-Analysis")
setwd("D:/Sondre/Dokumenter/UCSD/IRGN452 BigDataAnalytics/Project/BDA17-Bike-Share-Analysis/")

trips<-read.csv("data/trip.csv", stringsAsFactors = FALSE, sep = ";") #Set nrows for testing purposes
trips$startDate<- as.character(trips$starttime) #to characters
trips$endDate<- as.character(trips$stoptime) #to characters

#------------add startdate and enddate as POSIXct
trips$startDate <- as.POSIXct(trips$startDate,format="%m/%d/%Y %H:%M") #to date
trips$onlyDate<-as.Date(trips$startDate,tz = "PST8PDT")
#trips$onlyDate<-factor(trips$onlyDate)
trips$bikeid<-factor(trips$bikeid)

av_trips_per_bike_per_day <- (nrow(trips)/nlevels(trips$bikeid))/nlevels(trips$onlyDate)

# = 1.1266674 trips/day/bike

grpd <- group_by(trips, onlyDate, bikeid)
sum_grpd <- dplyr::summarise(grpd, departures= n())
date_grpd <- group_by(sum_grpd, onlyDate)
total_grpd <- dplyr::summarise(date_grpd, bikes_used = n())
mean_grpd <- dplyr::summarise(date_grpd, mean = mean(departures))

grpd_bike <- group_by(trips, onlyDate, add = FALSE)
num_unique_bikes <- dplyr::summarise(grpd_bike, unq_bikes = n_distinct(bikeid))

ggplot(data=num_unique_bikes, aes(x=onlyDate, y=unq_bikes, group=1))+
  geom_line() +
  scale_x_date("Date") +
  scale_y_continuous("Number of bikes used")+
  ggtitle("Number of unique bikes used per day")
                 
       