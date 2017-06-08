
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

trips$season<-time2season(trips$onlyDate, out.fmt = "seasons")
trips$season<-as.factor(trips$season)

av_trips_per_bike_per_day <- (nrow(trips)/nlevels(trips$bikeid))/nlevels(trips$onlyDate)

# = 1.1266674 trips/day/bike

grpd <- group_by(trips, onlyDate, bikeid)
sum_grpd <- dplyr::summarise(grpd, departures= n())
date_grpd <- group_by(sum_grpd, onlyDate)
total_grpd <- dplyr::summarise(date_grpd, bikes_used = n())
mean_grpd <- dplyr::summarise(date_grpd, mean = mean(departures))

grpd_bike <- group_by(trips, onlyDate, add = FALSE)
num_unique_bikes <- dplyr::summarise(grpd_bike, unq_bikes = n_distinct(bikeid))
num_unq_bikes_percent <- dplyr::summarise(grpd_bike, unq_bikes = 100*n_distinct(bikeid)/nlevels(bikeid))
yrng <-range(num_unq_bikes_percent$unq_bikes)
#season_shade <- geom_rect(aes(NULL, NULL, xmin=onlyDate, xmax = onlyDate, fill = season), ymin=yrng[1], ymax=yrng[2], data=trips)

unq_plot<-ggplot(data=num_unq_bikes_percent, aes(x=onlyDate, y=unq_bikes, group=1 ))+
  geom_line(color="#ff990c", size=1) +
  geom_line(data=num_unq_bikes_percent, size=1, aes(x = onlyDate, y = mean(unq_bikes)),color = "#140cff")+
  scale_x_date("Date") +
  scale_y_continuous("Percentage of total bike fleet used")+
  labs(title="Percentage of total bike fleet used per day", subtitle="Blue line shows average of 37.0%")+
  theme(axis.title = element_text(size=18), axis.text = element_text(size=14, face="bold"), title = element_text(size=22))
unq_plot
#unq_plot_shade <- unq_plot+season_shade

grp_bikeid <- group_by(trips, bikeid ) %>% dplyr::summarise(departures=n())

       