



#------------------------------Simple distributions-----------------------------

#------------------------------Total distribution of trips of entire dataset
#tiff('test.tiff', units="in", width=15, height=20, res=300)
userSum<-summary(trips$usertype)
userSum<-data.frame(usertype=names(userSum), totalTrips=userSum)

p1 <- ggplot(data = userSum, aes( x = usertype, y = totalTrips)) + 
  geom_bar(stat="identity",color='black',fill="#ff9e30")+
  theme_hc(bgcolor = "darkunica") +
  scale_colour_hc("darkunica")+
  xlab("Usertype")+
  ylab("Total number of trips taken")+
  theme(axis.title = element_text(size=14), 
        axis.text = element_text(size=12, face="bold"), 
        title = element_text(size=16, vjust = 1.5))+
  scale_y_continuous(breaks = seq(0,160000,25000))+
  geom_text(aes(label=totalTrips), vjust=1.5,size = 6, colour="#25333f")+
  ggtitle("Number of trips taken by members and non-members")
ggsave("plots/barplotByUsertype.png")
#dev.off()



#------------------------------Tripdistribution by month
tripsByMonth<-group_by(trips_df[trips_df$sYear==2015,], sMonth, usertype)
sumByMonth<-dplyr::summarize(tripsByMonth, trips = n())
levels(sumByMonth$usertype)<-c("Member","Short-Term Pass Holder")

sumByMonth$sMonth<-factor(sumByMonth$sMonth, levels=month.abb, ordered = TRUE)
fill = c("#ff9e30", "#5c616b")

monthlyTrips<-ggplot(tripsByMonth, aes(x = sMonth, y= trips))+
  geom_bar(data = sumByMonth, stat = "identity", aes(fill = usertype),position = position_stack(reverse = TRUE)) +
  theme_hc(bgcolor = "darkunica") +
  scale_colour_hc("darkunica")+
  scale_fill_manual(values = fill)+
  xlab("Month")+
  ylab("Total number of trips taken")+
  theme(axis.title = element_text(size=14), 
        axis.text = element_text(size=12, face="bold"), 
        title = element_text(size=14, vjust = 0.5))+
  scale_y_continuous(breaks = seq(0,22000,2000))+
  #geom_text(aes(label=sumByMonth$trips), vjust=1.5,size = 6, colour="#25333f")+
  ggtitle("Trips taken by members and non-members")
ggsave("plots/trips_by_months.png")


#------------------------------Agedistribution

#agedistribution over trips
ageplot <- ggplot(data = trips, aes(x = age)) + 
  geom_histogram(binwidth = 1, color = 'black', fill = '#ff9e30') + 
  theme_hc(bgcolor = "darkunica") +
  scale_colour_hc("darkunica")+
  xlab("Age")+
  ylab("Trips")+
  theme(axis.title = element_text(size=14), 
        axis.text = element_text(size=12, face="bold"), 
        title = element_text(size=14, vjust = 1.5))+
  scale_x_continuous(breaks = seq(15,81,5))+
  scale_y_continuous(breaks = seq(0,14000,2000))
  ggtitle("Distribution of age across all trips")
ggsave("plots/Agedistribution_of_trips.png")


#----------station plot--------
stationDistr<-ggplot(data = trips, aes(x = to_station_name)) + geom_bar(color='black',fill='orange') + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))



#------------plots for tripduration--------
tripDurLogHist<-ggplot(aes(x = tripduration/60), data = trips) + geom_histogram(color='black',fill='orange') + 
  scale_x_log10()
tripDurLogHist

tripDurContHist<-ggplot(aes(x = tripduration/60), data = trips) + geom_histogram(color='black',fill='orange') + 
  scale_x_continuous()
tripDurContHist

tripDurSqrtHist<-ggplot(aes(x = tripduration), data = trips) + geom_histogram(color='black',fill='orange') + 
  scale_x_sqrt()

tripDurBox<-ggplot(data = trips, aes(x = factor(0), y = tripduration)) + geom_boxplot() + scale_y_continuous(limits = c(0,1000)) + coord_flip()



#-----------Relations between weather and trips-------
#need to compare factors in weather$date and 
#trips$date, and plot number of trips under different weatherconditions, and 
#only compare weekdays with weekdays and weekends with weekends


tripDurByEventsBox<-ggplot(data = trips, aes(x = Events, y = tripduration)) + geom_boxplot()  #scale_y_continuous(limits = c(0,1500)) + coord_flip()


tripsByHour<-ggplot(data = trips, aes(x = sHour)) + 
  geom_histogram(color='black',fill='orange', binwidth = 1) +
  scale_x_continuous(breaks = seq(0,24,1))

tripsByMinute<-ggplot(data = trips, aes(x = sMinute)) + 
  geom_histogram(color='black',fill='orange', binwidth = 2) +
  scale_x_continuous(breaks = seq(0,60,5))

tripDurByDay<-ggplot(data = trips, aes(x = sDay, y = )) + geom_boxplot()


#-----------Weather-------
tripsByMinTemp<-ggplot(data = trips, aes(x = Min_Temperature_F)) + 
  geom_histogram(color='black',fill='orange') +
  scale_x_continuous()

tripsByRainInch<-ggplot(data = trips, aes(x = Precipitation_In)) + 
  geom_histogram(color='black',fill='orange') +
  scale_x_continuous()

tripsByMeanTemp<-ggplot(data = trips, aes(x = Mean_Temperature_F)) + 
  geom_histogram(color='black',fill='orange') +
  scale_x_continuous()

#plotting by temperature reveals more trips when its warmer, but migth need to 
#divide by total days of this temperature? as well as for rainy days, need to divide
#number of trips on rainy days in total to compare with non-rainy days