#agedistribution over trips
p1 <- ggplot(data = trips, aes(x = age)) + geom_histogram(binwidth = 1, color='black',fill='orange') + scale_x_continuous(breaks = seq(15,81,5))
p1

#----------station plot--------
stationDistr<-ggplot(data = trips, aes(x = to_station_name)) + geom_bar(color='black',fill='orange') + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))



#------------plots for tripduration--------
tripDurLogHist<-ggplot(aes(x = tripduration), data = trips) + geom_histogram(color='black',fill='orange') + 
  scale_x_log10()
tripDurLogHist

tripDurContHist<-ggplot(aes(x = tripduration), data = trips) + geom_histogram(color='black',fill='orange') + 
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
tripsByMinTemp<-ggplot(data = trips, aes(x = Min_TemperatureF)) + 
  geom_histogram(color='black',fill='orange') +
  scale_x_continuous()

tripsByRainInch<-ggplot(data = trips, aes(x = Precipitation_In)) + 
  geom_histogram(color='black',fill='orange') +
  scale_x_continuous()

tripsByMeanTemp<-ggplot(data = trips, aes(x = Mean_TemperatureF)) + 
  geom_histogram(color='black',fill='orange') +
  scale_x_continuous()

#plotting by temperature reveals more trips when its warmer, but migth need to 
#divide by total days of this temperature? as well as for rainy days, need to divide
#number of trips on rainy days in total to compare with non-rainy days