
setwd("/Users/tormagnusmichaelsen/Documents/BDA/Project/BDA17-Bike-Share-Analysis")

trips<-read.csv("data/trips_processed.csv", stringsAsFactors = FALSE, sep = ",") #Set nrows for testing purposes
weather<-read.csv("data/weather_processed2.csv")
stations<-read.csv("data/station.csv")
weatherSum<-read.csv("data/weatherSum.csv")


trips_df<-tbl_df(trips)
trips_members<-filter(trips_df, usertype=="Member")
trips_nonmembers<-filter(trips_df, usertype!="Member")
trips_members_df<-tbl_df(trips_members)


#-------------------------------------hourly trips------------------------------
#lineplot over trips by day and time
trips$sDay<-factor(trips$sDay, levels=c('Mon', 'Tues', 'Wed', 'Thurs', 'Fri', 'Sat', 'Sun'))
hourly <- group_by(trips_df, sHour, sDay)
#daily <- group_by(trips_df,sDay,sHour)

per_hour<- dplyr::summarize(hourly, departures = n())
#per_day<-dplyr::summarize(daily,departures = n())

#fixed bug x-ticks not showing by factorizing sHour
#also factorize sDay

hourlyPlot<-ggplot(hourly, aes(x = as.factor(sHour), y = departures, colour = sDay), ordered = TRUE) +
  geom_point(data = per_hour, aes(group = sDay)) +
  geom_line(data = per_hour, aes(group = sDay)) +
  theme_hc(bgcolor = "darkunica") +
  scale_colour_hc("darkunica")+
  scale_x_discrete() +
  scale_y_continuous(breaks = seq(0,5000,500))+
  xlab('Hour')+
  ylab('Trips')+
  ggtitle('Hourly trips across days')+
  theme(axis.title = element_text(size=14), 
        axis.text = element_text(size=10, face="bold"), 
        title = element_text(size=16, vjust = 0.5))+
  ggsave("plots/Hourly trips across days.png")

#------------hourly trips in weekdays and weekends separated, by agegroups------------------------------

ageHourly <- group_by(trips_df[trips_df$weekday==TRUE,], sHour, Agecat1)
#daily <- group_by(trips_df,sDay,sHour)

per_ageHourly<- dplyr::summarize(ageHourly, departures = n())
#per_day<-dplyr::summarize(daily,departures = n())

#fixed bug x-ticks not showing by factorizing sHour
#also factorize sDay

weekPlot<-ggplot(ageHourly, aes(x = as.factor(sHour), y = departures, colour = Agecat1), ordered = TRUE) +
  geom_point(data = per_ageHourly, aes(group = Agecat1)) +
  geom_line(data = per_ageHourly, aes(group = Agecat1)) +
  theme_hc(bgcolor = "darkunica") +
  scale_colour_hc("darkunica")+
  scale_x_discrete() +
  scale_y_continuous(breaks = seq(0,20000,2000))+
  xlab('Hour')+
  ylab('Trips')+
  ggtitle('Hourly trips by age, by members, mon-fri')+
  theme(axis.title = element_text(size=14), 
        axis.text = element_text(size=10, face="bold"), 
        title = element_text(size=14, vjust = 0.5))+
  ggsave("plots/Hourly_by_age.png")


#-----------------------------hourly trips in weekends, across agegroups-----------
ageHourlyWeekend <- group_by(trips_df[trips_df$weekday==FALSE,], sHour, Agecat1)
#daily <- group_by(trips_df,sDay,sHour)

per_ageHourlyWeekend<- dplyr::summarize(ageHourlyWeekend, departures = n())
#per_day<-dplyr::summarize(daily,departures = n())

#fixed bug x-ticks not showing by factorizing sHour
#also factorize sDay

weekPlot<-ggplot(ageHourlyWeekend, aes(x = as.factor(sHour), y = departures, colour = Agecat1), ordered = TRUE) +
  geom_point(data = per_ageHourlyWeekend, aes(group = Agecat1)) +
  geom_line(data = per_ageHourlyWeekend, aes(group = Agecat1)) +
  theme_hc(bgcolor = "darkunica") +
  scale_colour_hc("darkunica")+
  scale_x_discrete() +
  scale_y_continuous(limits = c(0,1800), breaks = seq(0,1800,200))+
  xlab('Hour')+
  ylab('Trips')+
  ggtitle('Hourly trips by age, by members, weekends')+
  theme(axis.title = element_text(size=14), 
        axis.text = element_text(size=10, face="bold"), 
        title = element_text(size=14))+
  ggsave("plots/Hourly_by_age_weekend.png")

#------------------------hourly across week, only members--------------------
MemberHourlyWeek <- group_by(filter(trips_df, usertype=="Member"), sHour, sDay)
#daily <- group_by(trips_df,sDay,sHour)

per_ageMemberHourlyWeek<- dplyr::summarize(MemberHourlyWeek, departures = n())
#per_day<-dplyr::summarize(daily,departures = n())

#fixed bug x-ticks not showing by factorizing sHour
#also factorize sDay

weekPlot<-ggplot(MemberHourlyWeek, aes(x = as.factor(sHour), y = departures, colour = sDay), ordered = TRUE) +
  geom_point(data = per_ageMemberHourlyWeek, aes(group = sDay)) +
  geom_line(data = per_ageMemberHourlyWeek, aes(group = sDay)) +
  theme_hc(bgcolor = "darkunica") +
  scale_colour_hc("darkunica")+
  scale_x_discrete() +
  scale_y_continuous(limits = c(0,4000), breaks = seq(0,4000,500))+
  xlab('Hour')+
  ylab('Trips')+
  ggtitle('Hourly trips by day, members ')+
  theme(axis.title = element_text(size=14), 
        axis.text = element_text(size=10, face="bold"), 
        title = element_text(size=14))+
  ggsave("plots/Hourly_by_day_member.png")



#---------------------------------------------
#hourly trips by agegroups
#dataprep
StartByAgeGroup<- group_by(trips_df, sHour, Agecat1)
SumStartByAgeGroup <- dplyr::summarize(StartByAgeGroup,rentals = n())

#bug fixed: grouping by agecat1 and then sHour doesn't produce correct plot
#plot

hourlyStartByAge<-ggplot(StartByAgeGroup, aes(x=as.factor(sHour), y = rentals, colour = Agecat1, group = Agecat1))+
  geom_point(data = SumStartByAgeGroup, aes(group = Agecat1)) +
  geom_line(data = SumStartByAgeGroup, aes(group = Agecat1)) +
  theme_hc(bgcolor = "darkunica") +
  scale_colour_hc("darkunica")+
  scale_x_discrete() +
  scale_y_continuous(breaks = seq(0,20000,2000))+
  xlab('Hour')+
  ylab('Trips')+

  ggtitle('Hourly trips across agegroups')+
  theme(axis.title = element_text(size=18), 
        axis.text = element_text(size=14, face="bold"), 
        title = element_text(size=22))+
  ggsave("plots/Hourly trips across agegroups.png")


#------------------------Usertype-----------------
#hourly trips by usertype
#dataprep
ByUserType <- group_by(trips_df,sHour, usertype) #%>% filter(!is.na(usertype))
SumByUserType<-dplyr::summarize(ByUserType,rentals = n())
#plot
UserTypePlot<-ggplot(ByUserType, aes(x = as.factor(sHour), y = rentals, colour = usertype))+
  geom_line(data = SumByUserType, aes(group = usertype))+
  geom_point(data = SumByUserType, aes(group = usertype)) +
  theme_hc(bgcolor = "darkunica") +
  scale_colour_hc("darkunica")+
  scale_x_discrete()+
  scale_y_continuous(limits = c(0,20000), breaks = seq(0,20000,2000))+
  xlab('Hour')+
  ylab('rentals')+
  theme(axis.title = element_text(size=14), 
        axis.text = element_text(size=8, face="bold"), 
        title = element_text(size=14, vjust = 0.5))+
  ggtitle('Hourly trips across usertype')+
  ggsave("plots/Hourly_across_usertype.png")

#----------------------hourly trips by season----------------------

#hourly trips by season
#dataprep
BySeason <- group_by(trips_df,sHour, season) #%>% filter(!is.na(usertype))
SumBySeason<-dplyr::summarize(BySeason,rentals = n())
#plot
SeasonPlot<-ggplot(BySeason, aes(x = as.factor(sHour), y = rentals, colour = season))+
  geom_line(data = SumBySeason, aes(group = season))+
  geom_point(data = SumBySeason, aes(group = season)) +
  scale_x_discrete()+
  scale_y_continuous()+
  xlab('Hour')+
  ylab('rentals')+
  theme(axis.title = element_text(size=18), 
        axis.text = element_text(size=14, face="bold"), 
        title = element_text(size=22))+
  ggtitle('Hourly trips across seasons')+
  ggsave("plots/Hourly trips across season.png")

#----------------------Weather----------------------


#hourly trips by member across weather, 
#notes: 
#dataprep
hourlyMemberByWeather <- group_by(trips_members_df,sHour, Events) #%>% filter(!is.na(usertype))
hourlySumMemberByWeather<-dplyr::summarize(hourlyMemberByWeather,Trips = n())
#add number of days with types of weather
hourlySumMemberByWeather$totalDays<-weatherSum$numberOfDays[match(hourlySumMemberByWeather$Events,weatherSum$Events)]
#plot
hourlyPlotMemberByWeather<-ggplot(hourlyMemberByWeather, aes(x = as.factor(sHour), y = Trips, colour = Events))+
  geom_line(data = hourlySumMemberByWeather, aes(group = Events))+
  geom_point(data = hourlySumMemberByWeather, aes(group = Events)) +
  theme_hc(bgcolor = "darkunica") +
  scale_colour_hc("darkunica")+
  scale_x_discrete()+
  scale_y_continuous()+
  #limits = c(-5,5), breaks = seq(-5,5,2)
  xlab('Hour')+
  ylab('Trips')+
  theme(axis.title = element_text(size=14), 
        axis.text = element_text(size=8, face="bold"), 
        title = element_text(size=14))+
  ggtitle('Trips by members in different weather')+
  ggsave("plots/Hourly_weather_member.png")


#----------------------Weather averaged----------------------

#hourly trips by members by weather, normalized to number of days with each type of weather
#notes: 
#dataprep
hourlyMemberByWeather <- group_by(trips_members_df,sHour, Events) #%>% filter(!is.na(usertype))
hourlySumMemberByWeather<-dplyr::summarize(hourlyMemberByWeather,Trips = n())
#add number of days with types of weather
hourlySumMemberByWeather$totalDays<-weatherSum$numberOfDays[match(hourlySumMemberByWeather$Events,weatherSum$Events)]
#plot
hourlyPlotMemberByWeather<-ggplot(hourlyMemberByWeather, 
                                  aes(x = as.factor(sHour), 
                                      y = Trips/totalDays, 
                                      colour = Events))+
  geom_line(data = hourlySumMemberByWeather, aes(group = Events))+
  geom_point(data = hourlySumMemberByWeather, aes(group = Events)) +
  theme_hc(bgcolor = "darkunica") +
  scale_colour_hc("darkunica")+
  scale_x_discrete()+
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30,5))+
  xlab('Hour')+
  ylab('Trips')+
  theme(axis.title = element_text(size=14), 
        axis.text = element_text(size=8, face="bold"), 
        title = element_text(size=14,  vjust = 0.5))+
  ggtitle('Trips by members, across weather')+
  ggsave("plots/Hourly_weather_member_ave.png")

trips_nonmembers$Events <- ifelse(trips_nonmembers$Events=="", "Clear", ifelse(trips_nonmembers$Events=="Fog","Fog", "Rain, snow or thunderstorm or all"))

#Edit values of WeatherSum
weatherSum$X<-as.character(weatherSum$X)
weatherSum$Events<-as.character(weatherSum$Events)
clearRow <- c("Clear", "Clear", weatherSum[weatherSum$Events=="", "numberOfDays"])
comboRow <- c("Rain, snow or thunderstorm or all", "Rain, snow or thunderstorm or all", sum(weatherSum[weatherSum$Events!="" & weatherSum$Events!="Fog", "numberOfDays"]))
weatherSum<-rbind(weatherSum, clearRow, comboRow)

hourlyNonmemberByWeather <- group_by(trips_nonmembers, sHour, Events) %>% dplyr::summarise(rentals = n())
hourlyNonmemberByWeather$totalDays <- as.numeric(weatherSum$numberOfDays[match(hourlyNonmemberByWeather$Events, weatherSum$Events)])
hourlyPlotNonmemberByWeather<-ggplot(hourlyNonmemberByWeather, aes(x = as.factor(sHour), y = rentals/totalDays, colour = Events))+
  geom_line(data = hourlyNonmemberByWeather, aes(group = Events), size=1.1)+
  geom_point(data = hourlyNonmemberByWeather, aes(group = Events), size=1.9) +
  scale_x_discrete()+
  scale_y_continuous()+
  xlab('Hour')+
  ylab('Rentals')+
  ggtitle('Trips by Short-Term Pass Holders, across weather')+
  theme(axis.title = element_text(size=18), 
        axis.text = element_text(size=14, face="bold"), 
        title = element_text(size=22), legend.text = element_text(size=14))+
  theme_hc(bgcolor = "darkunica") +
  scale_colour_hc("darkunica")
#!!!!!!!!!!! plot this over different agegroups to see if there's any difference?

#-------------------------------------timedifferences-------------------------------------
#plot over timediff vs userage and times, might be most reasonable to use mean of timediff?
#dataprep
TimeDiffVsAge <- group_by(filter(trips_df,Agecat2!="NA", timeDiff<=480),sHour, Agecat2) #%>% filter(!is.na(usertype))
SumByTimeAge<-dplyr::summarize(TimeDiffVsAge, median = median(timeDiff))
#plot
TimeDiffByAgePlot<-ggplot(TimeDiffVsAge, aes(x = as.factor(sHour), y = median/60, colour = Agecat2))+
  geom_line(data = SumByTimeAge, aes(group = Agecat2))+
  geom_point(data = SumByTimeAge, aes(group = Agecat2)) +
  theme_hc(bgcolor = "darkunica") +
  scale_colour_hc("darkunica")+
  scale_x_discrete()+
  scale_y_continuous(limits = c(-5,5), breaks = seq(-5,5,2))+
  xlab('Hour')+
  ylab('Median of timedifference, in minutes')+
  theme(axis.title = element_text(size=14), 
        axis.text = element_text(size=8, face="bold"), 
        title = element_text(size=14))+
  ggtitle('Time differences by agecategories')+
  ggsave("plots/Time_diff_by_age.png")


#-------------------------------------Day of the week, non-member-------------------------------------

TimeDiffVsDayBoth<- group_by(trips_df, sHour, sDay)
SumByTimeDayBoth<- dplyr::summarize(TimeDiffVsDayBoth, median = median(timeDiff))
#plot
ByTimeDiffPlotBoth<- ggplot(TimeDiffVsDayBoth, aes(x = as.factor(sHour), y = median/60, colour = sDay))+
  geom_line(data = SumByTimeDayBoth, aes(group = sDay))+
  geom_point(data = SumByTimeDayBoth, aes(group = sDay))+
  theme_hc(bgcolor = "darkunica") +
  scale_colour_hc("darkunica")+
  scale_x_discrete()+
  scale_y_continuous(limits = c(-4,14), breaks = seq(-4,14,2))+
  xlab('Hour')+
  ylab('Timedifference in minutes')+
  theme(axis.title = element_text(size=14), 
        axis.text = element_text(size=8, face="bold"), 
        title = element_text(size=14, vjust = 0.5))+
  ggtitle('Time difference by day, both usertypes')+
  ggsave("plots/Time_diff_by_day_both.png")

#-------------------------------------Usertype-------------------------------------

#Plot over timediff vs usertype and times
#dataprep
TimeDiffVsUser<- group_by(trips_df, sHour, usertype)
SumByTimeUser<- dplyr::summarize(TimeDiffVsUser, median = median(timeDiff))
#plot
ByTimeUserPlot<- ggplot(TimeDiffVsUser, aes(x = as.factor(sHour), y = median/60, colour = usertype))+
  geom_line(data = SumByTimeUser, aes(group = usertype))+
  geom_point(data = SumByTimeUser, aes(group = usertype))+
  theme_hc(bgcolor = "darkunica") +
  scale_colour_hc("darkunica")+
  scale_x_discrete()+
  scale_y_continuous(limits = c(-6,14), breaks = seq(-6,14,2))+
  xlab('Hour')+
  ylab('Timedifference in minutes')+
  theme(axis.title = element_text(size=14), 
        axis.text = element_text(size=8, face="bold"), 
        title = element_text(size=14))+
  ggtitle('Time difference by usertypes')+
  ggsave("plots/Time_diff_by_usertype.png")

#-------------------------------------Day of the week, member-------------------------------------
#Plot over timediff vs day and times, member
#dataprep
TimeDiffVsDayM<- group_by(filter(trips_df,usertype=="Member"), sHour, weekday)
SumByTimeDayM<- dplyr::summarize(TimeDiffVsDayM, median = median(timeDiff))
#plot
ByTimeUserPlot<- ggplot(TimeDiffVsDayM, aes(x = as.factor(sHour), y = median/60, colour = weekday))+
  geom_line(data = SumByTimeDayM, aes(group = weekday))+
  geom_point(data = SumByTimeDayM, aes(group = weekday))+
  theme_hc(bgcolor = "darkunica") +
  scale_colour_hc("darkunica")+
  scale_x_discrete()+
  scale_y_continuous(limits = c(-5,5), breaks = seq(-5,5,1))+
  xlab('Hour')+
  ylab('Timedifference in minutes')+
  theme(axis.title = element_text(size=14), 
        axis.text = element_text(size=8, face="bold"), 
        title = element_text(size=14))+
  ggtitle('Time difference by type of day, members')+
  ggsave("plots/Time_diff_by_day_members.png")


#-------------------------------------Day of the week, non-member-------------------------------------

TimeDiffVsDayN<- group_by(filter(trips_df,usertype=="Short-Term Pass Holder"), sHour, weekday)
SumByTimeDayN<- dplyr::summarize(TimeDiffVsDayN, median = median(timeDiff))
#plot
ByTimeUserPlot<- ggplot(TimeDiffVsDayN, aes(x = as.factor(sHour), y = median/60, colour = weekday))+
  geom_line(data = SumByTimeDayN, aes(group = weekday))+
  geom_point(data = SumByTimeDayN, aes(group = weekday))+
  theme_hc(bgcolor = "darkunica") +
  scale_colour_hc("darkunica")+
  scale_x_discrete()+
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20,2))+
  xlab('Hour')+
  ylab('Timedifference in minutes')+
  theme(axis.title = element_text(size=14), 
        axis.text = element_text(size=8, face="bold"), 
        title = element_text(size=14, vjust = 0.5))+
  ggtitle('Time difference by type of day, non-member')+
  ggsave("plots/Time_diff_by_day_nonmember.png")






#-----------------------Timedifference for trips by member and weather-----------------------------

#plot over timedifference for trips by members and weather
#dataprep
TimeDiffMemberWeather<-group_by(trips_members_df, sHour, Events)
DiffMeanMemberByWeather<-dplyr::summarize(TimeDiffMemberWeather, median = median(timeDiff))

#plot
MemberVsWeather<-ggplot(TimeDiffMemberWeather, aes(x = Events, y = median/60))+
  geom_line(data = DiffMeanMemberByWeather, aes(group = Events))+
  geom_point(data = DiffMeanMemberByWeather, aes(group = Events))+
  scale_x_discrete()+
  theme_hc(bgcolor = "darkunica") +
  scale_colour_hc("darkunica")+
  xlab('Hour')+
  ylab('mean of timediff')+
  theme(axis.title = element_text(size=14), 
        axis.text = element_text(size=8, face="bold"), 
        title = element_text(size=14, vjust = 0.5))+
  ggtitle("time differences for members by weather")+
  ggsave("plots/Time differences for members by weather.png")

#plot over timedifference for trips by non-members and weather
#dataprep
trips_nonMembers<-filter(trips_df, usertype=="Short-Term Pass Holder")
TimeDiffNonMemberVsWeather<-group_by(trips_nonMembers, sHour, Events)
DiffMeanNonMemberByWeather<-dplyr::summarize(TimeDiffNonMemberVsWeather, mean = mean(timeDiff))
#plot
NonMemberVsWeather<-ggplot(TimeDiffNonMemberVsWeather, aes(x = as.factor(sHour), y = mean, colour = Events))+
  geom_line(data = DiffMeanNonMemberByWeather, aes(group = Events))+
  geom_point(data = DiffMeanNonMemberByWeather, aes(group = Events))+
  scale_x_discrete()+
  xlab('Hour')+
  ylab('mean of timediff')+
  theme_minimal()+
  ggtitle('time differences for non-membersmembers by weather')+
  ggsave("plots/Time differences for non-members by weather.png")



