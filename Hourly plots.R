


#-------------------------------------hourly trips------------------------------
#lineplot over trips by day and time, average over number of days
hourly <- group_by(trips_df, sHour, sDay)
#daily <- group_by(trips_df,sDay,sHour)

per_hour<- dplyr::summarize(hourly, departures = n())
#per_day<-dplyr::summarize(daily,departures = n())

#fixed bug x-ticks not showing by factorizing sHour
hourlyPlot<-ggplot(hourly, aes(x = as.factor(sHour), y = departures/425 , colour = sDay)) +
  geom_point(data = per_hour, aes(group = sDay)) +
  geom_line(data = per_hour, aes(group = sDay)) +
  scale_x_discrete() +
  scale_y_continuous()+
  xlab('Hour')+
  ylab('rentals')+
  ggtitle('Hourly trips across days')+
  theme_minimal()+
  ggsave("plots/Hourly trips across days.png")


#hourly trips by agegroups
#dataprep
StartByAgeGroup<- group_by(trips_df, sHour, Agecat1)
SumStartByAgeGroup <- dplyr::summarize(StartByAgeGroup,rentals = n())

#bug fixed: grouping by agecat1 and then sHour doesn't produce correct plot
#plot
hourlyStartByAge<-ggplot(StartByAgeGroup, aes(x=as.factor(sHour), y = rentals, colour = Agecat1, group = Agecat1))+
  geom_point(data = SumStartByAgeGroup, aes(group = Agecat1)) +
  geom_line(data = SumStartByAgeGroup, aes(group = Agecat1)) +
  scale_x_discrete() +
  scale_y_continuous()+
  xlab('Hour')+
  ylab('rentals')+
  ggtitle('Hourly trips across agegroups')+
  theme_minimal()+
  ggsave("plots/Hourly trips across agegroups.png")


#hourly trips by members
#dataprep
ByUserType <- group_by(trips_df,sHour, usertype) #%>% filter(!is.na(usertype))
SumByUserType<-dplyr::summarize(ByUserType,rentals = n())
#plot
UserTypePlot<-ggplot(ByUserType, aes(x = as.factor(sHour), y = rentals, colour = usertype))+
  geom_line(data = SumByUserType, aes(group = usertype))+
  geom_point(data = SumByUserType, aes(group = usertype)) +
  scale_x_discrete()+
  scale_y_continuous()+
  xlab('Hour')+
  ylab('rentals')+
  theme_minimal() +
  ggtitle('Hourly trips across usertype')+
  ggsave("plots/Hourly trips across usertype.png")



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
  theme_minimal() +
  ggtitle('Hourly trips across seasons')+
  ggsave("plots/Hourly trips across season.png")


#hourly trips by members by weather, normalized to number of days with each type of weather
#dataprep
ByUserType <- group_by(trips_df,sHour, usertype) #%>% filter(!is.na(usertype))
SumByUserType<-dplyr::summarize(ByUserType,rentals = n())
#plot
UserTypePlot<-ggplot(ByUserType, aes(x = as.factor(sHour), y = rentals, colour = usertype))+
  geom_line(data = SumByUserType, aes(group = usertype))+
  geom_point(data = SumByUserType, aes(group = usertype)) +
  scale_x_discrete()+
  scale_y_continuous()+
  xlab('Hour')+
  ylab('rentals')+
  theme_minimal() +
  ggtitle('Hourly trips across usertype in different weather, averaged')+
  ggsave("plots/Hourly trips across usertype, averaged over weather.png")





#-------------------------------------timedifferences-------------------------------------
#plot over timediff vs userage and times, might be most reasonable to use mean of timediff?
#dataprep
TimeDiffVsAge <- group_by(trips_df,sHour, Agecat1) #%>% filter(!is.na(usertype))
SumByTimeAge<-dplyr::summarize(TimeDiffVsAge, mean = mean(timeDiff))
#plot
TimeDiffByAgePlot<-ggplot(TimeDiffVsAge, aes(x = as.factor(sHour), y = mean, colour = Agecat1))+
  geom_line(data = SumByTimeAge, aes(group = Agecat1))+
  geom_point(data = SumByTimeAge, aes(group = Agecat1)) +
  scale_x_discrete()+
  xlab('Hour')+
  ylab('mean of timediff')+
  theme_minimal() +
  ggtitle('Time differences between estimates and actual time by agecategories')+
  ggsave("plots/Time differences by agecategories.png")


#Plot over timediff vs usertype and times
#dataprep
TimeDiffVsUser<- group_by(trips_df, sHour, usertype)
SumByTimeUser<- dplyr::summarize(TimeDiffVsUser, median = median(timeDiff))
#plot
ByTimeUserPlot<- ggplot(TimeDiffVsUser, aes(x = as.factor(sHour), y = median, colour = usertype))+
  geom_line(data = SumByTimeUser, aes(group = usertype))+
  geom_point(data = SumByTimeUser, aes(group = usertype))+
  scale_x_discrete()+
  xlab('Hour')+
  ylab('median of timediff')+
  theme_minimal() +
  ggtitle('Time differences by usertypes')+
  ggsave("plots/Time differences by usertypes.png")


#plot over timedifference for trips by members and weather
#dataprep
trips_members<-filter(trips_df, usertype=="Member")
TimeDiffMemberWeather<-group_by(trips_members, sHour, Events)
DiffMeanMemberByWeather<-dplyr::summarize(TimeDiffVsWeather, mean = mean(timeDiff))

#plot
MemberVsWeather<-ggplot(TimeDiffVsWeather, aes(x = as.factor(sHour), y = mean, colour = Events))+
  geom_line(data = DiffMeanByWeather, aes(group = Events))+
  geom_point(data = DiffMeanByWeather, aes(group = Events))+
  scale_x_discrete()+
  xlab('Hour')+
  ylab('mean of timediff')+
  theme_minimal()+
  ggtitle('time differences for members by weather')+
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



