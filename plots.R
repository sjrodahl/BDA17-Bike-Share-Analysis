####

# BDA17 - Bike Share Analysis
# Sondre Rodahl and Tor Magnus Clemens Kvinnsland Michaelsen

####

library(ggplot2)

trip.df <- read.csv("D:/Sondre/Dokumenter/UCSD/IRGN452 BigDataAnalytics/Project/BDA17-Bike-Share-Analysis/data/trip.csv")

#Plot of number of trips split by gender. Observations without he gender variable is omitted. Category "other" is included
#gender-count.png
trip.gender <- subset(trip.df, gender!='')
ggplot(data=trip.gender) + geom_histogram(aes(x=gender),  fill = "#FF9911", colour = "black", stat="count")

males.df <- subset(trip.df, gender=="Male")
females.df <- subset(trip.df, gender =="Female")
