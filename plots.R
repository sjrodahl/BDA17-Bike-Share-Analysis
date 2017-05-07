####

# BDA17 - Bike Share Analysis
# Sondre Rodahl and Tor Magnus Clemens Kvinnsland Michaelsen

####

library(ggplot2)

trip.df <- read.csv("trip.csv")

ggplot(data=subset(trip.df, gender!='')) + geom_histogram(aes(x=gender) , stat="count")

ggplot(data=female.df) + geom_point(aes(x=tripduration, y = starttime))

males.df <- subset(trip.df, gender=="Male")
females.df <- subset(trip.df, gender =="Female")