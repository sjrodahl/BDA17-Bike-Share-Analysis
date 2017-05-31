##### --- Predictive model on tripduration

library(glmnet) # For LASSO model

#Comment out the other user.
#setwd("/Users/tormagnusmichaelsen/Documents/BDA/Project/BDA17-Bike-Share-Analysis/data")
setwd("D:/Sondre/Dokumenter/UCSD/IRGN452 BigDataAnalytics/Project/BDA17-Bike-Share-Analysis/")
#Read necessary data files:
trips<-read.csv("data/trips_processed.csv", nrows=5000) #Set nrows for testing purposes
weather<-read.csv("data/weather_processed.csv")
stations<-read.csv("data/station.csv")


#Make training and validation set
## 80% of the sample size
nobs = nrow(trips) 
smp_size <- floor(0.8 * nobs)

## set seed to make partition reproductible
set.seed(42561)
train_ind <- sample(seq_len(nobs), size = smp_size)

tripsTrain <- trips[train_ind,]
tripsTest <- trips[-train_ind,]

tripsTrainRemoveColumns <- tripsTrain[, c("onlyDate", "tripduration", "from_station_id", "to_station_id", "usertype", "gender", "birthyear", "sDay", "eDay", "weekday", "holiday", "age", "Max_Temperature_F", "Mean_Temperature_F", "Min_TemperatureF", "Max_Dew_Point_F", "MeanDew_Point_F", "Min_Dewpoint_F", "Max_Humidity", "Mean_Humidity", "Min_Humidity", "Max_Sea_Level_Pressure_In", "Mean_Sea_Level_Pressure_In", "Min_Sea_Level_Pressure_In", "Max_Visibility_Miles", "Mean_Visibility_Miles", "Min_Visibility_Miles", "Max_Wind_Speed_MPH", "Mean_Wind_Speed_MPH", "Max_Gust_Speed_MPH", "Precipitation_In", "Events", "Agecat1", "timeEst", "distanceEst")]

tripsTrainNoNa <- na.omit(tripsTrainRemoveColumns)

target = tripsTrainNoNa$tripduration
x = as.data.frame(tripsTrainNoNa[,-2]) #removing tripduration

#Creating LASSO model
model1 <- cv.glmnet(model.matrix(~.,x),target,alpha=1, nlambda =50)
plot(model1)
coef(model1, s = model1.01$lambda.min)

##############################################
##### Model 2: Predicting time difference. ###
##############################################

tripsTrainTimeDiff <- tripsTrain[, c("onlyDate", "from_station_id", "to_station_id", "usertype", "gender", "birthyear", "sDay", "eDay", "weekday", "holiday", "age", "Max_Temperature_F", "Mean_Temperature_F", "Min_TemperatureF", "Max_Dew_Point_F", "MeanDew_Point_F", "Min_Dewpoint_F", "Max_Humidity", "Mean_Humidity", "Min_Humidity", "Max_Sea_Level_Pressure_In", "Mean_Sea_Level_Pressure_In", "Min_Sea_Level_Pressure_In", "Max_Visibility_Miles", "Mean_Visibility_Miles", "Min_Visibility_Miles", "Max_Wind_Speed_MPH", "Mean_Wind_Speed_MPH", "Max_Gust_Speed_MPH", "Precipitation_In", "Events", "Agecat1", "distanceEst", "timeDiff")]
tripsTrainTimeDiffNoNa <- na.omit(tripsTrainTimeDiff)
targetTimeDiff <- tripsTrainTimeDiffNoNa$timeDiff
xTimeDiff <- as.data.frame(tripsTrainTimeDiffNoNa[,-34]) #removin timeDiff

modelTimeDiff <- cv.glmnet(model.matrix(~.,xTimeDiff), targetTimeDiff, alpha = 1, nlambda =50)
plot(modelTimeDiff)
coef(modelTimeDiff, s = modelTimeDiff$lambda.min)

#The result is terrible...