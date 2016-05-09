setwd("/Users/swolf/Desktop")

#############
# USDT PREP #
#############

# Download monthly 2015 data from: http://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time
# reading in all months from 2015
Jan15 <- read.csv("USDT stats/1014725994_T_ONTIME_2015_1.csv")
Feb15 <- read.csv("USDT stats/1014725994_T_ONTIME_2015_2.csv")
Mar15 <- read.csv("USDT stats/1014725994_T_ONTIME_2015_3.csv")
Apr15 <- read.csv("USDT stats/1014725994_T_ONTIME_2015_4.csv")
May15 <- read.csv("USDT stats/1014725994_T_ONTIME_2015_5.csv")
Jun15 <- read.csv("USDT stats/1014725994_T_ONTIME_2015_6.csv")
Jul15 <- read.csv("USDT stats/1014725994_T_ONTIME_2015_7.csv")
Aug15 <- read.csv("USDT stats/1014725994_T_ONTIME_2015_8.csv")
Sep15 <- read.csv("USDT stats/1014725994_T_ONTIME_2015_9.csv")
Oct15 <- read.csv("USDT stats/1014725994_T_ONTIME_2015_10.csv")
Nov15 <- read.csv("USDT stats/1014725994_T_ONTIME_2015_11.csv")
Dec15 <- read.csv("USDT stats/1014725994_T_ONTIME_2015_12.csv")

# joining all of the tables
usdt2015 <- rbind(Jan15, Feb15)
usdt2015 <- rbind(usdt2015, Mar15)
usdt2015 <- rbind(usdt2015, Apr15)
usdt2015 <- rbind(usdt2015, May15)
usdt2015 <- rbind(usdt2015, Jun15)
usdt2015 <- rbind(usdt2015, Jul15)
usdt2015 <- rbind(usdt2015, Aug15)
usdt2015 <- rbind(usdt2015, Sep15)
usdt2015 <- rbind(usdt2015, Oct15)
usdt2015 <- rbind(usdt2015, Nov15)
usdt2015 <- rbind(usdt2015, Dec15)

# removing redundant tables
rm (Jan15, Feb15, Mar15, Apr15, May15, Jun15, Jul15, Aug15, Sep15, Oct15, Nov15, Dec15)

# Formatting variables where necessary
usdt2015$MONTH <- as.factor(usdt2015$MONTH)
usdt2015$DAY_OF_WEEK <- as.factor(usdt2015$DAY_OF_WEEK)
usdt2015$ARR_DEL15 <- as.factor(usdt2015$ARR_DEL15)

# creating "scheduled hour of flight" variable - closest hour
# Formatting time for hour and minute extraction
usdt2015$Time_4 <- sprintf("%04d",usdt2015$CRS_DEP_TIME) # fix to 4 characters 
# Extracting hours
usdt2015$HOUR <- substr(usdt2015$Time_4, 1, nchar(usdt2015$Time_4)-2)
usdt2015$HOUR <- as.numeric(usdt2015$HOUR)
# Extracting minutes
usdt2015$MINUTE <- substr(usdt2015$Time_4, 3, 4)
usdt2015$MINUTE <- as.numeric(usdt2015$MINUTE)
usdt2015$Time_4 <- NULL
# Calculating closest hour of planned departure
usdt2015$closest_hour <- NULL
usdt2015$closest_hour[usdt2015$MINUTE < 30] <- usdt2015$HOUR[usdt2015$MINUTE < 30]
usdt2015$closest_hour[usdt2015$MINUTE >= 30 & usdt2015$HOUR != 23] <- usdt2015$HOUR[usdt2015$MINUTE >= 30 & usdt2015$HOUR != 23] + 1
usdt2015$closest_hour[usdt2015$MINUTE >= 30 & usdt2015$HOUR == 23] <- 0
usdt2015$closest_hour <- as.factor(usdt2015$closest_hour)

# formatting date variable
usdt2015$Date <- gsub("-","", usdt2015$FL_DATE)
usdt2015$FL_DATE <- NULL

# creating an "airport 'busyness' variable" by hour/airport
flights_count <- aggregate(usdt2015$FLIGHTS, by=list(usdt2015$Date, usdt2015$closest_hour, usdt2015$ORIGIN), FUN=sum, na.rm=TRUE)

# renaming variables
names(flights_count) <- c("Date", "closest_hour", "ORIGIN", "number_flights")

# joining flights_count with larger dataset
usdt2015 <- merge(x = usdt2015, y = flights_count, by=c("Date","closest_hour", "ORIGIN"), all.x = TRUE)
usdt2015$X <- NULL

# reducing dataset to include only flights that ORIGINATED in NYC
usdt2015_nyc_origin <- usdt2015[ which(usdt2015$ORIGIN == 'EWR' | usdt2015$ORIGIN == 'JFK' | usdt2015$ORIGIN == 'LGA'), ]

# reducing dataset to include only flights that go to LA
usdt2015_nyc_to_la <- usdt2015_nyc_origin[ which(usdt2015_nyc_origin$DEST == 'LAX' | 
                                                   usdt2015_nyc_origin$DEST == 'ONT' | 
                                                   usdt2015_nyc_origin$DEST == 'SNA' |
                                                   usdt2015_nyc_origin$DEST == "BUR" |
                                                   usdt2015_nyc_origin$DEST == "LGB"), ]

# file clean-up
usdt2015_nyc_to_la$QUARTER <- NULL
usdt2015_nyc_to_la$AIRLINE_ID <- NULL
usdt2015_nyc_to_la$FL_NUM <- NULL  
usdt2015_nyc_to_la$ORIGIN_AIRPORT_ID <- NULL  
usdt2015_nyc_to_la$ORIGIN_CITY_MARKET_ID <- NULL	
usdt2015_nyc_to_la$ORIGIN_CITY_NAME <- NULL
usdt2015_nyc_to_la$DEST_CITY_NAME <- NULL
usdt2015_nyc_to_la$DEST_AIRPORT_ID <- NULL
usdt2015_nyc_to_la$DEST_CITY_MARKET_ID <- NULL
usdt2015_nyc_to_la$DEP_TIME_BLK <- NULL
usdt2015_nyc_to_la$ARR_TIME_BLK <- NULL

# clearing out unnecessary dataframes
rm(usdt2015, usdt2015_nyc_origin, flights_count)




################
# WEATHER PREP #
################

setwd("/Users/swolf/Desktop")

# reading in all months from 2015
# https://www.ncdc.noaa.gov/qclcd/QCLCD
nyc_weather <- read.csv("NYC_central_park_hourly.csv")

nyc_weather$YEAR<-as.numeric(substr(as.numeric(nyc_weather$Date),1,4))
nyc_weather$MONTH<-as.numeric(substr(as.numeric(nyc_weather$Date),5,6))
nyc_weather$DAY_OF_MONTH <- as.numeric(substr(as.numeric(nyc_weather$Date),7,8))

# Formatting time for hour and minute extraction
nyc_weather$Time_4 <- sprintf("%04d",nyc_weather$Time) # fix to 4 characters 

# Extracting hours
nyc_weather$HOUR <- substr(nyc_weather$Time_4, 1, nchar(nyc_weather$Time_4)-2)
nyc_weather$HOUR <- as.numeric(nyc_weather$HOUR)

# Extracting minutes
nyc_weather$MINUTE <- substr(nyc_weather$Time_4, 3, 4)
nyc_weather$MINUTE <- as.numeric(nyc_weather$MINUTE)

# Calculating closest hour of temp reading
nyc_weather$closest_hour <- NULL
nyc_weather$closest_hour[nyc_weather$MINUTE < 30] <- nyc_weather$HOUR[nyc_weather$MINUTE < 30]
nyc_weather$closest_hour[nyc_weather$MINUTE >= 30 & nyc_weather$HOUR != 23] <- nyc_weather$HOUR[nyc_weather$MINUTE >= 30 & nyc_weather$HOUR != 23] + 1
nyc_weather$closest_hour[nyc_weather$MINUTE >= 30 & nyc_weather$HOUR == 23] <- 0

# Reducing dataset
nyc_weather$WBAN <- NULL
nyc_weather$StationType <- NULL  
nyc_weather$SkyCondition <- NULL	
nyc_weather$SkyConditionFlag <- NULL		
nyc_weather$VisibilityFlag <- NULL	
nyc_weather$WeatherType <- NULL	
nyc_weather$WeatherTypeFlag <- NULL	
nyc_weather$DryBulbFarenheitFlag <- NULL	
nyc_weather$DryBulbCelsiusFlag <- NULL	
nyc_weather$WetBulbFarenheitFlag <- NULL	
nyc_weather$WetBulbCelsiusFlag <- NULL	
nyc_weather$DewPointFarenheitFlag <- NULL
nyc_weather$DewPointCelsiusFlag <- NULL	
nyc_weather$RelativeHumidityFlag <- NULL	
nyc_weather$WindSpeedFlag <- NULL	
nyc_weather$WindDirection <- NULL	
nyc_weather$WindDirectionFlag <- NULL	
nyc_weather$ValueForWindCharacter <- NULL	
nyc_weather$ValueForWindCharacterFlag <- NULL	
nyc_weather$StationPressureFlag <- NULL	
nyc_weather$PressureTendency <- NULL	
nyc_weather$PressureTendencyFlag <- NULL	
nyc_weather$PressureChange <- NULL	
nyc_weather$PressureChangeFlag <- NULL	
nyc_weather$SeaLevelPressureFlag <- NULL	
nyc_weather$RecordType <- NULL	
nyc_weather$RecordTypeFlag <- NULL	
nyc_weather$HourlyPrecipFlag <- NULL	
nyc_weather$AltimeterFlag <- NULL	
nyc_weather$YEAR <- NULL	
nyc_weather$Time_4 <- NULL

nyc_weather$DryBulbCelsius <- NULL  
nyc_weather$WetBulbFarenheit <- NULL  
nyc_weather$WetBulbCelsius <- NULL
nyc_weather$DewPointCelsius <- NULL
nyc_weather$Altimeter <- NULL
nyc_weather$SeaLevelPressure <- NULL

# Replacing "M" (missing) with "NA"
nyc_weather[nyc_weather == "M"] <- NA

# Replacing "  T" (trace) with .005
levels(nyc_weather$HourlyPrecip)[levels(nyc_weather$HourlyPrecip)=="  T"] <- ".005"

# Reformatting all variables to numeric
options(digits=9)
nyc_weather$Visibility <- type.convert(as.character(nyc_weather$Visibility))  
nyc_weather$DryBulbFarenheit <- type.convert(as.character(nyc_weather$DryBulbFarenheit))  	
nyc_weather$WindSpeed <- type.convert(as.character(nyc_weather$WindSpeed))  	
nyc_weather$HourlyPrecip <- type.convert(as.character(nyc_weather$HourlyPrecip))  	

# inputting "0" instead of NA for HourlyPrecip where there was nothing
nyc_weather$HourlyPrecip[is.na(nyc_weather$HourlyPrecip)] <- 0

# File clean-up
nyc_weather$MONTH <- NULL
nyc_weather$DAY_OF_MONTH <- NULL
nyc_weather$HOUR <- NULL
nyc_weather$MINUTE <- NULL

# Aggregates hours for which there are multiple readings (should give ~ 1 reading for each hour, 24*365 = 8760)
# Missing 9 readings over the course of the year [seems to be close enough]
nyc_weather_aggregated <-aggregate(nyc_weather, by=list(nyc_weather$Date, nyc_weather$closest_hour), 
                                   FUN=mean, na.rm=TRUE)

# clean up
rm(nyc_weather)

################
# JOINING DATA #
################

merged <- merge(x = usdt2015_nyc_to_la, y = nyc_weather_aggregated, by=c("Date","closest_hour"), all.x = TRUE)

# getting rid of extra categories in airport codes
merged$ORIGIN <- factor(merged$ORIGIN)
merged$DEST <- factor(merged$DEST)

# getting rid of extra categories in UNIQUE_CARRIERS
merged$UNIQUE_CARRIER <- factor(merged$UNIQUE_CARRIER)

# clean up
merged$X <- NULL
merged$hour <- NULL  
merged$Group.1 <- NULL
merged$Group.2 <- NULL
merged$Date <- NULL
merged$YEAR <- NULL
merged$DAY_OF_MONTH <- NULL
merged$CRS_DEP_TIME <- NULL
merged$DEP_TIME <- NULL
merged$DEP_DELAY <- NULL
merged$DEP_DELAY_GROUP <- NULL
merged$DEP_DELAY_NEW <- NULL
merged$DEP_DEL15 <- NULL
merged$TAXI_OUT <- NULL
merged$WHEELS_OFF <- NULL
merged$WHEELS_ON <- NULL
merged$TAXI_IN <- NULL
merged$CRS_ARR_TIME <- NULL
merged$ARR_TIME <- NULL
merged$ACTUAL_ELAPSED_TIME <- NULL
merged$CANCELLED <- NULL
merged$CANCELLATION_CODE <- NULL
merged$DIVERTED <- NULL
merged$CRS_ELAPSED_TIME <- NULL
merged$AIR_TIME <- NULL
merged$FLIGHTS <- NULL
merged$DISTANCE <- NULL
merged$DISTANCE_GROUP <- NULL
merged$DewPointFarenheit <- NULL
merged$RelativeHumidity <- NULL
merged$StationPressure <- NULL
merged$CARRIER_DELAY <- NULL
merged$WEATHER_DELAY <- NULL
merged$NAS_DELAY <- NULL
merged$SECURITY_DELAY <- NULL
merged$LATE_AIRCRAFT_DELAY <- NULL
merged$CARRIER_DELAY_binary <- NULL
merged$WEATHER_DELAY_binary <- NULL
merged$NAS_DELAY_binary <- NULL
merged$SECURITY_DELAY_binary <- NULL
merged$LATE_AIRCRAFT_DELAY_binary <- NULL
merged$total_delay_0 <- NULL
merged$total_delay_negs <- NULL
merged$ARR_DELAY <- NULL
merged$ARR_DELAY_NEW <- NULL
merged$ARR_DELAY_GROUP <- NULL
merged$HOUR <- NULL
merged$MINUTE <- NULL
merged$Time <- NULL

rm(nyc_weather_aggregated, usdt2015_nyc_to_la)

# getting rid of observations with NAs
merged_complete <- merged[!is.na(merged$ARR_DEL15) & 
                                        !is.na(merged$MONTH) & 
                                        !is.na(merged$DAY_OF_WEEK) &
                                        !is.na(merged$UNIQUE_CARRIER) &
                                        !is.na(merged$ORIGIN) & 
                                        !is.na(merged$DEST) &
                                        !is.na(merged$closest_hour) &
                                        !is.na(merged$Visibility) &
                                        !is.na(merged$DryBulbFarenheit) & 
                                        !is.na(merged$WindSpeed) &
                                        !is.na(merged$HourlyPrecip) &
                                        !is.na(merged$number_flights),]

# removing unnecessary dataset
rm(merged)



#####################
# INFERENTIAL STUFF #
#####################

# creating partitions (80% training, 20% testing)
library(caret)
set.seed(1234)
inTraining <- createDataPartition(merged_complete$MONTH, p=0.8, list=FALSE)
training.set <- merged_complete[inTraining,]
testing.set <- merged_complete[-inTraining,]


# Logistic multiple regression on arr_del15 binary variable
model.null <- glm(ARR_DEL15 ~ 0,
                  family=binomial(link='logit'),data=training.set, na.action=na.exclude)

model.full <- glm(ARR_DEL15 ~  MONTH + DAY_OF_WEEK + 
                    UNIQUE_CARRIER + ORIGIN + DEST + closest_hour + Visibility + 
                    DryBulbFarenheit + WindSpeed + HourlyPrecip + number_flights, 
             family=binomial(link='logit'),data=training.set, na.action=na.exclude)

step(model.null,
     scope = list(upper=model.full),
     direction="both",
     test="Chisq",
     data=training.set_arr_del15
)

model.final <- glm(ARR_DEL15 ~  closest_hour + Visibility + MONTH + DryBulbFarenheit +
                     WindSpeed + UNIQUE_CARRIER + DAY_OF_WEEK + DEST + HourlyPrecip +
                     ORIGIN + number_flights,
                  family=binomial(link='logit'),data=training.set, na.action=na.exclude)
summary(model.final)

# Analysis of variance for individual terms
library(car)
Anova(model.final, type="II", test="Wald")

# Pseudo-R-squared
source("http://rcompanion.org/r_script/nagelkerke.r")
nagelkerke(model.final)

# TRAINING: assessing predictive ability of model
fitted.results <- predict(model.final, newdata = training.set, type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
training.set$log_pred <- fitted.results
misClasificError <- mean(fitted.results != training.set$ARR_DEL15, na.rm=TRUE)
print(paste('Accuracy',1-misClasificError))
# confusion matrix
table(training.set$log_pred, training.set$ARR_DEL15)
prop.table(table(training.set$log_pred, training.set$ARR_DEL15))
prop.table(table(training.set$ARR_DEL15))

# TESTING: assessing predictive ability of model
fitted.results <- predict(model.final, newdata = testing.set, type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
testing.set$log_pred <- fitted.results
misClasificError <- mean(fitted.results != testing.set$ARR_DEL15, na.rm=TRUE)
print(paste('Accuracy',1-misClasificError))
# confusion matrix
table(testing.set$log_pred, testing.set$ARR_DEL15)
prop.table(table(testing.set$log_pred, testing.set$ARR_DEL15))
prop.table(table(testing.set$ARR_DEL15))

library(ROCR)
p <- predict(model.final, newdata=testing.set, type="response", na.action=na.exclude)
pr <- prediction(as.numeric(p), as.numeric(testing.set$ARR_DEL15))
prf <- performance(pr, "tpr", "fpr", na.action=na.exclude)
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc



# Naive Bayes
library("e1071")
model_nb = naiveBayes(training.set[,-7], training.set$ARR_DEL15)
model_nb$levels
model_nb$tables

# TRAINING: predicting test data
predictor=predict(model_nb,training.set[,-7])
# confusion matrix
table(Predicted=as.character(predictor),Actual=as.character(training.set$ARR_DEL15))
prop.table(table(Predicted=as.character(predictor),Actual=as.character(training.set$ARR_DEL15)))

# TESTING: predicting test data
predictor=predict(model_nb,testing.set[,-7])
# confusion matrix
table(Predicted=as.character(predictor),Actual=as.character(testing.set$ARR_DEL15))
prop.table(table(Predicted=as.character(predictor),Actual=as.character(testing.set$ARR_DEL15)))



# Decision tree
library(caret)
modFit_dt <- train(ARR_DEL15 ~ ., method='rpart', data = training.set)
print(modFit_dt$finalModel)
# plot tree
library(rattle)
fancyRpartPlot(modFit_dt$finalModel)

# TRAINING: predicting training set
dt_predictions <- predict(modFit_dt, newdata = training.set)
table(training.set$ARR_DEL15, dt_predictions)
prop.table(table(training.set$ARR_DEL15, dt_predictions))

# TESTING: predicting testing set
dt_predictions <- predict(modFit_dt, newdata = testing.set)
table(testing.set$ARR_DEL15, dt_predictions)
prop.table(table(testing.set$ARR_DEL15, dt_predictions))



# Random forest
library(caret)
library(randomForest)
modFit_forest <- randomForest(y=training.set$ARR_DEL15, x=training.set[,-7], ntree=10000)

# TRAINING SET
prediction <- predict(modFit_forest, training.set[,-7])
table(training.set$ARR_DEL15, prediction)
prop.table(table(training.set$ARR_DEL15, prediction))

# TESTING SET
prediction <- predict(modFit_forest, testing.set[,-7])
table(testing.set$ARR_DEL15, prediction)
prop.table(table(testing.set$ARR_DEL15, prediction))



# Boosting
# GBM
modFit_boost_gbm <- train(ARR_DEL15 ~ ., method = 'gbm', data = training.set, verbose = FALSE)
print(modFit_boost_gbm)

# TRAINING
prediction <- predict(modFit_boost_gbm, training.set[,-7])
table(training.set$ARR_DEL15, prediction)
prop.table(table(training.set$ARR_DEL15, prediction))

# TESTING
prediction <- predict(modFit_boost_gbm, testing.set[,-7])
table(testing.set$ARR_DEL15, prediction)
prop.table(table(testing.set$ARR_DEL15, prediction))



# Boosting
# ADA
modFit_boost_ada <- train(ARR_DEL15 ~ ., method = 'ada', data = training.set, verbose = FALSE)
print(modFit_boost_ada)

# TRAINING
prediction <- predict(modFit_boost_ada, training.set[,-7])
table(training.set$ARR_DEL15, prediction)
prop.table(table(training.set$ARR_DEL15, prediction))

# TESTING
prediction <- predict(modFit_boost_ada, testing.set[,-7])
table(testing.set$ARR_DEL15, prediction)
prop.table(table(testing.set$ARR_DEL15, prediction))


# BASELINE MODEL
prop.table(table(testing.set$ARR_DEL15))

# TRAINING
prediction <- predict(model.null, training.set[,-7])
table(training.set$ARR_DEL15, prediction)
prop.table(table(training.set$ARR_DEL15, prediction))

# TESTING
prediction <- predict(model.null, testing.set[,-7])
table(testing.set$ARR_DEL15, prediction)
prop.table(table(testing.set$ARR_DEL15, prediction))




#####################
# EXPLORATORY STUFF #
#####################
# overall late flights
prop.table(table(merged_complete$ARR_DEL15))

# late arriving flights by ORIGIN
prop.table(table(merged_complete$ARR_DEL15, merged_complete$ORIGIN),2)

# late arriving flights by DEST
prop.table(table(merged_complete$ARR_DEL15, merged_complete$DEST),2)

# late arriving flights by day of the week
prop.table(table(merged_complete$ARR_DEL15, merged_complete$DAY_OF_WEEK),2)

# late arriving flights by month
prop.table(table(merged_complete$ARR_DEL15, merged_complete$MONTH),2)

# delayed flights by hour of day
prop.table(table(merged_complete$ARR_DEL15, merged_complete$closest_hour),2)
aggregate(ARR_DEL15 ~ closest_hour, NROW, data=merged_complete, na.action = na.omit)

# delayed flights by carrier
prop.table(table(merged_complete$ARR_DEL15, merged_complete$UNIQUE_CARRIER),2)

# average # of flights for late/non-late flights
aggregate(number_flights ~ ARR_DEL15, mean, data=merged_complete)

# average visibility for late/non-late flights
aggregate(Visibility ~ ARR_DEL15, mean, data=merged_complete)

# average temp for late/non-late flights
aggregate(DryBulbFarenheit ~ ARR_DEL15, mean, data=merged_complete)

# average wind speed for late/non-late flights
aggregate(WindSpeed ~ ARR_DEL15, mean, data=merged_complete)

# average precipitation for late/non-late flights
aggregate(HourlyPrecip ~ ARR_DEL15, mean, data=merged_complete)
