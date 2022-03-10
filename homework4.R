##### Homework 4 ----

##### Question 1 ----

# Part 1: Ensuring that there are no issues with bird excrement or frozen
# precipitation 
weather <- read.csv("/cloud/project/activity04/campus_weather.csv", 
                    na.strings = "#N/A")

metaDat <- read.csv("/cloud/project/activity04/meter_weather_metadata.csv",
                    na.strings = "#N/A")

sensorLog <- read.csv("/cloud/project/activity04/Sensor log.csv",
                      na.strings = "#N/A")

install.packages(c("dplyr","ggplot2","lubridate"))

library(dplyr)
library(ggplot2)
library(lubridate)

# parse date
weather$dateF <- mdy_hm(weather$Date)
# set up intervals
intervals <- weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1]

# calculate interval times
interval_times <- int_length(intervals)
# check interval times
intervals[interval_times != 900]

timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
  
}

timeCheck900(weather$dateF)

# Part 2: Excluding any precipitation occurring in temperatures below zero

weather$AirTemp.QC <- ifelse(weather$AirTemp < 0, NA, weather$AirTemp)

# Part 3: Not using X and Y level observations that are more than 2 degrees

weather$XLevel.QC <- ifelse(weather$XLevel > 2, NA, weather$XLevel)

weather$YLevel.QC <- ifelse(weather$YLevel > 2, NA, weather$YLevel)

# Part 4: Determine how many missing precipitation values are in your data

weather$Precip.QC <- is.na(weather$Precip)

precipitation <- sample(c(TRUE, FALSE), 40850, rep = TRUE)
sum(precipitation)

##### Question 2 ----

weather$BatVoltFlag <- ifelse(weather$BatVolt < 8.5, 1, 0)
View(weather)

##### Question 3 ----

# unrealistic data ranges in air temperature
ggplot(data=weather) +
  aes(x = AirTemp) +
  geom_histogram()

weather$unrealistic_temps <- ifelse(weather$AirTemp < -20 & weather$AirTemp > 30,
                            NA, weather$AirTemp)

ifelse(weather$AirTemp < 0 | weather$SolRad == 0, NA, weather$AirTemp | 
         weather$SolRad)

temperaturecheck <- function(x) {
  check <- ifelse(x < 0 | x == 0, NA, x | x)
  check[x < 0 | x == 0]
}

temperaturecheck(weather$SolRad)

##### Question 4 ----

# parse date
weather$dateF <- mdy_hm(weather$Date)
# create a month column
weather$doy <- yday(weather$dateF)
# create a year column
weather$year <- year(weather$dateF)

ggplot(data=weather[weather$doy >= 1 & weather$doy <= 90 & weather$year == 
                      "2021",],
       aes(x=dateF,
           y=AirTemp)) +
  geom_col(color="royalblue4") +
  theme_classic()

ggplot(data=weather[weather$doy >= 1 & weather$doy <= 90 & weather$year == 
                      "2021",],
       aes(x=dateF,
           y=AirTemp.QC)) +
  geom_col(color="royalblue4") +
  theme_classic()
       

  





