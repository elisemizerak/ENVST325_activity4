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
# create a month column
weather$doy <- yday(weather$dateF)
# create a year column
weather$year <- year(weather$dateF)

# examine precipitation using a bar plot
ggplot(data=weather[weather$doy > 121 & weather$doy < 274 ,],
       aes(x=dateF,
           y=Precip))+
  geom_col(color="royalblue4")+
  theme_classic()


# add a column to weather:
weather$precip.QC <- ifelse(weather$doy >= 121 & weather$doy <= 188 & 
                              weather$year == 2021, 
                            # evaluate if the doy is between May 1 
                            # and July 7 2021
                            NA, # value if true
                            weather$Precip) # value if false: uses original 
# precipitation observation

weather$FreezeFlag <- ifelse(weather$AirTemp <= 0, # check if at or below zero
                             1, # if true: set flag to 1
                             0) # if false: set flag to zero

#interval data: look at first 2 observations as interval
# Time 1 %--% Time 2
weather$dateF[1] %--% weather$dateF[2]

# look at the interval length from the first to the second observation:
int_length(weather$dateF[1] %--% weather$dateF[2])

# set up intervals
intervals <- weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1] # start date %--% end date
# interval starts with row 1, and row 2
# and ends on second to last row and final row

# calculate interval times
interval_times <- int_length(intervals)
# check interval times
intervals[interval_times != 900]

#create function for checking for irregular intervals that
# deviate from 900 seconds
# only argument is x, a vector of POSIXct formatted dates to be checked
# with an expected interval of 900 seconds
# the output is all observations that deviate from 900 seconds
timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
  
}
# run on weather data
timeCheck900(weather$dateF)

#### in class exercises ----

average <- function(x){
  x.no <- na.omit(x)
  sum(x.no)/length(x.no)
}

average(weather$AirTemp)

##### In class prompts ----

# Prompt 1
# You want to see if the solar radiation measurements experienced any issues 
# with build up or accumulation on the sensor in May and June of 2021. 
# Make an assessment with your group.

ggplot(data=weather[weather$doy >= 121 & weather$doy <= 151 ,],
       aes(x=dateF,
           y=SolRad))+
  geom_line() +
  theme_classic()

ggplot(data=weather[weather$doy >= 182 & weather$doy <= 212 ,],
       aes(x=dateF,
           y=SolRad))+
  geom_line() +
  theme_classic()

ggplot(data=weather[weather$doy >= 152 & weather$doy <= 182 ,],
       aes(x=dateF,
           y=SolRad))+
  geom_line() +
  theme_classic()

##### Prompt 2 ----
# Check for any date time issues using the function created in the tutorial. 
# Investigate instances of date time issues. What happens around daylight 
# savings? Are there issues with the time zone assumption?

# intervals

#interval data: look at first 2 observations as interval
# Time 1 %--% Time 2
intv <- weather$dateF[1] %--% weather$dateF[2]
# look at the interval length from the first to the second observation:
int_length(weather$dateF[1] %--% weather$dateF[2])

# set up intervals
intervals <- weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1] # start date %--% end date
# interval starts with row 1, and row 2
# and ends on second to last row and final row
# intervals is always going to be n-1 with n=total # of rows

# calculate interval times
interval_times <- int_length(intervals)
# check interval times
intervals[interval_times != 900]
