#############################################################################################################################
#                                                                                                                           #
#                                                                                                                           #
#                                  ASSIGNMENT - UBER SUPPLY DEMAND GROUP                                                    #
#                                                                                                                         #
#                                                                                                                           #
#                                                                                                                           #
############################################################################################################################


#Extract the data from csv file into a data fram
ubermasterdata <-read.csv("Uber Request Data.csv",stringsAsFactors = FALSE)

str(ubermasterdata) #understanding the structure of the master data

##############################################################
#                                                               #
#                                                               #
#       CLEANING OF THE DATA                                    #
#                                                               #
#################################################################

#create two variables to identify where time is needed to be cleansed as we can see there are lot of time in which seconds are missing and need to get time in correct same format of h m s

ubermasterdata$DifferentRequesttimestamp <- ifelse((str_count( ubermasterdata$Request.timestamp, ":"))> 1,FALSE, TRUE)
ubermasterdata$DifferentDroptimestamp <- ifelse((str_count( ubermasterdata$Drop.timestamp, ":"))> 1,FALSE, TRUE)

#If Drop time is na then treat it as false

ubermasterdata$DifferentDroptimestamp <- ifelse(is.na(ubermasterdata$Drop.timestamp),FALSE,ubermasterdata$DifferentDroptimestamp)

##Now add seconds to all the time stamp where the data is formatted in only hours and minutes so as to make it in similar format
## we will use paste command to add seconds in all the time stamps to make it in one symetric time stamp
Sys.time()

ubermasterdata$Request.timestamp <- paste(ubermasterdata$Request.timestamp,ifelse(ubermasterdata$DifferentRequesttimestamp == TRUE,":00",""),sep = "")
ubermasterdata$Drop.timestamp <- paste(ubermasterdata$Drop.timestamp,ifelse(ubermasterdata$DifferentDroptimestamp == TRUE,":00",""),sep = "")

##now we have two diffrent date formats one date format is present in dd/mm/yyy and other date format is present in dd-mm-yy
##so to make it in one symmetric way we convert all sates in dd/mm/yyy format

ubermasterdata$Request.timestamp <- gsub("-","/",ubermasterdata$Request.timestamp,fixed = TRUE)
ubermasterdata$Drop.timestamp <- gsub("-","/",ubermasterdata$Drop.timestamp,fixed = TRUE)

View(ubermasterdata)

##now the date and time format is in string convert them into date time format


ubermasterdata$Request.timestamp <-as.POSIXct(ubermasterdata$Request.timestamp,format = "%d/%m/%Y %H:%M:%S")
ubermasterdata$Drop.timestamp <- as.POSIXct(ubermasterdata$Drop.timestamp,format = "%d/%m/%Y %H:%M:%S")

str(ubermasterdata)


## now the cleansing is odne lets delete the coloumn to highlight the date and time diffrence

ubermasterdata <- ubermasterdata[,-c(7,8)]


##converting remaining coloumns into factors


ubermasterdata$Request.id <- as.factor(ubermasterdata$Request.id)
ubermasterdata$Pickup.point <- as.factor(ubermasterdata$Pickup.point)
ubermasterdata$Status <- as.factor(ubermasterdata$Status)

################################################################
## Drilling down of data and creation of coloumns for analaysis#
################################################################

ubermasterdata$Requestmonth <- months(ubermasterdata$Request.timestamp) ##drilling downn new coloumn as month
ubermasterdata$weekdays <- weekdays(ubermasterdata$Request.timestamp)   ##drilling down coloumns as weekdays

ubermasterdata$RequestDateOfMonth <- format(ubermasterdata$Request.timestamp, "%d") #drilling down date
ubermasterdata$RequestHourOfTheDay <- format(ubermasterdata$Request.timestamp, "%H") #drilling down hour
ubermasterdata$tripduration <- ubermasterdata$Drop.timestamp - ubermasterdata$Request.timestamp

#load the required packages 
require(dplyr)
require(ggplot2)
require(scales)





##combination of times during which cabs are booked##

#morning_Rush 6 to 11:59
#day_time  12:00 to 16:59
#Evening_Rush 17 to 19:59
#Night        20:00 to 23:59
#late night   00:00 to 5:59



#cut the time slots to understand when the cab booking was made##

ubermasterdata$cabrequest <- cut(as.integer(format(ubermasterdata$Request.timestamp, "%H")),breaks = c(0, 5.59, 11.59, 16.59, 19.59, 23.59), include.lowest=TRUE )
ubermasterdata$cabrequest <- as.factor(ubermasterdata$cabrequest)


#setting the time slots at which cab booking happens##

levels(ubermasterdata$cabrequest) <- c("late_night","morning_Rush","day_time","Evening_Rush","Night")


##overall cab status of airport vs city##

ggplot(ubermasterdata, aes(x = ubermasterdata$Status)) + geom_bar(aes(fill = Pickup.point), position = "dodge")

##we need to analyze when cabs are not avilable and when cabs are cancelled##

CarNotAvailalable <- subset(ubermasterdata, ubermasterdata$Status == "No Cars Available")
DriverCancelled <- subset(ubermasterdata, ubermasterdata$Status == "Cancelled")

##EVERY WEEKDAY FREQUENCY OF NON AVAILABILITY in CITY vs AIRPORT IS SHOWN IN THE GRAPH##


ggplot(CarNotAvailalable, aes(x = weekdays)) + geom_bar(aes(fill = Pickup.point), position = "dodge")


# PLOTTING BAR FOR THE FREQUENCY OF RIDES NOT AVAILABLE IN CITY VS AIRPORT on WHICH DAY TIME

ggplot(CarNotAvailalable, aes(x = cabrequest)) + geom_bar(aes(fill = Pickup.point), position = "dodge")


# PLOTTING BAR FOR THE FREQUENCY OF RIDES CANCELLED IN CITY VS AIRPORT on WEEKDAYS

ggplot(DriverCancelled, aes(x = weekdays)) + geom_bar(aes(fill = Pickup.point), position = "dodge")

## PLOTTING BAR FOR THE FREQUENCY OF RIDES CANCELLED IN CITY VS AIRPORT on WHICH DAY TIME

ggplot(DriverCancelled, aes(x = cabrequest)) + geom_bar(aes(fill = Pickup.point), position = "dodge")




       