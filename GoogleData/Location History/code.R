#######################################
# Libraries
#######################################
library(jsonlite)
library(ggplot2)
library(ggmap)
library(geosphere)
library(lubridate)
library(lattice)

#######################################
# Set up env
#######################################
Sys.setenv(http_proxy="")
setwd("~/Desktop/MyProject/GoogleData/Location History")
max_dist_tresh = 100

#######################################
# Get and prepare data
#######################################
data = fromJSON("LocationHistory.json")
data_obj = data$locations

data_obj$timestamp = as.numeric(data_obj$timestampMs)/1000
data_obj$timestampMs = NULL
data_obj$lon = data_obj$longitudeE7/10000000
data_obj$longitudeE7 = NULL
data_obj$lat = data_obj$latitudeE7/10000000
data_obj$latitudeE7 = NULL
data_obj$activitys = NULL

# remove inaccurate data
data_obj = data_obj[which(data_obj$accuracy<=100), ]
data_obj = data_obj[which(data_obj$verticalAccuracy<=50), ]

# caculate distances and dates
shift.vec <- function(vec, shift){
  if (length(vec) <= abs(shift)){
    rep(NA ,length(vec))
  } else {
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec) - shift)]) }
    else {
      c(vec[(abs(shift) + 1):length(vec)], rep(NA, abs(shift)))
    }
  }
}

data_obj$lat_end <- shift.vec(data_obj$lat, -1)
data_obj$lon_end <- shift.vec(data_obj$lon, -1)

deg2rad = function(deg) {
  rad =  as.numeric(deg) * (pi/180)
  rad
}

calcDist = function(row) {
  R = 6371 # Radius of the earth in km
  dlat = deg2rad(as.numeric(row["lat_end"])-as.numeric(row["lat"]))
  dlon = deg2rad(as.numeric(row["lon_end"])-as.numeric(row["lon"]))
  a = sin(dlat/2) * sin(dlat/2) + cos(deg2rad(row["lat"])) * cos(deg2rad(row["lat_end"])) * sin(dlon/2) * sin(dlon/2)
  c = 2 * atan2(as.numeric(sqrt(a)), as.numeric(sqrt(1-a)))
  d = R * c # Distance in km
  d / 1.6
}

data_obj$dist_to_prev = apply(data_obj, 1, FUN=calcDist)
data_obj$date = as.Date(as.POSIXct(data_obj$timestamp, origin="1970-01-01"))
data_obj$yearmon = paste(year(data_obj$date), sprintf("%02d", month(data_obj$date)), sep="-")

#######################################
# Location plotting
#######################################
# texas location plots
texas = get_map(location="texas", source="google", zoom=6, maptype = "terrain", color="color")
ggmap(texas) + geom_point(data = data_obj, aes(x=lon, y=lat), alpha=0.5, color="red") + 
  theme(legend.position = "right") + labs(x = "Longitude", y = "Latitude")

# houston location plots
houston = get_map(location="rice university", source="google", zoom=15, maptype = "terrain", color="color")
ggmap(houston) + geom_point(data = data_obj, aes(x=lon, y=lat), alpha=0.5, color="red") + 
  theme(legend.position = "right") + labs(x = "Longitude", y = "Latitude")

# dallas location plots
dallas = get_map(location="the colony,tx", source="google", zoom=10, maptype = "toner-lite", color="color")
ggmap(dallas) + geom_point(data = data_obj, aes(x=lon, y=lat), alpha=0.5, color="red") + 
  theme(legend.position = "right") + labs(x = "Longitude", y = "Latitude")

#######################################
# Velocity plot at locations in dallas
#######################################
data_obj = data_obj[which(!is.na(data_obj$velocity)), ]
data_obj = data_obj[which(data_obj$velocity <= 40), ]

ggmap(dallas) + geom_point(data=data_obj, aes(x=lon, y=lat, color=velocity), alpha=0.5) + 
  theme(legend.position="left") + scale_colour_gradient(limits=c(0, 40), low="blue", high="red")

#######################################
# Distance travelled per month
#######################################
dist_by_month = NULL
dist_by_month = aggregate(data_obj$dist_to_prev, by=list(yearmon = data_obj$yearmon), FUN = sum)
dist_by_month = dist_by_month[-1,]

barplot(dist_by_month$x, names.arg=dist_by_month$yearmon, xlab="Month", ylab="Miles Travelled", 
        main="Miles Travelled per Month", ylim=c(0,4000))

#######################################
# Distance travelled per month grouped by weekday
#######################################
dist_by_day = NULL
dist_by_day = aggregate(data_obj$dist_to_prev, by=list(date=data_obj$date), FUN = sum)
dist_by_day = na.omit(dist_by_day)

dist_by_day$dow = weekdays(dist_by_day$date)
dist_by_day$is_weekday = dist_by_day$dow
dist_by_day$is_weekday[dist_by_day$is_weekday == "Monday"] = "Weekday"
dist_by_day$is_weekday[dist_by_day$is_weekday == "Tuesday"] = "Weekday"
dist_by_day$is_weekday[dist_by_day$is_weekday == "Wednesday"] = "Weekday"
dist_by_day$is_weekday[dist_by_day$is_weekday == "Thursday"] = "Weekday"
dist_by_day$is_weekday[dist_by_day$is_weekday == "Friday"] = "Weekday"
dist_by_day$is_weekday[dist_by_day$is_weekday == "Saturday"] = "Weekend"
dist_by_day$is_weekday[dist_by_day$is_weekday == "Sunday"] = "Weekend"

dist_by_day$yearmon = paste(year(dist_by_day$date), sprintf("%02d", month(dist_by_day$date)), sep="-")

dist_by_day = dist_by_day[which(dist_by_day$x < max_dist_tresh),]
dist_by_day = dist_by_day[-which(dist_by_day$yearmon=="2016-07"), ]

is_weekday_by_month_mean = aggregate(dist_by_day[c("x")], by=list(yearmon=dist_by_day$yearmon, is_weekday=dist_by_day$is_weekday), FUN=mean)

barchart(x ~ yearmon, groups=is_weekday, is_weekday_by_month_mean, horizontal = FALSE,
         xlab="Month", ylab="Miles", ylim=c(0,80), main="Miles Travelled per Month",
         auto.key=list(size = 1, columns = 2, text=c("Weekday", "Weekend"), cex.title=1, title="")) 

boxplot(x ~ is_weekday, data=dist_by_day, main="Miles Travelled per Month", ylab="Miles")
boxplot(x ~ yearmon, data=dist_by_day, main="Miles Travelled per Month", ylab="Miles")
boxplot(x ~ dow, data=dist_by_day, main="Miles Travelled per DOW", ylab="Miles")

#######################################
# Distance travelled per month grouped by dow
#######################################
dist_by_day = NULL
dist_by_day = aggregate(data_obj$dist_to_prev, by=list(date=data_obj$date), FUN = sum)
dist_by_day = na.omit(dist_by_day)
dist_by_day$dow = weekdays(dist_by_day$date)
dist_by_day$yearmon = paste(year(dist_by_day$date), sprintf("%02d", month(dist_by_day$date)), sep="-")
dist_by_day = dist_by_day[which(dist_by_day$x < max_dist_tresh),]

dist_by_dow = aggregate(dist_by_day[c("x")], by=list(yearmon=dist_by_day$yearmon, dow=dist_by_day$dow), FUN=mean)
dist_by_dow = dist_by_dow[-which(dist_by_dow$yearmon=="2016-07"), ]

barchart(x ~ yearmon, groups=dow, dist_by_dow, horizontal = FALSE,
         xlab="Month", ylab="Miles", ylim=c(0,110), main="Miles Travelled per Month",
         auto.key=list(size = 1, columns = 7, text=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), cex.title=1, title="")) 


