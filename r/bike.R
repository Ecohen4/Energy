## part a
# import data
# options(stringsAsFactors=FALSE)
bike<-read.csv(file="/Users/elliotcohen/Downloads/data.csv")

# convert chacarter strings to date-time objects
bike$starttime<-as.POSIXct(bike$starttime)
bike$stoptime<-as.POSIXct(bike$stoptime)

# compute duration
bike$trip.duration<-bike$stoptime-bike$starttime

save(bike, file="bike.rData")

# compute average trip duration
avgTrip<-mean(bike$trip.duration) # minutes
as.numeric(avgTrip, units = "secs") # seconds

## part b.
# Each trip is associated with a bikeid. By tracing an individual bike's movement across successive trips, you might realize that a bike's trip will usually end at the station where the next trip begins but that this is not always the case.
load("bike.rData")

# order data.frame by bikeid, starttime and endtime.
bike<-bike[ order(bike$bikeid, bike$starttime, bike$stoptime), ]

# extract start/end location
n<-nrow(bike)
start<-bike$start.station.id[2:n]
end<-bike$end.station.id[1:(n-1)]

start<-as.character(start)
end<-as.character(end)

# count how many times a bike trip did not originate from the same station where the bike was last seen.
sum(start != end)

# Estimate the minimum fraction of the original dataset that must be missing.
sum(start != end)/n

## part c.
# Based on the available data, what is the average amount of time a bike spends at a station in seconds?
# given that the df is ordered by bikeid...  we can compute the time at each station

station.duration<-function(x){
  n<-dim(x)[1]
  t<-2:n  # start at the second timestep
  arrive.station<-x$stoptime[t-1] # bike "arrives" to station at stoptime of previous trip.
  leave.station<-x$starttime[t] # bike "leaves" station at starttime of current trip.
  station.duration<-difftime(leave.station, arrive.station, units="mins") # compute difference in time between leaving and arriving to a station.
  station.duration<-c(0, station.duration) # cannot compute for t=1, supply 0 or NA.
  return(station.duration)
}

test<-bike[1:1000, 1:5] # try on a subset of the data
test1<-station.duration(test)

# third attempt
k<-length(levels(test$bikeid))
station.duration<-array(0, dim(bike)[1])
test3<-for(i in 1:k) {
  x<-subset(test, bikeid==levels(test$bikeid)[i])
  n<-dim(x)[1]
  t<-2:n  # start at the second timestamp
  arrive.station<-x$stoptime[t-1] # bike "arrives" to station at stoptime of previous trip.
  leave.station<-x$starttime[t] # bike "leaves" station at starttime of current trip.
  station.duration<-difftime(leave.station, arrive.station, units="mins") # compute difference in time between leaving and arriving to a station.
  #x$station.duration<-c(NA, station.duration)
  #station.duration<-c(0, station.duration)
  if(i==1){df<-station.duration} else
    df<-rbind(df, station.duration)
}

test2<-ddply(test, .(bikeid), summarise, station.duration=station.duration(), .progress="text")
ddply(test, .(bikeid, end.station.id, start.station.id), summarise, station.duration=starttime-min(end.time), .progress="text")



ddply(test, .(bikeid, end.station.id, start.station.id), station.duration=function(x) {
  n<-dim(x)[1]
  t<-2:n  # start at the second timestamp
  arrive.station<-x$stoptime[t-1] # bike "arrives" to station at stoptime of previous trip.
  leave.station<-x$starttime[t] # bike "leaves" station at starttime of current trip.
  station.duration<-difftime(leave.station, arrive.station, units="mins") # compute difference in time between leaving and arriving to a station.
  #x$station.duration<-c(NA, station.duration)
  station.duration<-c(0, station.duration)
  return(station.duration)
} , .progress="text")

ddply(bike, .(bikeid), summarize, station.duration=station.duration(), .progress="text")


# n<-dim(dat[2])
# t<-2:n  # start at the second timestamp
# arrive.station<-dat$stoptime[t-1] # bike "arrives" to station at stoptime of previous trip.
# leave.station<-dat$starttime[t] # bike "leaves" station at starttime of current trip.
# station.duration<-difftime(leave.station, arrive.station, units="mins") # compute difference in time between leaving and arriving to a station.
# dat$station.duration<-c(NA, station.duration)

# compute mean
mean(station.duration, na.rm=TRUE) # NOTE: this value is skewed due to outliers in the data
summary(as.numeric(station.duration)) # huge positive and negative numbers skew mean estimate.

# compute robust alternative to mean: median
median(station.duration)

## part d.
# Describe and explain the major qualitative usage pattern for station fe2a5f and how it differs from station fec8ff?
# subset df to trips originating or terminating at either of these two stations.
a<-subset(bike, start.station.id %in% c("fe2a5f", "fec8ff") | end.station.id %in% c("fe2a5f", "fec8ff"))

# look at data based on station origin
origin<-subset(bike, start.station.id %in% c("fe2a5f", "fec8ff"))
origin<-droplevels(origin)
aggregate(cbind(station.duration, trip.duration) ~ start.station.id, data=origin, FUN=median)

# look at data based on station destination
destination<-subset(bike, end.station.id %in% c("fe2a5f", "fec8ff"))
destination<-droplevels(destination)
aggregate(cbind(station.duration, trip.duration) ~ end.station.id, data=destination, FUN=median)

# library(plyr)
# stats<-ddply(bike, .(end.station.id, start.station.id), numcolwise(median), .progress=TRUE)

## part e.
# Estimate the number of bikes at stations 8f0f64 and 4a4b61 for each hour on the hour of 2013/10/30.
origin<-subset(bike, start.station.id %in% c("8f0f64", "4a4b61"))
origin<-droplevels(origin)
aggregate(cbind(station.duration, trip.duration) ~ start.station.id, data=origin, FUN=median)

# Using the POSIX date-time objects, we can easily group the data with respect to time (e.g. by month, day, hour, etc..). This will come in handy for temporal aggregation.
origin$month <- cut(origin$stoptime, breaks = "month")
week <- cut(origin$stoptime, breaks = "week")
day <- cut(origin$stoptime, breaks = "day")
hour <- cut(origin$stoptime, breaks = "hour")

dummy<-strsplit(as.character(week), split=" ")
week<-laply(dummy, '[[', 1) # keep the date, drop the time
origin$week<-as.factor(week)

dummy<-strsplit(as.character(day), split=" ")
day<-laply(dummy, '[[', 1) # keep the date, drop the time
origin$day<-as.factor(day)

dummy<-strsplit(as.character(hour), split=" ")
hour<-laply(dummy, '[[', 2) # keep the time, drop the date
origin$hour<-as.factor(hour)

# summarize the data over each hour
# compute how many bikes arrive during that hour...
ddply(origin, .(start.station.id))
# Homogenous Poisson Process...
# 1. assume a constant rate of occurance of the poisson process (lamda).
# 2. assume time intervals are indpedent.
# 3. combining assumptions 1 and 2, above, the expectation is given by mu=lambda*t.
# 4. thus the probability of n occurances in an interval [0,t] is given by:
# Pn(t) = P[X(t)=n] = (exp(-lambda*t)*(lambda*t)^n)/factorial(n)
# lambda<- # bike arrivals per hour.
# n<-
prob<-(exp(-lambda*t)*(lambda*t)^n)/factorial(n)


#   Predict the number of bicycles arriving at stations 912d97, 2da8d7, 010d01, 36ba2f, fa4911 on 2013/11/26 in each hour. (To clarify, a bicycle arriving at 1:45PM would count as arriving during hour 13.)*
#   For the above two problems, submit the answer as a csv (comma separated) format (please follow the example below). If we are unable to easily parse your response, we will not grade it.