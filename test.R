## part a
# import data
bike<-read.csv(file="/Users/elliotcohen/Downloads/data.csv")
save(bike, file="bike.rData")
# load("bike.rData")

# convert chacarter strings to date-time objects
bike$starttime<-as.POSIXct(bike$starttime)
bike$stoptime<-as.POSIXct(bike$stoptime)

# compute duration
bike$trip.duration<-bike$stoptime-bike$starttime

# compute average trip duration
avgTrip<-mean(bike$trip.duration) # minutes
as.numeric(avgTrip, units = "secs") # seconds

## part b.
# Each trip is associated with a bikeid. By tracing an individual bike's movement across successive trips, you might realize that a bike's trip will usually end at the station where the next trip begins but that this is not always the case.

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

t<-2:n  # start at the second timestamp
arrive.station<-bike$stoptime[t-1] # bike "arrives" to station at stoptime of previous trip.
leave.station<-bike$starttime[t] # bike "leaves" station at starttime of current trip.
station.duration<-difftime(leave.station, arrive.station, units="mins") # compute difference in time between leaving and arriving to a station.
bike$station.duration<-c(NA, station.duration)
mean(station.duration) # NOTE: this value is skewed due to outliers in the data
summary(as.numeric(station.duration))

# compute robust alternative to mean: median
median(station.duration)

## part d.
# Describe and explain the major qualitative usage pattern for station fe2a5f and how it differs from station fec8ff?
# subset df to trips originating or terminating at either of these two stations.
a<-subset(bike, start.station.id %in% c("fe2a5f", "fec8ff") | end.station.id %in% c("fe2a5f", "fec8ff"))

library(plyr)
stats<-ddply(bike, .(end.station.id, start.station.id), numcolwise(mean), .progress=TRUE)

#   Estimate the number of bikes at stations 8f0f64 and 4a4b61 for each hour on the hour of 2013/10/30.*
#   Predict the number of bicycles arriving at stations 912d97, 2da8d7, 010d01, 36ba2f, fa4911 on 2013/11/26 in each hour. (To clarify, a bicycle arriving at 1:45PM would count as arriving during hour 13.)*
#   For the above two problems, submit the answer as a csv (comma separated) format (please follow the example below). If we are unable to easily parse your response, we will not grade it.