# Change directory
setwd("~/energy/data")

# Clear memory
rm(list=ls())

# Load libraries
library(ggplot2)
library(plyr)

# Load data
data <- read.csv("grand.csv")

# Make ts using POSIXct
data$date <- as.POSIXct(strptime(data$date, format="%Y-%m-%d %H:%M:%S"))

# Plot Demand Data by City
ggplot(data[!is.na(data$MW),], aes(x=date, y=MW, color=city)) +
        geom_line() +
        facet_wrap(~city, scales="free")

# Plot Temp Data by City
ggplot(data[!is.na(data$TEMP),], aes(x=date, y=TEMP, color=city)) +
        geom_line() +
        facet_wrap(~city, scales="free")

# Plot Dew Point Data by City
ggplot(data[!is.na(data$DEW.POINT),], aes(x=date, y=TEMP, color=city)) +
        geom_line() +
        facet_wrap(~city, scales="free")

# Keep only data from 2012 to 2014
data2 <- subset(data, date >= strptime("2012-01-01 00:00:00", format="%Y-%m-%d %H:%M:%S") & data$date < strptime("2014-01-01 00:00:00", format="%Y-%m-%d %H:%M:%S"))
# Therefore keeping only 1/4 of all data

# Plot Demand Data by City
ggplot(data2[!is.na(data2$MW),], aes(x=date, y=MW, color=city)) +
        geom_line() +
        facet_wrap(~city, scales="free")

# Plot Temp Data by City
ggplot(data2[!is.na(data2$TEMP),], aes(x=date, y=TEMP, color=city)) +
        geom_line() +
        facet_wrap(~city, scales="free")

# Plot Dew Point Data by City
ggplot(data2[!is.na(data2$DEW.POINT),], aes(x=date, y=TEMP, color=city)) +
        geom_line() +
        facet_wrap(~city, scales="free")

# Trim to remove outliers
data3 <- ddply(data, .(city), mutate, mean_MW = mean(MW, na.rm=TRUE), std_MW = sd(MW, na.rm=TRUE))
data3 <- subset(data3, MW <= mean_MW+2.58*std_MW & MW >= mean_MW-2.58*std_MW) # Drops data values beyond [1 to 99%] interval

# Do facet wrapping so that each city's MW and TEMP can be plotted next to each other
data4 <- data3
data4$type <- "Demand"
data4$TEMP <- NA
data4$DEW.POINT <- NA
data5 <- data3
data5$type <- "Temperature"
data5$MW <-NA
data5$DEW.POINT <- NA
data6 <- data3
data6$type <- "DewPoint"
data6$MW <- NA
data6$TEMP <- NA
data4 <- rbind(data4, data5, data6)

# Plot 
library(grid)
cities <- unique(data4$city)

for (i in 1:length(cities)) {
        current <- cities[i]
        filename <- paste(current,".pdf", sep="")
        p <- ggplot() +
                geom_line(data=subset(data4[!is.na(data4$MW),], city == current), aes(x=date, y=MW, color=city)) +
                geom_line(data=subset(data4[!is.na(data4$TEMP),], city == current), aes(x=date, y=TEMP, color = city)) + 
                geom_line(data=subset(data4[!is.na(data4$DEW.POINT),], city == current), aes(x=date, y=DEW.POINT, color = city)) + 
                facet_wrap(~type, scales="free")
        ggsave(paste(current,".pdf"), plot = p, width = 14.0, height = 8.5)
}