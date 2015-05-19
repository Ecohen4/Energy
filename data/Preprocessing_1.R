# Load Packages
library(plyr)
library(ggplot2)

# Import energy data files
setwd("~/energy/data/demand")
temp <- list.files(pattern="*.csv")
temp <- c(temp[1:3], temp[5:7], temp[9:length(temp)])
myfiles <- lapply(temp, read.csv)
attach(myfiles[[10]])
myfiles[[10]] <- myfiles[[10]][order(city, YR, M, D, HR),]
detach()

# Combine all data.frames in list into single data.frame
df <- rbind.fill(myfiles)

# Fill missing MIN data
df$MIN[which(is.na(df$MIN))] <- 0

# Drop granular data to get only hourly data, drop MWh column as some cities don't have that data
# df <- subset(df, MIN == 0)

# Format date / time
df$date <- ISOdate(df$YR, df$M, df$D, df$HR)

# Import Weather data files
setwd("~/energy/data/weather")
temp <- list.files(pattern="*.csv")
myfiles <- lapply(temp, read.csv)

# Fill missing data that are fillable (if any) (LAT, LONG, ELEV)
for (i in 1:length(temp)) {
        lat <- myfiles[[i]]$LAT[1]
        lon <- myfiles[[i]]$LON[1]
        elev <- myfiles[[i]]$ELEV[1]
        myfiles[[i]]$LAT[which(is.na(myfiles[[i]]$TEMP))] <- lat
        myfiles[[i]]$LON[which(is.na(myfiles[[i]]$TEMP))] <- lon
        myfiles[[i]]$ELEV[which(is.na(myfiles[[i]]$TEMP))] <- elev
}

# Combine all data.frams in list into single data.frame
df2 <- rbind.fill(myfiles)

# Format date / time
df2$hours <- ISOdate(df2$YR, df2$M, df2$D, df2$HR)
names(df2)[1] <- "date"

# Join data.frames
data <- join(df, df2, by=c("city", "date"))
data <- subset(data, select=c("city", "date", "MW", "TEMP", "DEW.POINT", "distance", "LAT", "LONG", "ELEV"))

setwd("~/energy/data/")
write.table(data, "grand.csv", sep=",")