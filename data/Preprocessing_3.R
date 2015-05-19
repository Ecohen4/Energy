setwd("~/Energy/data/")
data <- read.csv("grand.csv")

data2 <- subset(data, select=c(city,date,MW, TEMP, DEW.POINT))
data_dp <- data2[complete.cases(data2),]

cities <- unique(data_dp$city)

lis <- list()
MW <- list()
TEMP <- list()
DP <- list()
lis2 <- list()
lis3 <- list()
lis4 <- list()
lis5 <- list()
lis6 <- list()

require(ggplot2)

# Granular
setwd("~/Energy/data/cleaned_small")
for (i in 1:length(cities)) {
        current <- cities[i]
        startdate <- as.character(data_dp[data_dp$city==current,][1,2])
        YR <- as.numeric(substr(startdate, 1, 4))
        M <- as.numeric(substr(startdate, 6, 7))
        D <- as.numeric(substr(startdate, 9,10))
        HR <- as.numeric(substr(startdate, 12,13))
        temp <- ts(data_dp[data_dp$city==current,][,3:5], frequency=365, start=c(YR,M,D,HR))
        if (length(temp[,1]) >= 17520) { # At least 2 years of data
                lis[[i]] <- decompose(temp[,1])
                MW[[i]] <- temp[,1]
                TEMP[[i]] <- temp[,2]
                DP[[i]] <- temp[,3]
                write.csv(temp, file =paste(current, "_cleaned.csv", sep=""))
                pdf(paste(current,".pdf", sep=""), width = 14.0, height = 8.5)
                plot(lis[[i]])
                dev.off()
                lis2[[i]] <- decompose(lis[[i]]$trend)
                pdf(paste(current,"_2ndorder.pdf", sep=""), width = 14.0, height = 8.5)
                plot(lis2[[i]])
                dev.off()
                lis3[[i]] <- decompose(lis2[[i]]$trend)
                pdf(paste(current,"_3rdorder.pdf", sep=""), width = 14.0, height = 8.5)
                plot(lis3[[i]])
                dev.off()
                lis4[[i]] <- decompose(lis3[[i]]$trend)
                pdf(paste(current,"_4thorder.pdf", sep=""), width = 14.0, height = 8.5)
                plot(lis4[[i]])
                dev.off()
                lis5[[i]] <- decompose(lis4[[i]]$trend)
                pdf(paste(current,"_5thorder.pdf", sep=""), width = 14.0, height = 8.5)
                plot(lis5[[i]])
                dev.off()
                lis6[[i]] <- decompose(lis5[[i]]$trend)
                pdf(paste(current,"_6thorder.pdf", sep=""), width = 14.0, height = 8.5)
                plot(lis6[[i]])
                dev.off()
                mod = list()
                mod[[i]] <- lm(lis[[i]]$seasonal ~ row(temp)[,1] + MW[[i]])
                pdf(paste(current,"_fit.pdf", sep=""), width = 14.0, height = 8.5)
                plot(row(temp)[,1], lis[[i]]$seasonal, type="l")
                lines(row(temp)[,1],mod[[i]]$fitted,col=2)
                dev.off()
        }
}

# Aggregated at Day Level
setwd("~/Energy/data/cleaned_byday")

data_dp$YR <- substr(data_dp$date, 1, 4)
data_dp$M <- substr(data_dp$date, 6, 7)
data_dp$D <- substr(data_dp$date, 9,10)
data_dp$YMD <- paste(data_dp$YR, data_dp$M, data_dp$D, sep="-")

require(plyr)
data_dp2 <- subset(data_dp, select=c(city, MW, TEMP, DEW.POINT, YMD))
data_dp2 <- ddply(data_dp2, .(city, YMD), numcolwise(mean))

lis <- list()
MW <- list()
TEMP <- list()
DP <- list()
lis2 <- list()
lis3 <- list()
lis4 <- list()
lis5 <- list()
lis6 <- list()

for (i in 1:length(cities)) {
        current <- cities[i]
        startdate <- as.character(data_dp2[data_dp2$city==current,][1,2])
        YR <- as.numeric(substr(startdate, 1, 4))
        M <- as.numeric(substr(startdate, 6, 7))
        D <- as.numeric(substr(startdate, 9,10))
        temp <- ts(data_dp2[data_dp2$city==current,][,3:5], frequency=365, start=c(YR,M,D))
        if (length(temp[,1]) >= 730) { # At least 2 years of data
                lis[[i]] <- decompose(temp[,1])
                MW[[i]] <- temp[,1]
                TEMP[[i]] <- temp[,2]
                DP[[i]] <- temp[,3]
                write.csv(temp, file =paste(current, "_cleaned.csv", sep=""))
                pdf(paste(current,".pdf", sep=""), width = 14.0, height = 8.5)
                plot(lis[[i]])
                dev.off()
                mod = list()
                mod[[i]] <- lm(lis[[i]]$seasonal ~ row(temp)[,1] + MW[[i]])
                pdf(paste(current,"_fit.pdf", sep=""), width = 14.0, height = 8.5)
                plot(row(temp)[,1], lis[[i]]$seasonal, type="l")
                lines(row(temp)[,1],mod[[i]]$fitted,col=2)
                dev.off()
        }
                #if (length(lis[[i]]$trend) >= 730) {
                #        lis2[[i]] <- decompose(lis[[i]]$trend)
                #        pdf(paste(current,"_2ndorder.pdf", sep=""), width = 14.0, height = 8.5)
                #        plot(lis2[[i]])
                #        dev.off() 
                #}
                #if (length(lis2[[i]]$trend) >= 730) {
                #        lis3[[i]] <- decompose(lis2[[i]]$trend)
                #        pdf(paste(current,"_3rdorder.pdf", sep=""), width = 14.0, height = 8.5)
                #        plot(lis3[[i]])
                #        dev.off()
                #}
                #lis4[[i]] <- decompose(lis3[[i]]$trend)
                #pdf(paste(current,"_4thorder.pdf", sep=""), width = 14.0, height = 8.5)
                #plot(lis4[[i]])
                #dev.off()
                #lis5[[i]] <- decompose(lis4[[i]]$trend)
                #pdf(paste(current,"_5thorder.pdf", sep=""), width = 14.0, height = 8.5)
                #plot(lis5[[i]])
                #dev.off()
                #lis6[[i]] <- decompose(lis5[[i]]$trend)
                #pdf(paste(current,"_6thorder.pdf", sep=""), width = 14.0, height = 8.5)
                #plot(lis6[[i]])
                #dev.off()
}

