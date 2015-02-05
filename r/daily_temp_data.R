setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")

library(xlsx)
library(plyr)
library(ggplot2)
library(scales)
library(gdata)

# Daily Mean Temperature for Delhi, India 1995-2013.
data<-read.table(file="/Users/elliotcohen/Dropbox/Data/Climate/Daily_Temperature_1995-2013_Delhi.txt", header=FALSE, colClasses=c("factor", "factor","factor","numeric"))

names(data)<-c("Month","Day","Year","Temp")

# Create a Date attribute (column)
data$Date<-as.Date(as.character(paste(data$Year, data$Month, data$Day,sep="-")), "%Y-%m-%d")

yr<- subset(data, Date > as.Date("2012-03-31"))
yr<-subset(yr,Date<as.Date("2013-04-01"))
head(yr)
p<-ggplot(yr,aes(x=Date, y=Temp))
p+geom_line()