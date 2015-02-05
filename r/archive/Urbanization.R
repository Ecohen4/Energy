## Global urbanization 
setwd("/Users/elliotcohen/Dropbox/data/Urbanization")

library(xlsx)
library(plyr)
library(reshape2)
library(ggplot2)
library(scales)
library(fields)

## Import data
# United Nations, Department of Economic and Social Affairs, Population Division
# World Urbanization Prospects: The 2011 Revision
# File 14:  Average Annual Rate of Change of Urban Agglomerations with 750,000 Inhabitants or More in 2011, by Country, 1950-2025 (per cent)
cities=read.xlsx(file="/Users/elliotcohen/Dropbox/data/Urbanization/UN_2011-Growth_Rate-Cities_Over_750k.xlsx",sheetName="GROWTH-RATE-CITIES",as.data.frame=TRUE,header=TRUE, check.names=TRUE, startRow=13)

# bin growth rates by quantile
df<-melt(cities, id.vars=c(1:7))
get<-levels(df$variable)
dummy<-strsplit(get, split="X")
years<-laply(dummy, '[[', 2)
test<-gsub("[.]", "-", years)
df$variable<-factor(df$variable, labels=test)
df$bin<-cut(df$value, breaks=quantile(df$value))
df$bin.name<-factor(df$bin, labels=c("1st quartile","2nd quartile","3rd quartile","4th quartile"))

e1<-which(df$variable %in% c("1950-1955", "1955-1960", "1960-1965", "1965-1970", "1970-1975"))
e2<-which(df$variable %in% c("1975-1980", "1980-1985", "1985-1990", "1990-1995", "1995-2000"))
e3<-which(df$variable %in% c("2000-2005", "2005-2010", "2010-2015", "2015-2020", "2020-2025"))
df$epoch<-NA
df$epoch[e1]<-"1950-1975"
df$epoch[e2]<-"1975-2000"
df$epoch[e3]<-"2000-2025"

epochal<-ddply(df, .(Country.code, Country, City.code, Urban.agglomeration, Note, Latitude, Longitude, epoch), summarize, value=mean(value) )

epochal$bin<-cut(epochal$value, breaks=quantile(epochal$value))
epochal$bin.name<-factor(epochal$bin, labels=c("1st quartile","2nd quartile","3rd quartile","4th quartile"))
epochal$epoch<-as.factor(epochal$epoch)

# base map
world <- map_data("world")
worldmap <- ggplot(world, aes(x=long, y=lat, group=group)) +
  geom_path()

# layer map with population data
ggplot() + 
  geom_path(data=world, aes(x=long, y=lat, group=group)) +
  geom_point(data=df, aes(x=Longitude, y=Latitude, colour=bin.name)) + 
  facet_wrap(~variable) +
  # coord_equal() +
  #scale_y_continuous(breaks=(-2:2) * 30) +
  #scale_x_continuous(breaks=(-4:4) * 45) +
  labs(colour = 'Urbanization Rate') +
  theme_bw()
  # scale_color_brewer()

# ALTERNATE map with urbanization data
ggplot() + 
  geom_path(data=world, aes(x=long, y=lat, group=group)) +
  geom_point(data=epochal, aes(x=Longitude, y=Latitude, colour=bin.name)) + 
  facet_wrap(~epoch, nrow=length(levels(epochal$epoch))) +
  coord_equal() +
  scale_y_continuous(breaks=(-2:2) * 30) +
  scale_x_continuous(breaks=(-4:4) * 45) +
  labs(colour = 'Urbanization Rate') +
  theme_bw()
# scale_color_brewer()