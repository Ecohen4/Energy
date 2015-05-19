# Project:A Data-driven Approach to Identify Peer-cities 
#         for Sharing of Best Practices in Energy Management
# Clustering part
# Yiqian jin 
# May 18

setwd("~/Documents/Energy/data")
library(ggplot2)
library(plyr)
library(reshape2)
library(scales)
library(NbClust)
library(grid)

data=read.csv("grand.csv")
data$date <- as.POSIXct(strptime(data$date, format="%Y-%m-%d %H:%M:%S"))
data$day<-trunc(data$date,"day")
cityname=as.vector(unique(data$city))

##################### change to daily 

data.2<-data.frame()
for(i in 1: length(cityname)){
  tmp<-subset(data, city==cityname[i])
  tmp$day <- as.POSIXct(strptime(tmp$day, format="%Y-%m-%d"))
  tmp.daily=ddply(tmp, .(day), summarize, MW=mean(MW))
  tmp.daily$city=cityname[i]
  data.2=rbind(data.2, tmp.daily)
}

############### check the most frequent year
year<-c()
for (i in 1: length(cityname)){
  tmp2<-subset(data.2, city==cityname[i])
  tmp2$day <- as.POSIXct(strptime(tmp2$day, format="%Y-%m-%d"))
  year<-c(year,unique(format(tmp2$day, "%Y")))
  # tmp2$day<-trunc(tmp2$day,"year")
}
year
sort(table(year), decreasing=TRUE) ## 2013 appears most times, then 2012
####

data.3=subset(data.2, day>="2013-01-01" & day<="2013-12-31")
length(unique(data.3$city)) ## 36
dim(data.3)
citynames=as.vector(unique(data.3$city))

###### get the city list that has a complete 365 days data
city.kp=c()
for (i in 1:length(citynames)){
  tmp=subset(data.3, city==citynames[i])
  if(length(tmp$day)==365)
    city.kp=c(city.kp, citynames[i])
}

##### subset to get the final dataset
data.4=data.frame()
for (i in 1:length(city.kp)){
  tmp=subset(data.3, city==city.kp[i])
  data.4=rbind(data.4, tmp)
}
length(as.vector(unique(data.4$city)))  ## 27 cities

ggplot(data=data.4, aes(x=day, y=MW, color=city))+geom_line()+facet_wrap(~city, scales="free")
#####   find the abnormal outlier with city: Beirut, Mindelo, Isla Sao Nicolau
city.out=c("Beirut", "Mindelo", "Isla Sao Nicolau")
data.4.2=data.4[!data.4$city %in% city.out, ]
length(unique(data.4.2$city))

################ check data from 2012 year# ###%%%%%%%%%%%%%%%%%%%%%%%%%%%
data.2$day <- as.POSIXct(strptime(data.2$day, format="%Y-%m-%d"))
data.5=subset(data.2, day>="2012-01-01" & day<="2012-12-31")
length(unique(data.5$city)) ## 33
dim(data.5)
cityname.12=as.vector(unique(data.5$city))

###### get the city list that has a complete 365 days data
city.kp=c()
for (i in 1:length(cityname.12)){
  tmp=subset(data.3, city==cityname.12[i])
  if(length(tmp$day)==365)
    city.kp=c(city.kp, cityname.12[i])
}

###################  end 2012 %%%%%%%%%%%%%%%%%%%%
#### only Beirut added 2012, but it is swept out because of outliers ###

## check 2011

data.2$day <- as.POSIXct(strptime(data.2$day, format="%Y-%m-%d"))
data.3=subset(data.2, day>="2011-01-01" & day<="2011-12-31")
length(unique(data.3$city)) ## 36
dim(data.3)
cityname.11=as.vector(unique(data.3$city))

###### get the city list that has a complete 365 days data
city.kp=c()
for (i in 1:length(cityname.11)){
  tmp=subset(data.3, city==cityname.11[i])
  if(length(tmp$day)==365)
    city.kp=c(city.kp, cityname.11[i])
}


### 2011
Antigua=subset(data.3, city=="Antigua")
Delhi=subset(data.3, city=="Delhi - BRPL")
Philadelphia=subset(data.3, city=="Philadelphia")

## 2007
data.3=subset(data.2, day>="2007-01-01" & day<="2007-12-31")
Detroit=subset(data.3, city=="Detroit")
Indianapolis=subset(data.3, city=="Indianapolis")

## 2006
data.3=subset(data.2, day>="2006-01-01" & day<="2006-12-31")
Queensland=subset(data.3, city=="Queensland")

Victoria=subset(data.3, city=="Victoria")

South.Australia=subset(data.3, city=="South Australia")

New.South.Wales=subset(data.3, city=="New South Wales")

newadd=rbind(Antigua, Delhi, Philadelphia, Detroit, Indianapolis, Queensland,
             Victoria, South.Australia, New.South.Wales)

data.4.2=rbind(data.4.2, newadd)
citynames=as.vector(unique(data.4.2$city))

## remove San Diego ## outliers
data.4.2=subset(data.4.2, city!="San Diego")
citynames=as.vector(unique(data.4.2$city))

#### remove  Victoria and  South Australia, 
#### only use New South Wales and Queensland to represent Australia
data.4.2=subset(data.4.2, city!="Victoria")
data.4.2=subset(data.4.2, city!="South Australia")
citynames=as.vector(unique(data.4.2$city))
write.table(data.4.2,"data_30cities", sep=",")

ggplot(data=data.4.2, aes(x=day, y=MW, color=city))+geom_line()+
  facet_wrap(~city, scales="free", ncol=5)
last_plot()+scale_x_datetime(labels = date_format("%b"))+
  theme_minimal()+theme(legend.position = "none")



#####  scale ####
data.5=data.frame()
for (i in 1:length(citynames)){
  tmp=subset(data.4.2, city==citynames[i])
  tmp$MW=scale(tmp$MW)
  data.5=rbind(data.5, tmp) 
}

tmp=subset(data.4.2, city=="Abidjan")
for (i in 1:length(citynames)){
  tmp=subset(data.5, city==citynames[i])
  print(dim(tmp)) 
}



##########################################################
##  with smoothing~~
library(xts)
library(zoo)
library(TTR)
num=5
data.5.sma=data.frame()
for (i in 1:length(citynames)){
  tmp=subset(data.5, city==citynames[i])
  nas=tmp$MW[1:(num-1)]
  tmp$MW=SMA(tmp$MW, n=num)
  tmp$MW[1:(num-1)]=nas
  data.5.sma=rbind(data.5.sma, tmp)
}
sum(is.na(data.5.sma))

###### transfer to the data.frame that can be applied to kmeans
#### 1. label date to be the same
standard=data.5.sma$day[1:365]
data.5.3=data.frame()
for (i in 1:length(citynames)){
  tmp=subset(data.5.sma, city==citynames[i])
  tmp$day=standard
  data.5.3=rbind(data.5.3, tmp)
}

data.6=dcast(data.5.3, city~day, value.var='MW')
rownames(data.6)=data.6[,1]
data.6=data.6[,-1]

set.seed(10)
##(1)
km.out=kmeans(data.6, centers=4, nstart=10, iter.max=20)
# length(km.out$cluster)
result=as.data.frame(km.out$cluster)
clusteris3=which(result==3)
clusteris4=which(result==4)
result[clusteris3,]=4
result[clusteris4,]=3
write.table(result,"result_0518.csv", sep=",")


data.7<-data.5.3
data.7$cluster<-"0"
for (i in 1:nrow(result)){
  index=which(data.7$city==row.names(result)[i])
  data.7$cluster[index]<-result[i,]
}
data.7$day=as.Date(data.7$day)
data.7$cluster=as.factor(data.7$cluster)

data.7$day <- as.POSIXct(strptime(data.7$day, format="%Y-%m-%d"))
ggplot(data=data.7, aes(x=day, y=MW, color=city))+geom_line()+
  facet_wrap(~cluster, scales="free")
last_plot()+scale_x_datetime(labels = date_format("%b"))+
  theme_minimal()+
  theme(legend.position = "none", axis.title.x=element_blank())

write.table(subset(data.7, cluster==1), "cluster1.csv", sep=",")
write.table(subset(data.7, cluster==2), "cluster2.csv", sep=",")
write.table(subset(data.7, cluster==3), "cluster3.csv", sep=",")
write.table(subset(data.7, cluster==4), "cluster4.csv", sep=",")

