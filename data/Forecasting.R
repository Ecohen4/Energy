setwd("~/downloads/csv")
load("x.RData")
library(plyr)
library(forecast)
library(hydroGOF)
#----- clean the data ---- 
read.csv("grand.csv")->x
x=x[,c(1,2,4)]
x$date=as.Date(x$date)
tmp=unique(x[,1:2])
tmp2=vector()
for (i in 1:dim(tmp)[1]){
    tmp2[i]=mean(x$TEMP[((i-1)*24+1):(i*24)])
}
temp=cbind(tmp,Temp=tmp2)
temp=temp[-48695,]


read.csv("result.csv")->tag   # cluster result
read.csv("data_2.csv")->data
data=na.omit(data)
#spt=split(data,data$city)
data2=cbind(data,temp)[,-c(4,5)]

#----- cities index with cluster tag ----
cities=as.data.frame(cbind(as.character(unique(data$city)),c(1:48)))
colnames(cities)=c("city","No")
city.tag=join(cities,tag)

#---- cities in cluster 1 ----
#2013-2014
Eugene=ts(data[which(data$city=="Eugene"),][,2],frequency=365,start=c(2006,1,1))
part1=ts.decompose(Eugene)
part2=ts.forecast(Eugene,365)
Eugene2=cbind(data2[which(data$city=="Eugene"),][,c(1,2,4)],part1)
start=Eugene2[length(Eugene),1]
Eugene3=cbind(Date=times(start,365),part2)

#---- add traing and test ----
len=length(Eugene)
Eugene_tra=ts(Eugene[1:(len-365)],frequency=365,start=c(2006,1,1))
Eugene_ori=ts(Eugene[(len-365+1):len],frequency=365,start=c(2013,1,1))
Eugene_test=ts.forecast(Eugene_tra,365)

p=plot(Eugene_ori,Eugene_test[,1],xlim=c(200,500),ylim=c(200,500),xlab="observed",ylab="predict",main="Eugene")
abline(1,1,col="red")

sub=Eugene_ori-Eugene_test[,1]
hist(sub,xlim=c(-150,150),prob=TRUE,xlab="diff",main="distribution of residuals in test data")
curve(dnorm(x,mean=mean(sub), sd=sd(sub)), col="red", lwd=2, add=TRUE, yaxt="n")

x=times("2012-12-31",365)
plot(x,Eugene_ori,type="l",main="Observe/Predict of Eugene in 2013",xlab="time",ylab="Energy Demand")
lines(x,Eugene_test[,1],col="red")

rmse.eu=rmse(Eugene_ori,Eugene_test[,1])
#-----------------------------


write.csv(Eugene2,"Eugene-origianl.csv")
write.csv(Eugene3,"Eugene-predict-365.csv")

#---- cities in cluster 2 ----
#2014-2015
Dakar=ts(data[which(data$city=="Dakar"),][,2],frequency=365,start=c(2011,1,1))
part1=ts.decompose(Dakar)
part2=ts.forecast(Dakar,365)
Dakar2=cbind(data2[which(data$city=="Dakar"),][,c(1,2,4)],part1)
start=Dakar2[length(Abidjan),1]
Dakar3=cbind(Date=times(start,365),part2)

#---- add traing and test ----
len=length(Dakar)
Dakar_tra=ts(Dakar[1:(len-365)],frequency=365,start=c(2011,1,1))
Dakar_ori=ts(Dakar[(len-365+1):len],frequency=365,start=c(2014,1,1))
Dakar_test=ts.forecast(Dakar_tra,365)
plot(Dakar_ori,Dakar_test[,1],xlim=c(250,450),ylim=c(250,450),xlab="observed",ylab="predict",main="Dakar")
abline(1,1,col="red")

sub=Dakar_ori-Dakar_test[,1]
hist(sub,xlim=c(-50,100),prob=TRUE,xlab="diff",main="distribution of residuals in test data")
curve(dnorm(x,mean=mean(sub), sd=sd(sub)), col="red", lwd=2, add=TRUE, yaxt="n")

x=times("2013-12-31",365)
plot(x,Dakar_ori,type="l",main="Observe/Predict of Dakar in 2014",xlab="time",ylab="Energy Demand")
lines(x,Dakar_test[,1],col="red")

rmse.da=rmse(Dakar_ori,Dakar_test[,1])
#-----------------------------

write.csv(Dakar2,"Dakar-origianl.csv")
write.csv(Dakar3,"Dakar-predict-365.csv")

#---- cities in cluster 3 ----
#2013-2014
Louisville=ts(data[which(data$city=="Louisville"),][,2],frequency=365,start=c(2006,1,1))
part1=ts.decompose(Louisville)
part2=ts.forecast(Louisville,365)
Louisville2=cbind(data2[which(data$city=="Louisville"),][,c(1,2,4)],part1)
start=Louisville2[length(Louisville),1]
Louisville3=cbind(Date=times(start,365),part2)

#---- add traing and test ----
len=length(Louisville)
Louisville_tra=ts(Louisville[1:(len-365)],frequency=365,start=c(2006,1,1))
Louisville_ori=ts(Louisville[(len-365+1):len],frequency=365,start=c(2013,1,1))
Louisville_test=ts.forecast(Louisville_tra,365)
plot(Louisville_ori,Louisville_test[,1],xlim=c(3000,7000),ylim=c(3000,7000),xlab="observed",ylab="predict",main="Louisville")
abline(1,1,col="red")

x=times("2012-12-31",365)
plot(x,Louisville_ori,type="l",ylim=c(3000,7000),main="Observe/Predict of Louisville in 2013",xlab="time",ylab="Energy Demand")
lines(x,Louisville_test[,1],col="red")

rmse.lo=rmse(Louisville_ori,Louisville_test[,1])
#-----------------------------

# save the document
write.csv(Louisville2,"Louisville-origianl.csv")
write.csv(Louisville3,"Louisville-predict-365.csv")

#---- cities in cluster 4 ----
#2013-2014
Sacramento=ts(data[which(data$city=="Sacramento"),][,2],frequency=365,start=c(2006,1,1))
part1=ts.decompose(Sacramento)
part2=ts.forecast(Sacramento,365)
Sacramento2=cbind(data2[which(data$city=="Sacramento"),][,c(1,2,4)],part1)
start=Sacramento2[length(Sacramento),1]
Sacramento3=cbind(Date=times(start,365),part2)

#---- add traing and test ----
len=length(Sacramento)
Sacramento_tra=ts(Sacramento[1:(len-365)],frequency=365,start=c(2006,1,1))
Sacramento_ori=ts(Sacramento[(len-365+1):len],frequency=365,start=c(2013,1,1))
Sacramento_test=ts.forecast(Sacramento_tra,365)
plot(Sacramento_ori,Sacramento_test[,1],xlim=c(1000,2500),ylim=c(1000,2500),xlab="observed",ylab="predict",main="Sacramento")
abline(1,1,col="red")

x=times("2012-12-31",365)
plot(x,Sacramento_ori,type="l",ylim=c(1000,2500),main="Observe/Predict of Sacramento in 2013",xlab="time",ylab="Energy Demand")
lines(x,Sacramento_test[,1],col="red")

rmse.sa=rmse(Sacramento_ori,Sacramento_test[,1])
#-----------------------------


write.csv(Sacramento2,"Sacramento-original.csv")
write.csv(Sacramento3,"Sacramento-predict-365.csv")


#---- decompose function ------
ts.decompose=function(ts){
    components=decompose(ts)
    plot(components)
    trend=components$trend
    random=components$random
    seasonal=components$seasonal
    result=data.frame(trend,random,seasonal)
    colnames(result)=c("Trend","Random","Seasonal")
    return (result)
}
#---- forecast funtion ----
ts.forecast=function(ts,day){
    model=HoltWinters(ts,beta=FALSE,gamma=TRUE)
    plot(model)
    fore=forecast.HoltWinters(model,h=day)
    plot.forecast(fore)
  #  plotForecastErrors(fore$residuals)
    print(Box.test(fore$residuals,lag=20,type="Ljung-Box"))
    result=data.frame(forecast.HoltWinters(model,h=day))
    return(result)
}

#---- time format ----
times=function(day_before_start,d){
    st=as.Date(day_before_start)+1
    end=st+d-1
    return(as.Date(st:end))
}

#---- arima diff test ----
arima.diff=function(ts,d){
    diff=diff(ts,differences=d)
    plot.ts(diff)
    return(diff)
}
###############
## Appendix ###
###############
#--------------- functions for error forecast --------
plotForecastErrors <- function(forecasterrors) {
    # make a histogram of the forecast errors:
    mybinsize <- IQR(forecasterrors)/4
    mysd <- sd(forecasterrors)
    mymin <- min(forecasterrors) - mysd*5
    mymax <- max(forecasterrors) + mysd*3
    # generate normally distributed data with mean 0 and standard deviation mysd 
    mynorm <- rnorm(10000, mean=0, sd=mysd)
    mymin2 <- min(mynorm)
    mymax2 <- max(mynorm)
    if (mymin2 < mymin) { mymin <- mymin2 }
    if (mymax2 > mymax) { mymax <- mymax2 }
    # make a red histogram of the forecast errors, with the normally distributed data overl 
    mybins <- seq(mymin, mymax, mybinsize)
    hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
    # freq=FALSE ensures the area under the histogram = 1
    # generate normally distributed data with mean 0 and standard deviation mysd
    myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
    # plot the normal curve as a blue line on top of the histogram of forecast errors: 
    points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}