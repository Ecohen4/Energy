## temporal smoothing
## example smoothing for hourly energy data
daily <- ts(data$Energy_Supplied, start = c(2011, 01), frequency = 365) #daily
weekly<-aggregate(daily, nfrequency=52, ts.eps=1, FUN=sum) #weekly
monthly<-aggregate(daily, nfrequency=12, ts.eps=1, FUN=sum) #monthly
par(mfrow=c(3,1))
par(oma=c(0,0,2,0))           # set outter margins
par(mar=c(2,4,2,2) + 0.1)     # set plot margins
# par(mar=c(5,4,4,2) + 0.1.)    # default (bottom, left, top, right)
plot(daily, cex.lab=1.2, cex.axis=1.2)
plot(weekly, cex.lab=1.2, cex.axis=1.2)
plot(monthly, cex.lab=1.2, cex.axis=1.2)
title(main="Energy supplied [GWh] to the NR grid at varying time scales", outer=TRUE, cex.main=1.5)