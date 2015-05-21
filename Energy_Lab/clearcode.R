library(ggplot2)
library(forecast)
library(xts)
library(lubridate)
library(dygraphs)
library(reshape2)

setwd("/Users/Lichking/GithubFiles/Energy/data/")

demandData=read.csv("demand/SouthAustralia_Australia_HalfHourly_Demand.csv")
weaData=read.csv("weather/South Australia(Adelaide)_Interpolated_Hourly.csv")

setwd("/Users/Lichking/Desktop/Columbia/Courses/DataVisualization/Project/")

###preprocess time value
demandData$Date=paste(demandData$YR, demandData$M, demandData$D, sep="-")
demandData$Hours=paste(demandData$HR, "00", "00", sep=":")
demandData$Time=paste(demandData$Date, demandData$Hours, sep=" ")
demandData$Time=as.POSIXct(demandData$Time, format="%Y-%m-%d %H:%M:%S")
weaData$Time=as.POSIXct(weaData$hours, format="%Y-%m-%d %H:%M:%S")
weaData=na.omit(weaData)
demandData=na.omit(demandData)
weaDemand=merge(weaData[, c("Time", "city", "LAT", "LONG", "ELEV", "TEMP", "DEW.POINT")], 
                demandData[, c("Time", "Date",  "MW", "YR", "M", "D", "HR")], by="Time")
dailyMax=aggregate(weaDemand[, c("Date", "MW")], by=list(weaDemand$Date), FUN=max)
dailyMax=dailyMax[, c(2,3)]
dailyMax$Date=as.Date(dailyMax$Date)
dailyMax=dailyMax[order(dailyMax$Date), ]
ggplot(dailyMax, aes(x=Date, y=MW))+geom_line(colour="#E69F00")+labs(title="Time Series plot for South Australia Power Demand")
dailyDataTs=ts(dailyMax$MW, start=c(2011), end=c(2013), frequency=365)

fit <- stl(dailyDataTs, s.window="period")
plot(fit)
forecast(fit)
plot(forecast(fit, h=365, method="arima"))
dailyMaxNew=dailyMax[cut(dailyMax$Date, breaks="year")!="2010-01-01", ]
dailyDataTsNew=ts(dailyMaxNew$MW, start=c(2011), end=c(2014), frequency=365)
dailyDataTsNew[dailyDataTsNew<250]=400
fit2<-stl(dailyDataTsNew, s.window = "period")
plot(fit2)
fit3<-HoltWinters(dailyDataTsNew)
plot(fit3)
plot(forecast(fit3, 365))
dailyDataMsts=msts(dailyDataTsNew, seasonal.periods = c(7, 365.25))
fit4<-tbats(dailyDataMsts, use.trend=TRUE)
plot(forecast(fit4, h=365))
fit5<-tbats(c(dailyDataTsNew), seasonal.periods = c(week=7, year1=354.37, year2=365), use.trend=TRUE)
fit6<-nnetar(dailyDataTsNew)


###analyze the data of Singapore and Abidjan
AbidjanHWfit=HoltWinters(dailyDataTsNew)
AbidjanHWforecast=forecast(AbidjanHWfit, h=365)
AbidjanPlot=merge(actual=as.zoo(AbidjanHWforecast$x), predicted=as.zoo(cbind(mean=AbidjanHWforecast$mean,
                                                                             lower=AbidjanHWforecast$lower[,1],
                                                                             upper=AbidjanHWforecast$upper[,1])))
AbidjanPlotxts=xts(AbidjanPlot, date_decimal(index(AbidjanPlot)))
AbidjanPlot=dygraph(AbidjanPlotxts, "Abidjan Power Demand Data") %>%
            dySeries("actual", label = "Actual") %>%
            dySeries(c("lower", "mean", "upper"), label = "Predicted") %>%
            dyRangeSelector()

load("demand.RData")
names(Demand)
SingaporeDemand=Demand$Singapore
SingaporeDemand=SingaporeDemand[, c("Time", "MW", "Date", "MIN")]
SingaporeDemand=subset(SingaporeDemand, MIN==c(0))
SingaporeDemand=aggregate(SingaporeDemand[, c("Date", "MW")], by=list(SingaporeDemand$Date), FUN=max)
SingaporeDemand=SingaporeDemand[, c(2,3)]
SingaporeDemand$Date=as.Date(SingaporeDemand$Date)
SingaporeDemand=SingaporeDemand[order(SingaporeDemand$Date),]
SingaporeDemand=SingaporeDemand[-1,]
SingaporeDemand=SingaporeDemand[-(366:370),]
Abidjan2013=dailyDataTsNew[index(dailyDataTsNew)>=2013 & index(dailyDataTsNew)<2014]
popuRatio=4.708/5.399
G1=55176.88
G2=5260
growthRate=0.101
vecyear=c(1:365)
dailyGrowthRate=growthRate/365
deltaP_deltaG=(SingaporeDemand$MW*popuRatio-Abidjan2013)/(G1-G2)
deltaG=G2*(dailyGrowthRate+1)^vecyear
deltaG=deltaG-G2
deltaP=deltaP_deltaG*deltaG

AbidjanPlot=merge(actual=as.zoo(AbidjanHWforecast$x), predicted=as.zoo(cbind(mean=AbidjanHWforecast$mean,
                                                                             lower=AbidjanHWforecast$lower[,1],
                                                                             upper=AbidjanHWforecast$upper[,1])),
                  GDP=deltaP+AbidjanHWforecast$mean)
AbidjanPlotxts=xts(AbidjanPlot, date_decimal(index(AbidjanPlot)))
AbidjanPlot=dygraph(AbidjanPlotxts, "Abidjan Power Demand Data") %>%
            dySeries("actual", label = "Actual") %>%
            dySeries(c("lower", "mean", "upper"), label = "Predicted") %>%
            dySeries("GDP", label="Revised according to GDP") %>%
            dyRangeSelector()
AbidjanPlot

###analyze the data of SanDiego and Beirut
AbidjanHWfit=HoltWinters(dailyDataTs)
AbidjanHWforecast=forecast(AbidjanHWfit, h=365)
AbidjanPlot=merge(actual=as.zoo(AbidjanHWforecast$x), predicted=as.zoo(cbind(mean=AbidjanHWforecast$mean,
                                                                             lower=AbidjanHWforecast$lower[,1],
                                                                             upper=AbidjanHWforecast$upper[,1])))
AbidjanPlotxts=xts(AbidjanPlot, date_decimal(index(AbidjanPlot)))
AbidjanPlot=dygraph(AbidjanPlotxts, "Beinut Power Demand Data") %>%
  dySeries("actual", label = "Actual") %>%
  dySeries(c("lower", "mean", "upper"), label = "Predicted") %>%
  dyRangeSelector()
AbidjanPlot

load("demand.RData")
names(Demand)
SingaporeDemand=Demand$SanDiego
SingaporeDemand=SingaporeDemand[, c("Time", "MW", "Date")]
SingaporeDemand=aggregate(SingaporeDemand[, c("Date", "MW")], by=list(SingaporeDemand$Date), FUN=max)
SingaporeDemand=SingaporeDemand[, c(2,3)]
SingaporeDemand$Date=as.Date(SingaporeDemand$Date)
SingaporeDemand=SingaporeDemand[order(SingaporeDemand$Date),]
SingaporeDemand=SingaporeDemand[c(2289:2653),]
Abidjan2013=dailyDataTs[index(dailyDataTs)>=2013 & index(dailyDataTs)<2014]
popuRatio=2.179/1.346
G1=57955
G2=9928.04
growthRate=0.03
vecyear=c(1:365)
dailyGrowthRate=growthRate/365
deltaP_deltaG=(SingaporeDemand$MW*popuRatio-Abidjan2013)/(G1-G2)
deltaG=G2*(dailyGrowthRate+1)^vecyear
deltaG=deltaG-G2
deltaP=deltaP_deltaG*deltaG

AbidjanPlot=merge(actual=as.zoo(AbidjanHWforecast$x), predicted=as.zoo(cbind(mean=AbidjanHWforecast$mean,
                                                                             lower=AbidjanHWforecast$lower[,1],
                                                                             upper=AbidjanHWforecast$upper[,1])),
                  GDP=deltaP+AbidjanHWforecast$mean)
AbidjanPlotxts=xts(AbidjanPlot, date_decimal(index(AbidjanPlot)))
AbidjanPlot2=dygraph(AbidjanPlotxts, "Beirut Power Demand Data") %>%
  dySeries("actual", label = "Actual") %>%
  dySeries(c("lower", "mean", "upper"), label = "Predicted") %>%
  dySeries("GDP", label="Revised according to GDP") %>%
  dyRangeSelector()
AbidjanPlot2

###analyze the data of Brisbane and Mababane
AbidjanHWfit=fit
AbidjanHWforecast=forecast(AbidjanHWfit, h=365, method="arima")
AbidjanPlot=merge(actual=as.zoo(AbidjanHWforecast$x), predicted=as.zoo(cbind(mean=AbidjanHWforecast$mean,
                                                                             lower=AbidjanHWforecast$lower[,1],
                                                                             upper=AbidjanHWforecast$upper[,1])))
AbidjanPlotxts=xts(AbidjanPlot, date_decimal(index(AbidjanPlot)))
AbidjanPlot=dygraph(AbidjanPlotxts, "Mbabane Power Demand Data") %>%
  dySeries("actual", label = "Actual") %>%
  dySeries(c("lower", "mean", "upper"), label = "Predicted") %>%
  dyRangeSelector()
AbidjanPlot

load("demand.RData")
names(Demand)
SingaporeDemand=Demand$Queensland
SingaporeDemand=SingaporeDemand[, c("Time", "MW", "Date", "MIN")]
SingaporeDemand=subset(SingaporeDemand, MIN==c(0))
SingaporeDemand=aggregate(SingaporeDemand[, c("Date", "MW")], by=list(SingaporeDemand$Date), FUN=max)
SingaporeDemand=SingaporeDemand[, c(2,3)]
SingaporeDemand$Date=as.Date(SingaporeDemand$Date)
SingaporeDemand=SingaporeDemand[order(SingaporeDemand$Date),]
SingaporeDemand=SingaporeDemand[SingaporeDemand$Date<as.Date("2007-01-01")&
                                  SingaporeDemand$Date>=as.Date("2006-01-01"),]
Abidjan2013=dailyDataTs[index(dailyDataTs)>=2013 & index(dailyDataTs)<2014]
popuRatio=0.066/4.091
G1=57357.61
G2=8872
growthRate=0.03
vecyear=c(1:365)
dailyGrowthRate=growthRate/365
deltaP_deltaG=(SingaporeDemand$MW*popuRatio-Abidjan2013)/(G1-G2)
deltaG=G2*(dailyGrowthRate+1)^vecyear
deltaG=deltaG-G2
deltaP=deltaP_deltaG*deltaG

AbidjanPlot=merge(actual=as.zoo(AbidjanHWforecast$x), predicted=as.zoo(cbind(mean=AbidjanHWforecast$mean,
                                                                             lower=AbidjanHWforecast$lower[,1],
                                                                             upper=AbidjanHWforecast$upper[,1])),
                  GDP=deltaP+AbidjanHWforecast$mean)
AbidjanPlotxts=xts(AbidjanPlot, date_decimal(index(AbidjanPlot)))
AbidjanPlot2=dygraph(AbidjanPlotxts, "Mbabane Power Demand Data") %>%
  dySeries("actual", label = "Actual") %>%
  dySeries(c("lower", "mean", "upper"), label = "Predicted") %>%
  dySeries("GDP", label="Revised according to GDP") %>%
  dyRangeSelector()
AbidjanPlot2


###analyze the data of San Diego and Delhi
AbidjanHWfit=stl(dailyDataTs, s.window="period")
AbidjanHWforecast=forecast(fit, h=365, method="arima")
AbidjanPlot=merge(actual=as.zoo(AbidjanHWforecast$x), predicted=as.zoo(cbind(mean=AbidjanHWforecast$mean,
                                                                             lower=AbidjanHWforecast$lower[,1],
                                                                             upper=AbidjanHWforecast$upper[,1])))
AbidjanPlotxts=xts(AbidjanPlot, date_decimal(index(AbidjanPlot)))
AbidjanPlot=dygraph(AbidjanPlotxts, "Delhi Power Demand Data") %>%
  dySeries("actual", label = "Actual") %>%
  dySeries(c("lower", "mean", "upper"), label = "Predicted") %>%
  dyRangeSelector()
AbidjanPlot
load("demand.RData")
names(Demand)
SingaporeDemand=Demand$SanDiego
SingaporeDemand=SingaporeDemand[, c("Time", "MW", "Date")]
SingaporeDemand=aggregate(SingaporeDemand[, c("Date", "MW")], by=list(SingaporeDemand$Date), FUN=max)
SingaporeDemand=SingaporeDemand[, c(2,3)]
SingaporeDemand$Date=as.Date(SingaporeDemand$Date)
SingaporeDemand=SingaporeDemand[order(SingaporeDemand$Date),]
SingaporeDemand=SingaporeDemand[index(SingaporeDemand)>=2012 & index(SingaporeDemand)<2013,]

Abidjan2013=dailyDataTs[index(dailyDataTs)>=2012 & index(dailyDataTs)<2013]
popuRatio=24.953/1.346
G1=57955
G2=3165.95
growthRate=0.118
vecyear=c(1:365)
dailyGrowthRate=growthRate/365
deltaP_deltaG=(SingaporeDemand$MW*popuRatio-Abidjan2013)/(G1-G2)
deltaG=G2*(dailyGrowthRate+1)^vecyear
deltaG=deltaG-G2
deltaP=deltaP_deltaG*deltaG

AbidjanPlot=merge(actual=as.zoo(AbidjanHWforecast$x), predicted=as.zoo(cbind(mean=AbidjanHWforecast$mean,
                                                                             lower=AbidjanHWforecast$lower[,1],
                                                                             upper=AbidjanHWforecast$upper[,1])),
                  GDP=deltaP+AbidjanHWforecast$mean)
AbidjanPlotxts=xts(AbidjanPlot, date_decimal(index(AbidjanPlot)))
AbidjanPlot=dygraph(AbidjanPlotxts, "Abidjan Power Demand Data") %>%
  dySeries("actual", label = "Actual") %>%
  dySeries(c("lower", "mean", "upper"), label = "Predicted") %>%
  dySeries("GDP", label="Revised according to GDP") %>%
  dyRangeSelector()
AbidjanPlot


###south Australia
AbidjanHWfit=fit4
AbidjanHWforecast=forecast(AbidjanHWfit, h=365)
AbidjanPlot=merge(actual=as.zoo(dailyDataTs), predicted=as.zoo(cbind(mean=AbidjanHWforecast$mean,
                                                                             lower=AbidjanHWforecast$lower[,1],
                                                                             upper=AbidjanHWforecast$upper[,1])))
AbidjanPlotxts=xts(AbidjanPlot, date_decimal(index(AbidjanPlot)))
AbidjanPlot=dygraph(AbidjanPlotxts, "South Australia Power Demand Data") %>%
  dySeries("actual", label = "Actual") %>%
  dySeries(c("lower", "mean", "upper"), label = "Predicted") %>%
  dyRangeSelector()
AbidjanPlot



compare_wea=merge(abidjan_wea[, c("Time", "TEMP", "DEW.POINT")], 
                  singapore_wea[, c("Time", "TEMP", "DEW.POINT")], by="Time")
names(compare_wea)=c("Time", "Abidjan_temp", "Abidjan_dew", "Singapore_temp", "Singapore_dew")
compare_temp_plot=compare_wea[, c("Time", "Abidjan_temp", "Singapore_temp")]
compare_temp_plot=melt(compare_temp_plot, id.vars = "Time")
ggplot(compare_temp_plot, aes(x=Time, y=value, colour=variable))+geom_line()
plot(x=compare_wea$Time, y=compare_wea$Abidjan_temp, type="l")
lines(x=compare_wea$Time, y=compare_wea$Singapore_temp, col="blue")

compare_dew_plot=compare_wea[, c("Time", "Abidjan_dew", "Singapore_dew")]
compare_dew_plot$Abidjan_dew[compare_dew_plot$Abidjan_dew<=17]=20
compare_dew_plot=melt(compare_dew_plot, id.vars = "Time")
ggplot(compare_dew_plot, aes(x=Time, y=value, colour=variable))+geom_line()



###plot GDP growth rate
china=c(10.4, 9.3, 7.7, 7.7)
india=c(10.3, 6.6, 5.1, 6.9)
cote_d_ivoire=c(2.0, -4.4, 10.7, 8.7)
chad=c(13.6, 0.1, 8.9, 4.0)

denmark=c(1.6, 1.2, -0.7, -0.5)
australia=c(2.0, 2.3, 3.7, 2.5)
united_states=c(2.5, 1.6, 2.3, 2.2)

GDPdata=data.frame(Date=as.Date(c("2010-01-01", "2011-01-01", "2012-01-01", "2013-01-01")),
                   China=china,
                   India=india,
                   Cote_d_Ivoire=cote_d_ivoire,
                   Chad=chad,
                   Denmark=denmark,
                   Australia=australia,
                   United_States=united_states)
GDPdataxts=xts(GDPdata[, -1], order.by = GDPdata$Date)
dygraph(GDPdataxts, "GDP growth plot (%)")


