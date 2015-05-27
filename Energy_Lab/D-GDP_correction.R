## we choose: (based on the data quality and GDP data resource)
# Singapore - Manila_Philippines
# Singapore - Dakar_Senegal
# San Diego - Amman_Jordan (to be continued)
## based on the GDP data we estimate the demand in 2013

# GDP of countries (current US$)
# source:World Bank
Jodan_GDP_10_13 <- c(26425379437,  28840263380,	31015239545,	33678500148)
Philippines_GDP_10_13 <- c(1.99589E+11,  2.24143E+11,	2.5024E+11,	2.72067E+11)
Senegal_GDP_10_13 <- c(12932427724,  14440676498,	14045680427,	14791699009)
Singapore_GDP_2011 <- 274065000000
Singapore_GDP_2012 <- 286908000000
Singapore_GDP_2013 <- 297941000000
Queensland_GDP_2011 <- 267942000000
Queensland_GDP_2012 <- 284441000000
Queensland_GDP_2013 <- 290158000000
# data issue 10-11,11-12,12-13
# lack of Tokyo data

# source: 'The global urban competitiveness report - 2011'
Amman_GDP_2010 <- 6128640000*1.09
Amman_GDP_2011 <- 7216940000*1.05
Dakar_GDP_2010 <- 1363990000*1.09
Dakar_GDP_2011 <- 1595730000*1.05
Manila_GDP_2010 <- 3669550000*1.09
Manila_GDP_2011 <- 4048010000*1.05

# estimate data
# calculate the city/country GDP
Amman_in_Jodan = mean(Amman_GDP_2010/Jodan_GDP_10_13[1],Amman_GDP_2011/Jodan_GDP_10_13[2])
Dakar_in_Senegal = mean(Dakar_GDP_2010/Senegal_GDP_10_13[1],Dakar_GDP_2011/Senegal_GDP_10_13[2])
Manila_in_Philippines = mean(Manila_GDP_2010/Philippines_GDP_10_13[1],Manila_GDP_2011/Philippines_GDP_10_13[2])

Amman_GDP_2012_est = Jodan_GDP_10_13[3]*Amman_in_Jodan
Amman_GDP_2013_est = Jodan_GDP_10_13[4]*Amman_in_Jodan
Amman_GDP_2014_est = Jodan_GDP_10_13[4]*1.03*Amman_in_Jodan
Dakar_GDP_2012_est = Senegal_GDP_10_13[3]*Dakar_in_Senegal
Dakar_GDP_2013_est = Senegal_GDP_10_13[4]*Dakar_in_Senegal
Dakar_GDP_2014_est = Senegal_GDP_10_13[4]*1.045*Dakar_in_Senegal
Manila_GDP_2012_est = Philippines_GDP_10_13[3]*Manila_in_Philippines
Manila_GDP_2013_est = Philippines_GDP_10_13[4]*Manila_in_Philippines
Manila_GDP_2014_est = Philippines_GDP_10_13[4]*1.06*Manila_in_Philippines
# 2014 GDP growth rate forcast from World bank

## the population data
Amman_Popu_2014 = 1148000
Dakar_Popu_2014 = 3393000
Manila_Popu_2014 = 1745000  # NCR:12764000
Tokyo_Popu_2014 = 37833000
Singapore_Popu_2014 = 5517000
# source: UN, Department of Economic and Social Affairs
Queensland_Popu_2013 = 4690910
Queensland_Popu_2014 = 4740927  
# http://www.qgso.qld.gov.au/products/reports/pop-growth-qld/index.php, Dec. 30

# estimate the population of 2013
Amman_Popu_2015_est = 1155000
Dakar_Popu_2015_est = 3520000
Manila_Popu_2015_est = 1745000*(1.661/1.581)^(1/7)
Tokyo_Popu_2015_est = 38001000
Singapore_Popu_2015_est = 5619000
Amman_Popu_2013_est = (Amman_Popu_2014)^2/Amman_Popu_2015_est
Dakar_Popu_2013_est = (Dakar_Popu_2014)^2/Dakar_Popu_2015_est
Manila_Popu_2013_est = 1745000/(1.661/1.581)^(1/7)
Tokyo_Popu_2013_est = (Tokyo_Popu_2014)^2/Tokyo_Popu_2015_est
Singapore_Popu_2013_est = (Singapore_Popu_2014)^2/Singapore_Popu_2015_est # we have true value

#########################
# calculation
#########################
# GDP_correction function
GDP_correction <- function(GDP_developing_2013,GDP_developing_2014,
               Popu_developing_2013,Popu_developing_2014,
               GDP_developed_2013,Popu_developed_2013,
               demand_developing_2013,demand_developed_2013){
  delta_PPP <- vector("numeric",365)
  for(i in 1:365){
    delta_PPP[i] = GDP_developing_2013/Popu_developing_2013*((((GDP_developing_2014/Popu_developing_2014)/(GDP_developing_2013/Popu_developing_2013))^((i-1)/365))-1)
  }
  delta_demand <- vector("numeric",365)
  for(i in 1:365){
    # delta_demand[i] = (demand_developed_2013[i]/Popu_developed_2013*Popu_developing_2013-demand_developing_2013[i])*delta_PPP[i]/(GDP_developed_2013/Popu_developed_2013-GDP_developing_2013/Popu_developing_2013)
    delta_demand[i] = (demand_developed_2013[365]/Popu_developed_2013*Popu_developing_2013-demand_developing_2013[365])*delta_PPP[i]/(GDP_developed_2013/Popu_developed_2013-GDP_developing_2013/Popu_developing_2013)  
  }
  return(delta_demand)
}

## Manila
# the 2013 demand data of Manila
demandData = data_list_demand[[6]]
demandData = na.omit(demandData)
dailyMax=aggregate(demandData[, c("MW")], by=list(demandData$Date), FUN=max)
dailyMax$YR = substr(as.factor(dailyMax[,1]),1,4)
dailyMax = subset(dailyMax,dailyMax$YR==2013)
demand_developing_2013 <- dailyMax[,2]

# the 2013 demand data of Singapore
demandData = data_list_demand[[13]]
demandData = na.omit(demandData)
dailyMax=aggregate(demandData[, c("MW")], by=list(demandData$Date), FUN=max)
dailyMax$YR = substr(as.factor(dailyMax[,1]),1,4)
dailyMax = subset(dailyMax,dailyMax$YR==2013)
demand_developed_2013 <- dailyMax[,2]

# The GDP correction of Manila
Manila_correction <- GDP_correction(Manila_GDP_2013_est,Manila_GDP_2014_est,
                                    Manila_Popu_2013_est,Manila_Popu_2014,
                                    Singapore_GDP_2013,Singapore_Popu_2013_est,
                                    demand_developing_2013,demand_developed_2013)

# Correction comparison (very little change)
demandData = data_list_demand[[6]]
demandData = na.omit(demandData)
dailyMax_all=aggregate(demandData[, c("MW")], by=list(demandData$Date), FUN=max)
dailyMax_all$YR = substr(as.factor(dailyMax_all[,1]),1,4)
dailyMax_raw = rbind(subset(dailyMax_all,dailyMax_all$YR==2011),subset(dailyMax_all,dailyMax_all$YR==2012))
dailyMax <- data.frame(Date = dailyMax_raw[,1],MW = dailyMax_raw[,2])
dailyDataTs=ts(dailyMax$MW, start=c(2011), end=c(2013), frequency=365)
forecast_stl <- forecast(stl(dailyDataTs, s.window="period"), h=365, method="arima")
true_data <- subset(dailyMax_all,dailyMax_all$YR==2013)
library(lubridate)
ManilaPlot = merge(actual = as.zoo(forecast_stl$x),predicted=as.zoo(cbind(mean=forecast_stl$mean,
                                                                          lower=forecast_stl$lower[,1],
                                                                          upper=forecast_stl$upper[,1])),
                   GDP=forecast_stl_corr,
                   TURE=ts(true_data[,2],start = c(2013),end=c(2014), frequency=365))
ManilaPlotxts=xts(ManilaPlot, date_decimal(index(ManilaPlot)))
ManilaPlot_final=dygraph(ManilaPlotxts, "Manila Power Demand Data") %>%
  dySeries("actual", label = "Actual") %>%
  dySeries(c("lower", "mean", "upper"), label = "Predicted") %>%
  dySeries("GDP", label="Revised according to GDP") %>%
  dySeries("TURE", label="Ture value") %>%
  dyRangeSelector()
ManilaPlot_final

## Dakar
# the 2013 demand data of Dakar
demandData = data_list_demand[[5]]
demandData = na.omit(demandData)
dailyMax=aggregate(demandData[, c("MW")], by=list(demandData$Date), FUN=max)
dailyMax$YR = substr(as.factor(dailyMax[,1]),1,4)
dailyMax = subset(dailyMax,dailyMax$YR==2013)
demand_developing_2013 <- dailyMax[,2]

Dakar_correction <- GDP_correction(Dakar_GDP_2013_est,Dakar_GDP_2014_est,
                                    Dakar_Popu_2013_est,Dakar_Popu_2014,
                                    Singapore_GDP_2013,Singapore_Popu_2013_est,
                                    demand_developing_2013,demand_developed_2013)

# Correction comparison (very little change)
library(lubridate)
DakarPlot = merge(actual = as.zoo(forecast_stl$x),predicted=as.zoo(cbind(mean=forecast_stl$mean,
                                                                         lower=forecast_stl$lower[,1],
                                                                         upper=forecast_stl$upper[,1])),
                  GDP=forecast_stl_corr,
                  TURE=ts(true_data[,2],start = c(2013),end=c(2014), frequency=365))
DakarPlotxts=xts(DakarPlot, date_decimal(index(DakarPlot)))
DakarPlot_final=dygraph(DakarPlotxts, "Dakar Power Demand Data") %>%
  dySeries("actual", label = "Actual") %>%
  dySeries(c("lower", "mean", "upper"), label = "Predicted") %>%
  dySeries("GDP", label="Revised according to GDP") %>%
  dySeries("TURE", label="Ture value") %>%
  dyRangeSelector()
DakarPlot_final

