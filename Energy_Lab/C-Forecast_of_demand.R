
# Manila
demandData = data_list_demand[[6]]
demandData = na.omit(demandData)
dailyMax_all=aggregate(demandData[, c("MW")], by=list(demandData$Date), FUN=max)
dailyMax_all$YR = substr(as.factor(dailyMax_all[,1]),1,4)
dailyMax_raw = rbind(subset(dailyMax_all,dailyMax_all$YR==2011),subset(dailyMax_all,dailyMax_all$YR==2012))
dailyMax <- data.frame(Date = dailyMax_raw[,1],MW = dailyMax_raw[,2])
dailyDataTs=ts(dailyMax$MW, start=c(2011), end=c(2013), frequency=365)

# Decomposition
Decomp <- stl(dailyDataTs, s.window="period")
plot(Decomp)
# ARIMA
forecast_stl <- forecast(Decomp, h=365, method="arima")
plot(forecast_stl)
# HW
fit_HW <-HoltWinters(dailyDataTs)
# plot(fit_HW)
forecast_HW <- forecast(fit_HW, 365, method="arima")
plot(forecast_HW)
# true data
true_data <- subset(dailyMax_all,dailyMax_all$YR==2013)
plot(true_data[,2],type="l",,col="black",ylim=c(4000,9000))

plot(forecast_stl$mean,col="blue",ylim=c(4000,9000))
par(new=T)
plot(forecast_HW$mean,col="red",ylim=c(4000,9000))
par(new=T)
plot(true_data[,2],type="l",,col="black",ylim=c(4000,9000))
# par(new=T)
# plot(forecast_stl_corr,type="l",,col="green",ylim=c(4000,9000))

# The MSE
MSE_Cal <- function(data_true,data_est){
  return(1/length(data_true)*sum((data_true-data_est)^2))
}
MSE_Cal(true_data[,2],forecast_stl$mean) # 390698.7
MSE_Cal(true_data[,2],forecast_HW$mean) # 519957.9

# The MPE
MPE_Cal <- function(data_true,data_est){
  return(1/length(data_true)*sum(abs(data_true-data_est)/data_true))
}
MPE_Cal(true_data[,2],forecast_stl$mean) # 7.354%
MPE_Cal(true_data[,2],forecast_HW$mean) # 8.388%



# Dakar
demandData = data_list_demand[[5]]
demandData = na.omit(demandData)
dailyMax_all=aggregate(demandData[, c("MW")], by=list(demandData$Date), FUN=max)
dailyMax_all$YR = substr(as.factor(dailyMax_all[,1]),1,4)
dailyMax_raw = rbind(subset(dailyMax_all,dailyMax_all$YR==2011),subset(dailyMax_all,dailyMax_all$YR==2012))
dailyMax <- data.frame(Date = dailyMax_raw[,1],MW = dailyMax_raw[,2])
dailyDataTs=ts(dailyMax$MW, start=c(2011), end=c(2013), frequency=365)

# Decomposition
Decomp <- stl(dailyDataTs, s.window="period")
plot(Decomp)
# ARIMA
forecast_stl <- forecast(Decomp, h=365, method="arima")
plot(forecast_stl)
# HW
fit_HW <-HoltWinters(dailyDataTs)
# plot(fit_HW)
forecast_HW <- forecast(fit_HW, 365, method="arima")
plot(forecast_HW)
# true data
true_data <- subset(dailyMax_all,dailyMax_all$YR==2013)
plot(true_data[,2],type="l",,col="black",ylim=c(4000,9000))

# Three comparison
plot(forecast_stl$mean,col="blue",ylim=c(4000,9000))
par(new=T)
plot(forecast_HW$mean,col="red",ylim=c(4000,9000))
par(new=T)
plot(true_data[,2],type="l",,col="black",ylim=c(4000,9000))
# par(new=T)
# plot(forecast_stl_corr,type="l",,col="green",ylim=c(4000,9000))

# The MSE
MSE_Cal <- function(data_true,data_est){
  return(1/length(data_true)*sum((data_true-data_est)^2))
}
MSE_Cal(true_data[,2],forecast_stl$mean) # 576.8227
MSE_Cal(true_data[,2],forecast_HW$mean) # 485.3078

# The MPE
MPE_Cal <- function(data_true,data_est){
  return(1/length(data_true)*sum(abs(data_true-data_est)/data_true))
}
MPE_Cal(true_data[,2],forecast_stl$mean) ### 4.657%
MPE_Cal(true_data[,2],forecast_HW$mean) # 4.297%

