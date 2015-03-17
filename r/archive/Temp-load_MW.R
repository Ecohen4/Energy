# This script loads hourly temperature and demand data. 
# It produces and plots:
#  (a) the minimum daily demand versus the minimum daily temperature and 
#  (b) the maximum daily demand versus the maximum daily temperature

#install.packages("segmented")
require(segmented)

# Set working directory
# setwd("~/Documents/Research/Misc Analyses/Electricity/Demand vs Temp/")
setwd("/Users/elliotcohen/Dropbox/Data/Cohen-McCreight/")
#load("stnData.rsav")  # raw ISD hourly Temp and precip data
#load("metdata.rsav")  # ISD station metadata

load("StateTemps.rsav")  # hourly temp data for Beneficiary States
load("d.StateTemps.rsav") # daily temp data for Beneficiary States

StateTemps$ID<-as.factor(StateTemps$ID)
levels(StateTemps$ID)
Delhi<-subset(StateTemps, ID=="Delhi")

# # Mike's code....
# # Load hourly temperatures. File format should be a one-column CSV.
# temps <- read.csv("temps.csv", header = FALSE, sep = ",", row.names = NULL)
# # Convert temps to deg F
# temps <- temps*1.8+32
# 
# # Load hourly electricity demand. File format should be a one-column CSV.
# demand <- read.csv("demand.csv", header = FALSE, sep = ",", row.names = NULL)
# 
# # Set number of days in the year
# n.days <- dim(temps)[1]/24
# 
# # Create dataframe with overnight minimum temperature and demand
# daily.min <- as.data.frame(matrix(9999, ncol = 2, nrow = (n.days-1)))
# colnames(daily.min) <- c("temp","demand")
# for (i in 1:(n.days-1))
# {
#   daily.min$temp[i] <- min(temps[(i*24-11):(i*24+12),1])
#   daily.min$demand[i] <- min(demand[(i*24-11):(i*24+12),1])
# }

## 
# Fit segmented linear regression to two segments with estimated breakpoint
lm.min <- lm(demand~temp,data=daily.min)
seg.min <- segmented(lm.min,seg.Z=~temp,psi=list(temp=20),control=seg.control(display=FALSE))
par(new=TRUE)
plot(seg.min,col="green")

# Plot minimum overnight demand vs minimum overnight temperature
# Overlay segmented linear regression
plot(daily.min$temp,daily.min$demand)
plot(seg.min,add=TRUE,res=FALSE,link=TRUE,col="green")

# Create dataframe with daily maximum temperature and demand
daily.max <- as.data.frame(matrix(9999, ncol = 2, nrow = n.days))
colnames(daily.max) <- c("temp","demand")
for (i in 1:n.days)
{
  daily.max$temp[i] <- max(temps[(i*24-23):(i*24),1])
  daily.max$demand[i] <- max(demand[(i*24-23):(i*24),1])
}

# Fit segmented linear regression to two segments with estimated breakpoint
lm.max <- lm(demand~temp,data=daily.max)
seg.max <- segmented(lm.max,seg.Z=~temp,psi=list(temp=58),control=seg.control(display=FALSE))

# Plot maximum overnight demand vs maximum overnight temperature
# Overlay segmented linear regression
plot(daily.max$temp,daily.max$demand)
plot(seg.max,add=TRUE,res=FALSE,link=TRUE,col="green")


# Plot maximum and minimum together, along with segmented regression fits
plot(daily.min$temp,daily.min$demand,ylim=c(min(daily.min$demand),max(daily.max$demand)),xlim=c(min(daily.min$temp),max(daily.max$temp)))
plot(seg.min,add=TRUE,res=FALSE,link=TRUE,col="green")
par(new=TRUE)
plot(daily.max$temp,daily.max$demand,ylim=c(min(daily.min$demand),max(daily.max$demand)),xlim=c(min(daily.min$temp),max(daily.max$temp)),axes=FALSE,xlab="",ylab="",pch="x")
plot(seg.max,add=TRUE,res=FALSE,link=TRUE,col="red")