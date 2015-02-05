###################################################
### Chunk 1: Load libraries and files
###################################################
# options(warn=1)
# if (!require(rgdal)) install.packages('rgdal')
# if (!require(spdep)) install.packages('spdep')
# if (!require(sp)) install.packages('sp')
# if (!require(MBA)) install.packages('MBA')
# if (!require(fields)) install.packages('fields')
# options(warn=2)

###################################################
### Chunk 2: Download Station List
###################################################
## SETWD() TO WHERE YOU WANT TO DOWNLOAD YOUR FILES
# setwd(/Users/elliotcohen/Dropbox/data/cohen-mccreight/data/raw) 
# 
# file <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-history.csv"
# repeat {
#   try(download.file(file, "data/ish-history.csv", quiet=TRUE))
#   if (file.info("data/ish-history.csv")$size > 0) {break}
# }
# st <- read.csv("data/ish-history.csv")
# dim(st)
# names(st)
# names(st)[c(3,10)] <- c("NAME", "ELEV")
# st <- st[,-5]
###################################################
### Chunk 3: Subset Station List and check if downlaods are successful
###################################################

# #FILTER STATION LIST BY LAT / LON AND RANGE----
# #ALTERNATIVELY OR IN ADDITION CAN FILTER BY COUNTRY (BELOW)
# #ENTER IN MIN / MAX LAT / LON AND TIME RANGE
# #LAT / LON IN (LAT / LON) X 1000
# #TIME IN YYYYMMDD
# lon.min <- 70000
# lon.max <- 90000
# lat.min <- 22000
# lat.max <- 38000
# dat.start <- 20110401     #April 01 2011
# dat.end <- 20130401       #April 01 2013
# st <- subset(st,LON > lon.min & LON < lon.max & LAT > lat.min & LAT < lat.max & BEGIN <= dat.start & END >= dat.end) #230 stations in lat-long region. 136 of which in India
# 
# ##FILTER BY COUNTRY----
# ##ENTER COUNTRY A2 CODES IN QUOTES("") BELOW
# ##A2 CODES CAN BE FOUND HERE http://www.worldatlas.com/aatlas/ctycodes.htm 
# ##TO INCLUDE COUNTRY USE ==, TO EXCLUDE COUNTRY USE !=
# ctry <-c("IN")
# ##IF NOT SUBSETTING BY COUNTRY, SKIP THIS AND SEE BELOW...
# st.set <- subset(st,CTRY == ctry)     #136 stations in Northern India
# ## USE THIS INSTEAD IF NOT SUBSETTING BY CTRY CODE...
# # st.set <- st

# ##EDIT LAT, LON, ELEV, BEGIN, AND END COLUMNS
# st.set$LAT <- st.set$LAT/1000
# st.set$LON <- st.set$LON/1000
# st.set$ELEV <- st.set$ELEV/10
# st.set$BEGIN <- as.numeric(substr(st.set$BEGIN, 1, 4)) #substring of character vector (x, start, stop)
# st.set$END <- as.numeric(substr(st.set$END, 1, 4))
# 
# Check if download successful...
# outputs <- as.data.frame(matrix(NA, dim(st.set)[1], 2))
# names(outputs) <- c("FILE", "STATUS")
# ## STATUS equal to 0 equals downloaded, 127 equals not downloaded    
# 
# outputs2 <- data.frame(FILE=NA, STATUS=NA)
# for (y in as.numeric(substr(dat.start,1,4)):as.numeric(substr(dat.end,1,4))) {
#   for (s in 1:dim(st.set)[1]) {
#     #outputs[s,1] <- paste(sprintf("%06d", st.set[s,1]), "-", sprintf("%05d", st.set[s,2]), "-", y, ".gz", sep="")
#     theFile <- paste(sprintf("%06d", st.set[s,1]), "-", sprintf("%05d", st.set[s,2]), "-", y, ".gz", sep="")
# #    wget <- paste("wget -P data/raw ftp://ftp.ncdc.noaa.gov/pub/data/noaa/", y, "/", outputs[s,1], sep="")
#     theWget <- paste("wget -P data/raw http://www1.ncdc.noaa.gov/pub/data/noaa/", y, "/", theFile, sep="")
#     print(theWget)
#     #outputs[s,2] <- try(system(wget, intern=FALSE, ignore.stderr=TRUE))
#     theTry <- try(system(theWget, intern=FALSE, ignore.stderr=TRUE))
#     print(theTry)
#     outputs2 <- rbind(outputs2, data.frame(FILE=theFile, STATUS=theTry))
#   }
# }
# 
# head(outputs)
# sum(outputs2$STATUS == 1)  # NOT downloaded
# sum(outputs2$STATUS == 0)  # Download complete
# 
# save(outputs2, file='outputs2.rsav')
# system("gunzip -r data/raw", intern = FALSE, ignore.stderr = TRUE)

# ###################################################
# ### Chunk 4: Get the actual station data and compile
# ###################################################
# # Done with code above.. data already downloaded!
# # Adjust code below as needed to get relevant data set.
# # Need to identify which column widths to extract....  EC 10-24-2012
# files <- list.files("/Users/elliotcohen/Dropbox/data/cohen-mccreight/data/raw/")
# column.widths <- c(4,6,5,4,2,2,2,2,1,6,7,5,5,5,4,3,1,1,4,1,5,1,1,1,6,1,1,1,5,1,5,1,5,1)
# column.names<-c("Total.Var.Char","USAFID","WBAN","YYYY","MM","DD","HR","Min","data.source","LAT","LONG","Report.Type","Elev","Identifier","QAQC","WIND.DIR","WIND.DIR.QAQC","WIND.Measure","WIND.SPD","WIND.SPD.QAQC","Cloud.height","Cloud.height.QAQC","Cloud.height.method","CAVOK","Visibility","Visibility.QAQC","Visibility.var","Visibility.var.QAQC","TEMP","TEMP.QAQC","DEW.POINT","DEW.POINT.QAQC","ATM.PRES","ATM.PRES.QAQC")
# 
# ## NOTES
# # TOTAL-VARIABLE-CHARACTERS includes remarks, additional data, and element quality section
# # The number of characters in the variable data section. The total record length = 105 + the value stored in this field.
# # Precip data will be contained in the variable data, if recorderd.
# 
# ## QAQC codes:
# # 0 = Passed gross limits check
# # 1 = Passed all quality control checks
# # 2 = Suspect
# # 3 = Erroneous
# # 4 = Passed gross limits check , data originate from an NCDC data source
# # 5 = Passed all quality control checks, data originate from an NCDC data source
# # 6 = Suspect, data originate from an NCDC data source
# # 7 = Erroneous, data originate from an NCDC data source
# # 9 = Passed gross limits check if element is present
# 
# ## General data flags:
# # 99999 = Missing. (number of 9's depends on character width of data field)
# 
# df<-as.data.frame(matrix(NA,length(files),length(column.names)))
# for (i in 1:length(files)) {
#   data <- read.fwf(paste("/Users/elliotcohen/Dropbox/data/cohen-mccreight/data/raw/", files[i], sep=""), column.widths)
#   names(data)<-column.names
# #   data <- data[,c(2:8,10:11,13,16,19,29,31,33)]
# #   names(data) <- c("USAFID","WBAN","YR","M","D","HR","MIN","LAT","LONG","ELEV","WIND.DIR", "WIND.SPD", "TEMP","DEW.POINT","ATM.PRES")
#   data$LAT <- data$LAT/1000
#   data$LONG <- data$LONG/1000
#   data$WIND.SPD <- data$WIND.SPD/10
#   data$TEMP <- data$TEMP/10
#   data$DEW.POINT <- data$DEW.POINT/10
#   data$ATM.PRES <- data$ATM.PRES/10
#   
# # write.csv(data, file=paste("/Users/elliotcohen/Dropbox/data/cohen-mccreight/data/", files[i], ".csv", sep=""), row.names=FALSE)
#   if(i==1) {dataset <- data} else {dataset <- rbind(dataset,data)}
# }
# 
# dim(dataset) # 1,242,299 x 34
# #save(dataset, file='Northern_India_Weather_Station_Data.rsav')
# #save(dataset, file='Wx_stn_data.rsav')
# #write.csv(dataset, file="Northern_India_weather_station_data.csv", row.names=FALSE)
# save(dataset, file="dataset.rsav")
# dataset$USAFID<-as.factor(dataset$USAFID)
# length(levels(dataset$USAFID))



###################################################
### chunk number 5: Alternative way to grab and compile the station data...
###################################################
# # Grab the raw data files one at a time and rbind into one large df containing all the station data for the period of interest (2011-2013)
# files <- list.files("/Users/elliotcohen/Dropbox/data/cohen-mccreight/data/raw")
# for (i in 1:length(files)){
#     st <- read.csv(file=paste("/Users/elliotcohen/Dropbox/data/cohen-mccreight/data/csv/", files[i],".csv", sep=""))
#     if(i==1) {st.set <- st} else {st.set <- rbind(st.set,st)}
# }
# 
# head(st.set)
# dim(st.set) # 1,242,299 x 34
# st.set<-as.data.frame(st.set)
# st.set$USAFID<-as.factor(st.set$USAFID)
# 
###################################################
### chunk number 6: Data cleanup and dim checks
###################################################
# # TEMP data
# 
# st<-subset(st.set, select=c(Total.Var.Char, USAFID, YYYY, MM, DD, HR, Min, LAT, LONG, Elev, TEMP, TEMP.QAQC))
# ## or
# load("dataset.rsav")  # the full set of station data
# 
# st<-subset(dataset, select=c(Total.Var.Char, USAFID, YYYY, MM, DD, HR, Min, LAT, LONG, Elev, TEMP, TEMP.QAQC))
# 
# st<-subset(st, TEMP.QAQC != 2) #remove suspect observations..
# st<-subset(st, TEMP.QAQC != 3) #remove suspect observations..
# st<-subset(st, TEMP.QAQC != 6) #remove suspect observations..
# st<-subset(st, TEMP.QAQC != 7) #remove suspect observations..
# 
# #st <- st[order(st$M, st$D, st$HR), ]
# # How many missing observations ?
# sum(st$TEMP == 999.9)
# st$TEMP[st$TEMP == 999.9] <- NA
# # sum(st$WIND.SPD == 999.9)
# # sum(st$WIND.DIR == 999)
# # sum(st$DEW.POINT == 999.9)
# # sum(st$ATM.PRES == 9999.9)
# # st$WIND.SPD[st$WIND.SPD == 999.9] <- NA
# # st <- st[st$MIN == 0,]
# # dim(st)
# 
# # remove rows with NA's
# # keeps rows with complete entries (e.g. no NAs)
# test<-st[complete.cases(st[,]),] 
# test2<-na.omit(st)
# identical(test,test2)         #TRUE
# 
# # check dimensions...
# length(which(is.na(st$TEMP))) #number of NA's in st$TEMP = 0
# dim(st)[1]-dim(test)[1]       #number of NA's removed by complete.cases = 0 --> dims match.
# st<-test
# 
# # create DATE attributes
# st$DATE <- as.Date(paste(st$YYYY, st$MM, st$DD, sep="-"), format="%Y-%m-%d")
# st$YM<-paste(st$YYYY, st$MM, sep="-")   #unique months
# st$YM<-as.factor(st$YM)
# length(levels(st$YM)) #36 months in the record
# save(st, file="st.rsav")

###################################################
### Data import, compliation and cleanup complete
### .... Start analysis
###################################################
###################################################
### Chunk 8: Hourly to daily and monthly aggregation
###################################################
# load("st.rsav")

# # Aggregate from hourly to daily and monthly
# d.mean<-aggregate(st$TEMP ~ st$DATE + st$USAFID, data=st, FUN=mean)
# names(d.mean)<-c("DATE","USAFID","TEMP")
# # d.mean$DATE <- as.Date(d.mean$DATE, format="%Y-%m-%d")
# daily<-d.mean
# save(daily, file="daily.rsav")  #raw version... additional QAQC below.
# load("daily.rsav")

# m.mean<-aggregate(st$TEMP ~ st$USAFID + st$YYYY + st$MM, data=st, FUN=mean)
# names(m.mean)<-c("USAFID","YYYY","MM","TEMP")
# m.mean$DATE <- as.Date(paste(m.mean$YYYY, m.mean$MM, "15", sep="-"))
# save(m.mean, file="m.mean.rsav")
 
# load("m.mean.rsav")
# #monthly<-m.mean


################################################
### Update Nov 11 2013
################################################
###################################################
### Chunk 1: Import libs, files and functions
###################################################
# setwd("/Users/elliotcohen/Dropbox/Data/Cohen-McCreight/")
# files <- list.files("/Users/elliotcohen/Dropbox/data/cohen-mccreight/")
# 
# require(plyr)
# require(reshape2)
# require(fields)
# library(ggplot2)
# library(scales)
# library(gdata)
# library(chron)
# 
# 
# # # Aggregate hourly to daily...
# # first compute mean, then min and max..
# d.mean.temp<-aggregate(st$TEMP ~ st$DATE + st$USAFID, data=st, FUN=mean)
# names(d.mean.temp)<-c("DATE","USAFID","TEMP")
# 
# # rename as "daily" for QAQC
# daily<-d.mean.temp
#
# #### QAQC
# # remove rows with NA's
# # keeps rows with complete entries (e.g. no NAs), except if the NA is in column 7 (WIND.SPD; e.g. if don't care about WIND.SPD yet...)
# test<-daily[complete.cases(daily[,]),] 
# identical(test, daily)  # TRUE
# 
# # check dimensions...
# length(which(is.na(daily$TEMP))) #number of NA's in TEMP column... 0
# dim(daily)[1]-dim(test)[1] #number of NA's removed from the DF.. 0
# # passes dim check. 
# 
# # quality control: check number of observations at each weather station.
# # Toss stations with less than 80% of the data
# daily$USAFID<-as.factor(daily$USAFID)
# table(daily$USAFID)
# 
# nstns<-length(levels(daily$USAFID))
# nobs<-length(range(daily$DATE)[1]:range(daily$DATE)[2]) #obs per station during period of record if none missing..
# stn_codes<-levels(daily$USAFID)       # vector of station names
# stn_codes<-as.data.frame(stn_codes)  
# 
# # Create a vector identifying which stations have less than 80% of the data and thus should be removed
# stn_codes$keep<-0  # empty vector to be populated in for loop...
# 
# for (i in 1:nstns){
#   if (table(daily$USAFID)[i]<0.8*nobs) {stn_codes$keep[i]<-"No"} else
#   {stn_codes$keep[i]<-"Yes"}
# }
# 
# keep<-subset(stn_codes, keep=="Yes")  #keep 108 of 136 statoins
# 
# #now subset "daily" to keep only the observations associated with a station that we want to keep
# keep.data<-daily[daily$USAFID %in% keep$stn_codes,] 
# test<-subset(daily, USAFID %in% keep$stn_codes)
# identical(keep.data, test) #true
# daily<-keep.data
# daily<-droplevels(daily)
# 
# # check to make sure unwated stations remove-->TRUE
# table(daily$USAFID)
# 
# # reassign to proper name
# d.mean.temp<-daily
# save(d.mean.temp, file="d.mean.temp.rsav")
# load("d.mean.temp.rsav")
# 
####################################
#### Repeat for daily MAXTEMP
#####################################
# d.max.temp<-aggregate(st$TEMP ~ st$DATE + st$USAFID, data=st, FUN=max)
# names(d.max.temp)<-c("DATE","USAFID","MAXTEMP")

# # rename as "daily" for QAQC
# daily<-d.max.temp 

#### QAQC
# # remove rows with NA's
# # keeps rows with complete entries (e.g. no NAs)
# test<-daily[complete.cases(daily[,]),] 
# identical(test, daily)  # TRUE
# 
# # check dimensions...
# length(which(is.na(daily$MAXTEMP))) #number of NA's in TEMP column... 0
# dim(daily)[1]-dim(test)[1] #number of NA's removed from the DF.. 0
# # passes dim check. 
# 
# # quality control: check number of observations at each weather station.
# # Toss stations with less than 80% of the data
# daily$USAFID<-as.factor(daily$USAFID)
# table(daily$USAFID)
# 
# nstns<-length(levels(daily$USAFID))
# nobs<-length(range(daily$DATE)[1]:range(daily$DATE)[2]) #obs per station during period of record if none missing..
# stn_codes<-levels(daily$USAFID)       # vector of station names
# stn_codes<-as.data.frame(stn_codes)  
# 
# # Create a vector identifying which stations have less than 80% of the #data and thus should be removed
# stn_codes$keep<-0  # empty vector to be populated in for loop...
# 
# for (i in 1:nstns){
#   if (table(daily$USAFID)[i]<0.8*nobs) {stn_codes$keep[i]<-"No"} else
#   {stn_codes$keep[i]<-"Yes"}
# }
# 
# keep<-subset(stn_codes, keep=="Yes")  #keep 108 of 136 statoins
# 
# #now subset "daily" to keep only the observations associated with a station that we want to keep
# keep.data<-daily[daily$USAFID %in% keep$stn_codes,] 
# test<-subset(daily, USAFID %in% keep$stn_codes)
# identical(keep.data, test) #true
# daily<-keep.data
# daily<-droplevels(daily)
# 
# # reassign to proper name
# d.max.temp<-daily
# save(d.max.temp, file="d.max.temp.rsav")
# load("d.max.temp.rsav")

# ##################################################
# ## Repeat for daily minTEMP
# ##################################################
# d.min.temp<-aggregate(st$TEMP ~ st$DATE + st$USAFID, data=st, FUN=min)
# names(d.min.temp)<-c("DATE","USAFID","MINTEMP")
# 
# # rename as "daily" for QAQC
# daily<-d.min.temp
# 
# #### QAQC
# # remove rows with NA's
# # keeps rows with complete entries (e.g. no NAs)
# test<-daily[complete.cases(daily[,]),] 
# identical(test, daily)  # TRUE
# 
# # check dimensions...
# length(which(is.na(daily$MINTEMP))) #number of NA's in TEMP column... 0
# dim(daily)[1]-dim(test)[1] #number of NA's removed from the DF.. 0
# # passes dim check. 
# 
# # quality control: check number of observations at each weather station.
# # Toss stations with less than 80% of the data
# daily$USAFID<-as.factor(daily$USAFID)
# table(daily$USAFID)
# 
# nstns<-length(levels(daily$USAFID))
# nobs<-length(range(daily$DATE)[1]:range(daily$DATE)[2]) #obs per station during period of record if none missing..
# stn_codes<-levels(daily$USAFID)       # vector of station names
# stn_codes<-as.data.frame(stn_codes)  
# 
# # Create a vector identifying which stations have less than 80% of the data and thus should be removed
# stn_codes$keep<-0  # empty vector to be populated in for loop...
# 
# for (i in 1:nstns){
#   if (table(daily$USAFID)[i]<0.8*nobs) {stn_codes$keep[i]<-"No"} else
#   {stn_codes$keep[i]<-"Yes"}
# }
# 
# keep<-subset(stn_codes, keep=="Yes")  #keep 108 of 136 statoins
# 
# #now subset "daily" to keep only the observations associated with a station that we want to keep
# keep.data<-daily[daily$USAFID %in% keep$stn_codes,] 
# test<-subset(daily, USAFID %in% keep$stn_codes)
# identical(keep.data, test) #true
# daily<-keep.data
# daily<-droplevels(daily)
# 
# # reassign to proper name
# d.min.temp<-daily
# save(d.min.temp, file="d.min.temp.rsav")
# load("d.min.temp.rsav")
# 
# # merge min, mean and max
# dim(d.min.temp)
# dim(d.mean.temp)
# dim(d.max.temp)
# test<-merge(d.min.temp, d.mean.temp, by=c("DATE","USAFID"))
# names(test)<-c("DATE","USAFID","MINTEMP","MEANTEMP")
# test2<-merge(test,d.max.temp,by=c("DATE","USAFID"))
# test2[,3:5]<-round(test2[,3:5], digits=2)
# d.temp<-test2
# save(d.temp, file="d.temp.rsav") #not-melted
# 
# dailytemp<-melt(d.temp, id.vars=c("DATE","USAFID"))
# save(dailytemp, file="dailytemp.rsav")
# 
# testplot<-subset(dailytemp, USAFID=="420270")
# p<-ggplot(testplot, aes(x=DATE, y=value, group=variable, colour=variable)) + geom_line()
# p 
###################################
### repeat for monthly aggregation... MAX
###################################
# m.max<-aggregate(st$TEMP ~ st$USAFID + st$YYYY + st$MM, data=st, FUN=max)
# names(m.max)<-c("USAFID","YYYY","MM","MAXTEMP")
# m.max$DATE <- as.Date(paste(m.max$YYYY, m.max$MM, "15", sep="-"))
# m.max.temp<-m.max
# save(m.max.temp, file="m.max.temp.rsav")
# load("m.max.temp.rsav")
# # rename as monthly for QAQC
# monthly<-m.max.temp
# 
# # QAQC
# stn_codes<-levels(monthly$USAFID)       # vector of station names
# stn_codes<-as.data.frame(stn_codes)  
# stn_codes$keep2<-0  # empty vector to be populated in for loop...
# 
# for (i in 1:nstns){
#   if (table(monthly$USAFID)[i]<length(levels(monthly$YM))) {stn_codes$keep2[i]<-"No"} else {stn_codes$keep2[i]<-"Yes"}
# }
# 
# keep<-subset(stn_codes, keep2=="Yes")
# 
# #now subset "monthly" to keep only the observations associated with a station that we want to keep
# keep.data<-monthly[monthly$USAFID %in% keep$stn_codes,] 
# test<-subset(monthly, USAFID %in% keep$stn_codes)
# identical(keep.data, test) #true
# monthly<-keep.data
# monthly<-droplevels(monthly)
# 
# # how many NA's are there in the monthly TEMP data...
# length(which(is.na(monthly$MAXTEMP))) # 0
# 
# # reassign to proper name
# m.max.temp<-monthly
# save(m.max.temp, file="m.max.temp.rsav")
# load("m.max.temp.rsav")
# 
# ## repeat for monthly aggregation... MEAN
# m.mean<-aggregate(st$TEMP ~ st$USAFID + st$YYYY + st$MM, data=st, FUN=mean)
# names(m.mean)<-c("USAFID","YYYY","MM","meanTEMP")
# m.mean$DATE <- as.Date(paste(m.mean$YYYY, m.mean$MM, "15", sep="-"))
# m.mean.temp<-m.mean
# save(m.mean.temp, file="m.mean.temp.rsav")
# load("m.mean.temp.rsav")
# # rename as monthly for QAQC
# monthly<-m.mean.temp
# 
# # QAQC
# stn_codes<-levels(monthly$USAFID)       # vector of station names
# stn_codes<-as.data.frame(stn_codes)  
# stn_codes$keep2<-0  # empty vector to be populated in for loop...
# 
# for (i in 1:nstns){
#   if (table(monthly$USAFID)[i]<length(levels(monthly$YM))) {stn_codes$keep2[i]<-"No"} else {stn_codes$keep2[i]<-"Yes"}
# }
# 
# keep<-subset(stn_codes, keep2=="Yes")
# 
# #now subset "monthly" to keep only the observations associated with a station that we want to keep
# keep.data<-monthly[monthly$USAFID %in% keep$stn_codes,] 
# test<-subset(monthly, USAFID %in% keep$stn_codes)
# identical(keep.data, test) #true
# monthly<-keep.data
# monthly<-droplevels(monthly)
# 
# # how many NA's are there in the monthly TEMP data...
# length(which(is.na(monthly$meanTEMP))) # 0
# 
# # reassign to proper name
# m.mean.temp<-monthly
# save(m.mean.temp, file="m.mean.temp.rsav")
# load("m.mean.temp.rsav")

# ##############################
# # repeat monthly aggregation using ddply
# ############################
# monthly<-ddply(st, .(USAFID, LAT, LONG, Elev, YYYY, MM, YM), summarize, MINTEMP=min(TEMP),MEANTEMP=mean(TEMP), MAXTEMP=max(TEMP))
# names<-names(monthly)
# 
# str(monthly)
# dim(monthly)      #3836 x 8
# table(monthly$USAFID)  # some stations have few obs --> remove them.
# 
# monthly$DATE<-as.Date(paste(monthly$YM, "15", sep="-"), format="%Y-%m-%d")
# stn_codes<-levels(monthly$USAFID)       # vector of station names
# stn_codes<-as.data.frame(stn_codes)  
# stn_codes$keep2<-0  # empty vector to be populated in for loop...
# 
# for (i in 1:nstns){
#   if (table(monthly$USAFID)[i]<length(levels(monthly$YM))) {stn_codes$keep2[i]<-"No"} else {stn_codes$keep2[i]<-"Yes"}
# }
# 
# keep<-subset(stn_codes, keep2=="Yes")
# 
# #now subset "monthly" to keep only the observations associated with a station that we want to keep
# keep.data<-monthly[monthly$USAFID %in% keep$stn_codes,] 
# test<-subset(monthly, USAFID %in% keep$stn_codes)
# identical(keep.data, test) #true
# monthly<-keep.data
# monthly<-droplevels(monthly)
# table(monthly$USAFID)  # check that unwanted stations have been removed-->TRUE
# 
# # how many NA's are there in the monthly TEMP data...
# length(which(is.na(monthly$MAXTEMP))) # 0
# 
# #####################################
# ### Additional QAQC
# #####################################
# ## look for complete duplicate records
# # duplicated(monthly)  #logical. TRUE/FALSE for each row in monthly
# sum(duplicated(monthly)) # none
# 
# # Show repeat records
# monthly[duplicated(monthly),]  # none
# 
# # # Original data with repeats removed. These do the same:
# # test<-unique(monthly)
# # test2<-monthly[!duplicated(monthly),]
# # identical(test, test2)
# 
# # check for NAs --> NONE
# sum(is.na(monthly)) 
# look<-which(is.na(monthly[,]), arr.ind=TRUE)
# monthly[look[,1],]
# 
# # find months with more records than there are stations..
# length(levels(monthly$USAFID)) # 107 ISH stations...
# table(monthly$DATE)
# 
# #look for multiple entries for same USAFID in a given month...
# test1<-subset(monthly, DATE=="2013-08-15")
# test2<-subset(monthly, DATE=="2013-09-15")
# test3<-subset(monthly, DATE=="2013-10-15")
# 
# duplicated(test1$USAFID)
# duplicated(test2$USAFID)
# duplicated(test3$USAFID)
# 
# ## LAT-LONG-ELEV attributes are slightly different for the same ISH station in some cases... use ddply to compute mean of multiple records at same location in same month.
# 
# monthly<-ddply(monthly, .(USAFID, DATE, YYYY, MM, YM), numcolwise(mean))
# monthly<-droplevels(monthly)
# length(levels(monthly$USAFID))  # still 107 --> good!
# dim(monthly)[1]/length(levels(monthly$USAFID))  #34 exactly!
# 
# ## END QAQC
# ## reassign to proper name and save
# m.temp<-monthly
# save(m.temp, file="m.temp.rsav")
# load("m.temp.rsav")
# 
# monthlytemp<-melt(m.temp, id.vars=c("USAFID","DATE","LAT","LONG","Elev"), measure.vars=c("MINTEMP","MEANTEMP","MAXTEMP"))
# save(monthlytemp, file="monthlytemp.rsav")
###
### Done data import/cleanup and compliation (Nov 13 2013.)
###
###############################
## Create contour map of temps.
##############################
load("monthlytemp.rsav")
monthly<-monthlytemp
library(akima)
library(fields)

mons<-seq(as.Date("2011-04-15", format="%Y-%m-%d"), as.Date("2012-03-15", format="%Y-%m-%d"), by="months")

## PLOT - temperature contour map of India Apr 2011 - March 2012.
par(mfrow=c(1,1))         # default
# par(mar=c(5,4,4,2)+0.1) # default
par(mar=c(2,2,1.5,1)+0.1) 
par(oma=c( 0,0,0,4))     # margin of 3 spaces width at right hand side
set.panel(3,4)           # 3X4 matrix of plots

# now draw plots using image() command
for (i in 1:12){
  data<-subset(monthly, DATE==mons[i])
  zz0<-interp(x=data$LONG, y=data$LAT, z=data$MAXTEMP)
  image(zz0, zlim=c(9,50), col=topo.colors(n=64,alpha=1), main=paste(levels(months(1))[data$MM[1]], "Max Temp. (C)", sep=" "), lab.breaks=NULL)
  contour(zz0, add=T)
}

set.panel(1,4) # nXm matrix of plots
par(oma=c(0,0,0,1))# reset margin to be much smaller.
par(mar=c(0,0,0,0)+0.0) 
image.plot(legend.only=TRUE, legend.width=2.5, zlim=c(9,50), col=topo.colors(n=64,alpha=1), cex.axis=1.5) 
# image.plot tricked into  plotting in margin of old setting 
# end plot

# Can also plot stations one-at-a-time (nstns=108)
hourly.test<-subset(st, USAFID==420270)
daily.test<-subset(d.temp, USAFID==420270)
monthly.test<-subset(monthly, USAFID==420270)

plot(hourly.test$DATE, hourly.test$TEMP, main="Temperature readings", ylab="Temperature (Degrees C)", xlab="Month", col="grey")
points(daily.test$DATE, daily.test$TEMP, col="brown")
lines(monthly.test$DATE, monthly.test$TEMP, type="b", pch=16)
legend("topleft", c("Hourly", "Daily mean", "Monthly mean"), inset=0.02, pch=c(1,1,16), col=c("grey", "red", "black"))

##################################################
### Find the kNN closest weather stations to power stations
################################################
###################################################
### Chunk 1: Import libs, files and functions
###################################################
require(plyr)
require(reshape2)
require(fields)
library(ggplot2)
library(scales)
library(gdata)
library(chron)

setwd("/Users/elliotcohen/Dropbox/Data/Cohen-McCreight/")
files <- list.files("/Users/elliotcohen/Dropbox/data/cohen-mccreight/")

# Load power station data
load("Stationwise.rsav")
load("Stationwise2.rsav")
load("CGSmeta.rsav")
load("CGSmeta2.rsav")
load("mergeDF.rsav")

range(mergeDF$Date)  # "2011-04-01" "2012-03-01"
range(Stationwise$Date)  # "2011-04-01" "2012-03-01"

# Load weather station data (created above)
load("dataset.rsav"); #str(dataset)   #ISH station data at 136 locations in India
load("st.rsav"); #str(st)              #same as above but select columns only
load("d.temp.rsav"); str(d.temp)        #daily min/mean/max temp (not melted)
load("dailytemp.rsav"); str(dailytemp)  #daily min/mean/max temp (melted)
load('m.temp.rsav'); str(m.temp)            #monthly min/mean/max temp (not melted)
load("monthlytemp.rsav"); str(monthlytemp)  #monthly min/mean/max temp (melted)

dataset$USAFID<-as.factor(dataset$USAFID)
monthly$USAFID<-as.factor(monthly$USAFID)

length(levels(dataset$USAFID)) #136
length(levels(daily$USAFID))   #108 stations passed QAQC
length(levels(monthly$USAFID)) #107 stations passed QAQC



# ###################################
# ## match power plants to closest weather stations
# ###################################
# ## Grab ISH meta data
# ## pare down ISH data to individual sites (USAFID levels).
# 
# load("m.max.temp2.rsav")
# monthly<-m.max.temp2  # assign nickname for ease of coding..
# whUniques <- match(levels(monthly$USAFID), as.character(monthly$USAFID))
# ISHmeta <- monthly[whUniques,]
# 
# mons<-seq(range(monthly$DATE)[1], range(monthly$DATE)[2], by="months")
# length(mons)  #34 months in period of record
# dim(monthly)[1]/length(levels(monthly$USAFID)) #34 records for every USAFID
# 
# # subset to only the metadata (e.g. not measurement vars)
# ISHmeta<-ISHmeta[,c(1,6:8)]
# summary(ISHmeta[,c("LONG", "LAT")])
# 
# ## Now grab power plant metadata
# ## CGS lat-long cords are in CGSmeta2
# load("CGSmeta2.rsav")
# summary(CGSmeta2[,c("X.DEC",'Y.DEC')])  ## positive! nice.
# 
# # find the closest ISH weather station to any set of stations ("stn")
# findClosestISH <- function(stn, knn) {
#   ## find station name, id, lat,
#   llDist <- rdist.earth(matrix(c(stn$X.DEC,stn$Y.DEC), ncol=2),
#                         matrix(c(ISHmeta$LONG, ISHmeta$LAT), ncol=2),
#                         miles=FALSE, R=6371) ## mean radius in km
#   sortDistInds <- sort( llDist, ind=TRUE)$ix
#   return( cbind(stnCode=stn$Stn_code,
#                 distance.km=llDist[sortDistInds[1:knn]],
#                 ISHmeta[sortDistInds[1:knn],] ) )
# }
# knn=5  ## i kept a few to look at, though I end up throwing them out
# ISHTempsNearby <- dlply( CGSmeta2, 1, findClosestISH, knn=knn )
# ISHClosest <- ldply(ISHTempsNearby,
#                     function(ll) ll[which(ll$distance.km==min(ll$distance.km)),])
# 
# save(ISHClosest, file="ISHClosest.rsav")
# 
# ## ISHClosest contains CGS paired with the closest ISH station.  
# # Then to grab the observed data, subset(m.max.temp, USAFID %in% ISHClosest$USAFID)
# # Then merge(Stationwise, ISHClosest, by=Stn_code)
# identical(mergeDF[,1:26], Stationwise[,1:26])  #TRUE
# Stationwise<-merge(Stationwise[,1:26], ISHClosest, by="Stn_code")
# #Stationwise<-subset(Stationwise, select=c(9:10,1:8,11:12,15:29,31:35))
# #Stationwise<-Stationwise[order(Stationwise$Date),]
# names<-names(Stationwise)
# names(Stationwise)<-c(names[1:27],"ISH.dist.km","USAFID","ISH.LAT","ISH.LONG","ISH.ELEV")
# 
# # order along 1st column, ties along 2nd, ...
# #Stationwise<-Stationwise[do.call(order,Stationwise), ]
# 
# save(Stationwise, file="Stationwise.rsav")

##############################
### up to here on Nov 12 2013....
#############################
# ## merge Stationwise with actual temp measurements...
# load("Stationwise.rsav")
# load("m.temp.rsav")
# temp<-subset(m.temp, select=c("USAFID","DATE","MINTEMP","MEANTEMP","MAXTEMP"))
# names<-names(temp)
# names(temp)<-c("USAFID","Date", names[3:5])
# master<-merge(Stationwise, temp, by=c("USAFID","Date"))
# #master<-master[,-28]  # remove redundant column
# dim(master) # 276 x 35
# save(master,file="master.rsav")


# Now compute supply-chain impacts to beneficiaries.
# organize master into time-invariant and time-dependent vars...
#test<-subset(master, select=c(3:17,1,28:32,18:27))
test2<-subset(master, select=c(3:13,1,29:32,2,15,16:27,33:35))
test3<-subset(test2, select=c(1:11,19,20,12:16,17,18,31:33,21:30))

test4<-melt(test3, id.vars=c(1:23))
names<-names(test4)
names(test4)<-c(names[1:23],"Beneficiary","Allocation")
test4<-subset(test4, select=c(1:12,14:23,13,24,25))
head(test4)  # columns 1:17 are time invariant, columns 18:25 are time dependent.

# order alphabeticaly by Stn_code
test5<-test4[order(test4$Stn_code), ]
# then order chronologically
test6<-test5[order(test5$POSIXct), ]

# re-assign back to test.
test<-test6

## compute the total allocation from all CGS to each beneficiary in each month
monsum<-aggregate(Allocation ~ Beneficiary + POSIXct, data=test, FUN=sum)
annsum<-aggregate(Allocation ~ Beneficiary, data=test, FUN=sum)
## order along 1st column, ties along 2nd, ...
monsum<-monsum[do.call(order, monsum), ]

# use monsum to cross-chech CGSmelt...
check1<-cbind(REAmelt, monsum[,3])
check2<-merge(REAmelt, monsum, by=c("Beneficiary","POSIXct"))
names<-c("Beneficiary","POSIXct","Date","REA.Allocation","Stationwise.Allocation")
names(check1)<-names
names(check2)<-names
identical(round(check1$REA.Allocation, digit=0), round(check1$Stationwise.Allocation, digits=0)) #TRUE
identical(round(check2$REA.Allocation, digit=2), round(check2$Stationwise.Allocation, digits=2)) #TRUE


## vector of beneficiary names
states<-levels(monsum$Beneficiary)

## compute a weight function 
## each station is given a weight based on its contribution to a particular beneficiary (state) divided by the contribution of all stations to that beneficiary for a particular month.
## subset(monsum, Beneficiary==states[i], select=Allocation) gives the total contirbution of all stations to a particular beneficiary for each month.  This vector is repeated 22 times (number of stations) such that it is the denominator of the weight function for each station contributing to that beneficiary in that month.  
## test$Allocation[set] gives the numerator of the weight function (e.g. MW allocation from a particular station)

test$Weight<-NA
annsum$check<-NA
# subset(monsum, Beneficiary==states[i], select=Allocation) --> vector of monthly aggregate allocations from CGS to beneficiary[i], ordered chronologially (length=12)
# test$Allocation[set] --> vector of stationwise monthly allocations from CGS to beneficiary[i] 

for(i in 1:length(states)){
  set<-which(test$Beneficiary==states[i])
  data<-test[set,]
  data<-data[order(data$POSIXct),]
  numerator<-data$Allocation
  data2<-as.matrix(subset(monsum, Beneficiary==states[i], select=Allocation))
  denom<-rep(data2, each=length(levels(test$Stn_code)))
  test$Weight[set]<-numerator/denom
  annsum$check[i]<-sum(test$Allocation[set])
  }

## check that all NA's replaced by a weight value
sum(is.na(test))  # zero

## check that annsum$Allocation == annsum$check
annsum       # looks good.

## check that weights add up to one for each beneficiary in each month...
check<-aggregate(Weight ~ Beneficiary + POSIXct, data=test, FUN=sum)
check$Weight<-round(check$Weight, digits=0)
check    #Yes, all sum up to 1.

test2<-ddply(test, .(POSIXct, Beneficiary), summarize, RateOfSale=sum(RateOfSale*Weight), PAFM=sum(PAFM*Weight), MINTEMP=sum(MINTEMP*Weight), MEANTEMP=sum(MEANTEMP*Weight), MAXTEMP=sum(MAXTEMP*Weight), ISH.dist.km=sum(ISH.dist.km*Weight), Allocation=sum(Allocation), Weight=round(sum(Weight), digits=0))

test2[,3:9]<-round(test2[,3:9], digits=2)
names<-names(test2)
names(test2)<-c(names[1:4],"CGSmintemp","CGSmeantemp","CGSmaxtemp", names[8:10])
supplychain<-test2

save(supplychain, file="supplychain.rsav")

# Note: these are supply chain, production-weighted averages.  For example, MAXTEMP is not the maxtemp of the beneficiary state in that month, but the production weighted average of the temperatures at all of the CGS supplying power to the beneficiary.

#####################################
##### Repeat for capital cities of NR states
#####################################
# CGS stations matched to nearest ISH station for impact to production
# State capitals matched to nearest ISH station for impact to consumption
options(stringsAsFactors=FALSE)
Beneficiary<-levels(monsum$Beneficiary)
capital<-c("Chandigarh", "Delhi", "Shimla", "Chandigarh","Srinagar", "Chandigarh","Jaipur","Dehradun","Luchnow","Delhi")
pop<-c(856900,10400900,150600,856900,948100,856900,2462500,426674,2186000,10400900)
lat<-c(30.750,28.670,31.110,30.750,34.090,30.750,26.920,30.3157,26.8470,28.670)
long<-c(76.780,77.210,77.160,76.780,74.790,76.780,75.800,78.3586,80.9470, 77.210)
states<-data.frame(Beneficiary=Beneficiary, capital=capital, pop=pop, lat=lat, long=long)

save(states, file="states.rsav")
###################################
## match power plants to closest weather stations
###################################
## TEST 1 ##
# Grab ISH meta data
# pare down ISH data to individual sites (USAFID levels).

load("m.temp.rsav")
monthly<-m.temp # assign nickname for ease of coding..
whUniques <- match(levels(monthly$USAFID), as.character(monthly$USAFID))
ISHmeta <- monthly[whUniques,]

mons<-seq(range(monthly$DATE)[1], range(monthly$DATE)[2], by="months")
length(mons)  #34 months in period of record
dim(monthly)[1]/length(levels(monthly$USAFID)) #34 records for every USAFID

# subset to only the metadata (e.g. not measurement vars)
ISHmeta<-ISHmeta[,c(1,6:8)]
summary(ISHmeta[,c("LONG", "LAT")])

## Now grab state metadata.  
load("states.rsav")
summary(states[,c("long",'lat')])  ## positive! nice.

# find the closest ISH weather station to any set of stations ("stn")
findClosestISH <- function(states, knn) {
  ## find station name, id, lat,
  llDist <- rdist.earth(matrix(c(states$long,states$lat), ncol=2),
                        matrix(c(ISHmeta$LONG, ISHmeta$LAT), ncol=2),
                        miles=FALSE, R=6371) ## mean radius in km
  sortDistInds <- sort( llDist, ind=TRUE)$ix
  return( cbind(match=states$capital,
                distance.km=llDist[sortDistInds[1:knn]],
                ISHmeta[sortDistInds[1:knn],] ) )
}
knn=5  ## i kept a few to look at, though I end up throwing them out
ISHTempsNearby <- dlply( states, 1, findClosestISH, knn=knn )
ISHClosest <- ldply(ISHTempsNearby,
                    function(ll) ll[which(ll$distance.km==min(ll$distance.km)),])

save(ISHClosest, file="ISHClosest.rsav")

# grab the observed temp data (m.temp) and merge with master
supplychain<-merge(supplychain, ISHClosest[,1:4], by="Beneficiary")
supplychain$DATE<-as.Date(supplychain$POSIXct)
supplychain<-merge(supplychain, m.temp, by=c("USAFID", "DATE"))

#test<-subset(supplychain, select=c(3,13,1,14,21:23,7,8,9,10,11,5,6))
test<-subset(supplychain, select=c("DATE","Beneficiary","Allocation","PAFM","RateOfSale","ISH.dist.km","CGSmintemp","CGSmeantemp","CGSmaxtemp","match","USAFID","distance.km","MINTEMP","MEANTEMP","MAXTEMP"))
names<-names(test)
names(test)<-c("Date","Beneficiary","TB.Allocation","TB.PAF","TB.RateofSale","TB.ISH.dist","TB.min.t","TB.mean.t","TB.max.t","match","IB.USAFID","IB.ISH.dist","IB.min.t","IB.mean.t","IB.max.t")
supplychain<-test
save(supplychain, file="supplychain.rsav")
# use this in risk model!
##############################
#### up to here on NOV 13 2013.
###
##
###############################