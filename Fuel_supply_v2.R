#options(error=utils::recover)
options(stringsAsFactors=FALSE)

Mons<-c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
Yr<-c("11","12","13")
#UniqueMon<-expand.grid(Mons,Yr)
UniqueMons<-outer(Mons,Yr, paste, sep="")
UniqueMons<-as.vector(UniqueMons)
dfnames<-paste("coal",UniqueMons, sep=".")
theColNames<-c("Stn_num","Transport","Stn_name","Capacity","Norm_Stock_Days","Daily_Req_MT","Act_Stock_MT","Act_Stock_Days","SevenDayStock","FourDayStock","Reason")
theColNames2<-c("Stn_num","Transport","Stn_name","Capacity","Norm_Stock_Days","Daily_Req_MT","Import_Stock_MT","Indig_Stock_MT","Act_Stock_Days","SevenDayStock","FourDayStock","Reason")

# split data into two groups: before Mar12 and after Mar12....
for (i in 1:length(UniqueMons[1:14])) {
#getData <- function(i) {
  print(i)
  theFile <- paste("/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/Fuel Supply/csv_data/coal/",UniqueMons[i],".csv", sep="")
  if (!file.exists(theFile)) next
  dfname<-read.table(file=theFile, sep=",", strip.white=TRUE, blank.lines.skip=TRUE, fill=TRUE, skip=15, header=FALSE, check.names=TRUE, comment="@", col.names=theColNames)
  if (dim(dfname)[2]>11) dfname[,12] <- NULL
  dfname <- subset(dfname, !is.na(as.numeric(Stn_num)) ) #remove all rows that are not associated with a station number
  dfname<-droplevels(dfname)
  
  if(i==1) {DF <- dfname} else {DF <- rbind(DF,dfname)}
  str(dfname)
  }
str(DF)

# Now do manipulations with compiled data... 
DF$Stn_num<-as.factor(DF$Stn_num)
DF$Transport<-as.factor(DF$Transport)
DF$Stn_name<-as.factor(DF$Stn_name)
#DF$Capacity<-as.numeric(DF$Capacity)
#DF$Norm_Stock_Days<-as.numeric(DF$Norm_Stock_Days)
#DF$Daily_Req_MT<-as.numeric(DF$Daily_Req_MT)
#DF$Act_Stock_MT<-as.numeric(DF$Act_Stock_MT)
DF$Act_Stock_Days<-as.numeric(DF$Act_Stock_Days)

n<-dim(DF)[1]
for (j in 1:n){
  if(DF$Act_Stock_Days[j]<7) {DF$Critical[j]=1} else 
    (DF$Critical[j]=0)
}
save(DF, file="CoalSupply_Apr11-Dec12.rsave")

ExpectedObs<-i*30 # i months x 30 days per month
length(levels(DF$Stn_name)) # number of stations reported
length(levels(DF$Stn_name)) # number of stations reported
#table(DF$Stn_name) # number of observations per station during the period of record
#round(table(DF$Stn_name)/ExpectedObs, digits=3) # Fraction of possible observations in record

########
######## Repeat for DF2
for (i in 15:length(UniqueMons)) {
  print(i)
  theFile <- paste("/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/Fuel Supply/csv_data/coal/",UniqueMons[i],".csv", sep="")
  if (!file.exists(theFile)) next
  dfname<-read.table(file=theFile, sep=",", strip.white=TRUE, blank.lines.skip=TRUE, fill=TRUE, skip=15, header=FALSE, check.names=TRUE, comment="@", col.names=theColNames2)
  dfname <- subset(dfname, !is.na(as.numeric(Stn_num)) ) #remove all rows that are not associated with a station number
  dfname<-droplevels(dfname)
  
  if(i==15) {DF2 <- dfname} else {DF2 <- rbind(DF2,dfname)}
  str(dfname)
}

# Now do manipulations with compiled data... 
#class(DF2)<-c(rep("Factor",3), rep("numeric",6), rep("character",3)) # why does't this work?

DF2$Stn_num<-as.factor(DF2$Stn_num)
DF2$Transport<-as.factor(DF2$Transport)
DF2$Stn_name<-as.factor(DF2$Stn_name)
#DF2$Capacity<-as.numeric(DF2$Capacity)
#DF2$Norm_Stock_Days<-as.numeric(DF2$Norm_Stock_Days)
#DF2$Daily_Req_MT<-as.numeric(DF2$Daily_Req_MT)
#DF2$Import_Stock_MT<-as.numeric(DF2$Import_Stock_MT)
#DF2$Indig_Stock_MT<-as.numeric(DF2$Indig_Stock_MT)
DF2$Act_Stock_Days<-as.numeric(DF2$Act_Stock_Days)

n<-dim(DF2)[1]
for (j in 1:n){
  if(DF2$Act_Stock_Days[j]<7) {DF2$Critical[j]=1} else 
    (DF2$Critical[j]=0)
}
########## up to here on Sept 30 2013.
########## error in if (DF2$Act_Stock_Days[j] < 7) { : 
######### missing value where TRUE/FALSE needed
######### 
# if (any(is.na(dfname)) warn("there are NA's" ,.immed=TRUE))
save(DF2, file="CoalSupply_May11-July13.rsav")

ExpectedObs<-(i-14)*30 # i months x 30 days per month
length(levels(DF2$Stn_name)) # number of stations reported
length(levels(DF2$Stn_num)) # number of stations reported
table(DF2$Stn_name) # number of observations per station during the period of record
round(table(DF2$Stn_name)/ExpectedObs, digits=2) # Fraction of possible observations in record

#############################
###########################
# can also grab one file at a time...
coal<-read.table(file="/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/Fuel Supply/csv_data/coal/apr11.csv",sep=",",strip.white=TRUE, blank.lines.skip=TRUE,fill=TRUE, skip=15, header=FALSE, check.names=TRUE)

names(coal)<-c("Date","Stn_num","Transport","Stn_name","Capacity","Norm_Stock_Days","Daily_Req_MT","Act_Stock_MT","Act_Stock_Days","<7days","<4days","Reason")

coal <- subset(coal, as.numeric(as.character(Stn_num)) > 0) #remove all rows that are not associated with a station number
which(is.na(coal))
coal<-droplevels(coal)
coal$Capacity<-as.numeric(coal$Capacity)
coal$Norm_Stock_Days<-as.numeric(coal$Norm_Stock_Days)
coal$Daily_Req_MT<-as.numeric(coal$Daily_Req_MT)
coal$Act_Stock_MT<-as.numeric(coal$Act_Stock_MT)
coal$Act_Stock_Days<-as.numeric(coal$Act_Stock_Days)

n<-dim(coal)[1]
for (i in 1:n){
  if(coal$Act_Stock_Days[i]<7) {coal$Critical[i]=1} else (coal$Critical[i]=0)
  }

dim(coal)[1]/length(levels(coal$Stn_name)) # number of days reported in month
length(levels(coal$Stn_name)) # number of stations reported
length(levels(coal$Stn_num)) # number of stations reported

save(coal, file="coal_supply.rsav")