# ###################################################
# ### Coal Supply to Thermal Power Stations
# ###################################################
# #setwd("/Users/elliotcohen/Dropbox/Data/Cohen-McCreight")
# setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")
# library(plyr)
# library(reshape2)
# library(ggplot2)
# 
# #options(error=utils::recover)
# options(stringsAsFactors=FALSE)
# 
# ###################################################
# ### Create names and structures for incoming data
# ###################################################
# files<-list.files("/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/Fuel Supply/csv_data/coal/"); files
# 
# dummy<-unlist(strsplit(files, "[.]"))
# dummy1<-matrix(dummy, nrow=65, byrow=TRUE)
# uniqueMons<-dummy1[,1]
# 
# ### Use this...
# ChronMons<-uniqueMons[order(as.Date(paste(15,uniqueMons, sep=""), format="%d%b%y"))]
# 
# ### Other handy date manipulations...
# # Date<-rep(NA, length(uniqueMons))
# # for(i in 1:length(uniqueMons)){
# #   Date[i]<-as.Date(paste(15,uniqueMons[i], sep=""), format="%d%b%y")
# # }
# # ChronDate<-Date[order(as.Date(Date, format="%d/%m/%Y"))]
# # #dates<-seq(as.Date("2008-03-15"), as.Date("2013-07-15"), by="months")
# 
# ## Define variables for getData
# nastrings<-c("CENTRAL ELECTRICITY AUTHORITY","OPERATION MONITORING DIVISION","DAILY COAL REPORT","Page 1 of3","Page 2 of3", "Page 3 of3", "REGION/", "STATE","POWER","STATION","DAYS","NORTHERN","WESTERN","SOUTHERN","EASTERN","ALL INDIA TOTAL")
# 
# theColNames<-c("Stn_num","Transport","Stn_name","Capacity","Norm_Stock_Days","Daily_Req_MT","Act_Stock_MT","Act_Stock_Days","SevenDayStock","FourDayStock","Reason")
# theColNames2<-c("Stn_num","Transport","Stn_name","Capacity","Norm_Stock_Days","Daily_Req_MT","Import_Stock_MT","Indig_Stock_MT","Act_Stock_Days","SevenDayStock","FourDayStock","Reason")
# theColNames3<-c("Stn_num","Transport","Stn_name","Capacity","Norm_Stock_Days","Daily_Req_MT","Import_Stock_MT","Indig_Stock_MT","Total_Stock_MT","Act_Stock_Days","SevenDayStock","FourDayStock","Reason")
# 
# length(theColNames)
# length(theColNames2)
# length(theColNames3)
# 
# ###################################################
# ### Import the data files in THREE groups (Mar08-Feb12);  (Mar12-Feb13), and (Mar13-July13) --> ChronMons[1:48], ChronMons[49:60], ChronMons[61:65]
# ###################################################
# # ## Import and compile FIRST set of .csv's (Mar08-Feb12)
# # for(i in 1:length(ChronMons[1:48])){ 
# #   #getData <- function(i) {
# #   print(i)
# #   theFile <- paste("/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/Fuel Supply/csv_data/coal/",ChronMons[i],".csv", sep="")
# #   if (!file.exists(theFile)) next
# #   data<-read.table(file=theFile, sep=",", strip.white=TRUE, blank.lines.skip=TRUE, fill=TRUE, skip=14, header=FALSE, check.names=TRUE, comment="@", col.names=theColNames)
# #   #if (dim(data)[2]>11) data[,12] <- NULL
# #   data <- subset(data, ! is.na(as.numeric(data$Stn_num)) ) #remove all rows that are not associated with a station number
# #   data<-droplevels(data)
# #   data$ChronMons<-ChronMons[i]
# #   data$Date<-as.Date(paste(15,ChronMons[i], sep=""), format="%d%b%y")
# #   
# #   if(i==1) {DF <- data} else {DF <- rbind(DF,data)}
# #   str(data)
# # }
# # str(DF)
# # save(DF, file="DFraw.rsav")
# 
# load("DFraw.rsav")
# 
# ### Visually inspect data
# DF[1:100,]
# DF<-subset(DF, select=c(1:8,13))
# dim(DF)  #40,894
# #DF$Transport<-as.factor(DF$Transport)
# DF$Stn_name<-as.factor(DF$Stn_name)
# DF$Stn_num<-as.integer(DF$Stn_num)
# 
# table(DF$Stn_num) # all nums, but some have few observations...
# 
# levels(DF$Stn_name)
# length(levels(DF$Stn_name)) #186 --> mostly Stn_names, but a few erroneous...
# table(DF$Stn_name) # get rid of erroneous stations....
# 
# 
# # get rid of erroneous stations....
# # If Stn_name can be coerced to a numeric, then is.na=FALSE.
# # If Stn_name cannot be coerced to numeric (e.g. a valid Stn_name), then is.na=TRUE
# DF$Stn_name<-as.character(DF$Stn_name)
# test <- subset(DF, is.na(as.numeric(DF$Stn_name)) ) #remove all rows with invalid Stn_name
# table(test$Stn_name)  # this works well but doesn't get the "" blank Stn_names..
# test2<-subset(test, Stn_name != "")
# table(test2$Stn_name)  # Excellent!
# test2<-droplevels(test2)
# DF<-test2
# 
# # check for NAs --> NONE
# sum(is.na(DF)) 
# look<-which(is.na(DF[,]), arr.ind=TRUE)
# DF[look[,1],]
# 
# # Check for complete.cases..
# test<-DF[complete.cases(DF[,]),]
# if(identical(test,DF)) {print("No Missing data")} else
# {print("Missing data--Check")}
# dim(DF)[1]-dim(test)[1]
# DF<-test # re-assign DF to the complete cases only
# 
# # Double check for NAs...
# sum(is.na(DF)) #0
# 
# #################################################
# #################################################
# ### combine stations with similar names...
# dummy<-strsplit(as.character(DF$Stn_name), split=" ")
# DF$firstname<-laply(dummy, '[[', 1)
# 
# # before using ddply, need to convert columns to as.numeric()
# # convert class to numeric()
# DF$Capacity<-as.integer(DF$Capacity)              
# DF$Norm_Stock_Days<-as.numeric(DF$Norm_Stock_Days) 
# DF$Daily_Req_MT<-as.numeric(DF$Daily_Req_MT)
# DF$Act_Stock_MT<-as.numeric(DF$Act_Stock_MT)
# #DF$Import_Stock_MT<-as.numeric(DF$Import_Stock_MT)   # only used in DF2 and DF3
# #DF$Indig_Stock_MT<-as.numeric(DF$Indig_Stock_MT)     # only used in DF2 and DF3
# #DF$Total_Stock_MT<-as.numeric(DF$Indig_Stock_MT)    # only used in DF2 and DF3--> same as Act_Stock_MT
# DF$Act_Stock_Days<-as.numeric(DF$Act_Stock_Days)
# str(DF)
# 
# ## where are the NAs?
# sum(is.na(DF$Capacity))          #4
# sum(is.na(DF$Norm_Stock_Days))   #4
# sum(is.na(DF$Daily_Req_MT))      #4
# #sum(is.na(DF$Import_Stock_MT)) 
# #sum(is.na(DF$Indig_Stock_MT))    
# sum(is.na(DF$Act_Stock_MT))     #4
# sum(is.na(DF$Act_Stock_Days))   #62
# sum(is.na(DF[,4:9]))  # only 78 total NA's out of 40,644 observations...
# 
# which(is.na(DF), arr.ind=TRUE)
# look<-which(is.na(DF), arr.ind=TRUE)
# head(DF[look[,1],1:9])
# 
# # only 78 total NA's out of 40,644 observations... Omit them
# DF<-na.omit(DF) 
# dim(DF) # 40,582 x  11
# 
# # combine stations with similar names and aggregate from quasi-daily to monthly-mean
# test<-ddply(DF, .(firstname, Stn_num, Transport, Date), numcolwise(mean)) #1310 obs of 10 vars
# head(test); dim(test) # 3051 x 9
# 
# test2<-na.omit(test) # 3051 obs of 9 vars
# head(test2); dim(test2)
# 
# test2[,6:9]<-round(test2[,6:9], digits=2)
# test2$Capacity<-as.integer(test2$Capacity)
# head(test2)
# 
# ## check values....
# #test2$Total_Stock_MT = test2$Import_Stock_MT + test2$Indig_Stock_MT  #only for DF2 and DF3
# #test2$Total_Stock_MT<-round(test2$Total_Stock_MT, digits=2)
# test2$test<-round(test2$Act_Stock_MT/test2$Daily_Req_MT, digits=2) # compute Act_Stock_Days
# 
# test2<-na.omit(test2) # 3050 obs of 9 vars
# head(test2); dim(test2)
# 
# # visually inspect..
# cbind(test2$Act_Stock_Days, test2$test) # Looks good!
# 
# # but double-check anyway..
# # For which rows does calculated and reported Act_Stock_Days match to the nearest tens place (e.g. 24-->20 and 26-->30)
# ok<-which(round(test2$Act_Stock_Days, digits=-1)==round(test2$test, digits=-1))
# keep<-test2[ok,]
# dim(keep); head(keep)  #2925 of 3051 observations are OK.
# keep<-keep[,1:10]     #keep the good data.
# 
# 
# drop<-test2[-ok,]
# head(drop); dim(drop)  #125 rows are NOT OK... fix them!
# 
# if(dim(drop)[1] + dim(keep)[1] == dim(test2)[1]) {print("No Missing data")} else
# {print("Missing data--Check")}
# 
# drop$diff<- drop$Act_Stock_Days - drop$test
# drop2<-drop[order(drop$diff),]
# drop2[,]   # in some cases, Daily_Req_MT not reported, so test=INF
# 
# drop3<-subset(drop2, diff>=5)
# dim(drop3); head(drop3)
# drop4<-subset(drop2, diff<5)  # Data is close enough... add back to keep.
# dim(drop4); head(drop4)
# drop4<-drop4[,1:10]
# 
# # For remaing drop data...which should I beleive?  calculated or reported Act_Stock_Days ??
# # cross-reference with original PDFs... *Calcuated* is more accurate.  Assign: drop2$Act_Stock_Days <- drop2$test
# drop3<-drop3[order(drop3$Date),]
# drop3$Act_Stock_Days <-drop3$test
# identical(drop3$Act_Stock_Days, drop3$test)
# drop3<-drop3[,1:10]
# 
# # re-combine with full dataset...
# if(dim(drop3)[1] + dim(drop4)[1] + dim(keep)[1] == dim(test2)[1]) {print("No Missing data")} else
# {print("Missing data--Check")}
# 
# MonSum<-rbind(keep, drop3, drop4)
# 
# MonSum<-MonSum[order(MonSum$firstname),]
# # MonSum looks good!
# 
# # double check for NAs --> NONE.
# which(is.na(MonSum), arr.ind=TRUE)
# look<-which(is.na(MonSum), arr.ind=TRUE)
# head(MonSum[look[,1],1:9])
# 
# # Check for complete.cases..
# test<-MonSum[complete.cases(MonSum[,]),]
# if(identical(test,MonSum)) {print("No Missing data")} else
# {print("Missing data--Check")}
# 
# table(MonSum$firstname)  ## --> Still need to combine CHANDRAPUR entries...
# test1<-subset(MonSum, firstname=="CHANDRAPURA")
# test2<-subset(MonSum, firstname=="CHANDRAPURA(DVC)")
# test3<-rbind(test1, test2)
# 
# test4<-subset(MonSum, firstname=="CHANDRAPUR(MAHA"); head(test4)
# test5<-subset(MonSum, firstname=="CHANDRAPUR(MAHAR"); head(test5)
# test6<-subset(MonSum, firstname=="CHANDRAPUR(MAHARA"); head(test6)
# test7<-subset(MonSum, firstname=="CHANDRAPUR(MAHARAS"); head(test7)
# test8<-subset(MonSum, firstname=="CHANDRAPUR(MAHARASHT"); head(test8)
# test9<-rbind(test4, test5, test6, test7, test8)
# 
# # DROP FROM THE DATA...
# MonSum<-subset(MonSum, firstname !="CHANDRAPURA")
# MonSum<-subset(MonSum, firstname !="CHANDRAPURA(DVC)")
# MonSum<-subset(MonSum, firstname !="CHANDRAPUR(MAHA")
# MonSum<-subset(MonSum, firstname !="CHANDRAPUR(MAHAR")
# MonSum<-subset(MonSum, firstname !="CHANDRAPUR(MAHARAS")
# MonSum<-subset(MonSum, firstname !="CHANDRAPUR(MAHARASHT")
# 
# 
# # Add back in with corrected names..
# test3$firstname<-"CHANDRAPURA"
# test9$firstname<-"CHANDRAPUR (MAHARASHTA)"
# 
# MonSum<-rbind(MonSum, test3, test9)
# MonSum<-MonSum[order(MonSum$firstname),]
# dim(MonSum)  #no data lost in split-combine.
# 
# # #################################################
# # #################################################
# # ### quality control: check number of observations at each station.
# # ## Discard stations with less than 50 observations (on avg., ~1 per month)
# # nstns<-length(levels(DF$Stn_name))
# # nmons<-length(seq(range(DF$Date)[1],range(DF$Date)[2], by="months")); nmons
# # max(table(DF$Stn_name)) # max number of observations for one station=845
# # 
# # DF$Stn_name<-as.factor(DF$Stn_name)  # convert Stn_name back to factor...
# # stns<-levels(DF$Stn_name)
# # nstns<-length(stns)
# # nstns #178
# # 
# # stns<-as.data.frame(stns)
# # stns$keep<-rep(0,nstns)  # empty vector to be populated in for loop...
# # for (i in 1:nstns){
# #   if (table(DF$Stn_name)[i]<50) {stns$keep[i]<-"No"} else {stns$keep[i]<-"Yes"}
# # }
# # 
# # keep<-subset(stns, keep=="Yes") #142 of 178 stations
# # 
# # #now subset "DF" to keep only the observations associated with a station that we want to keep
# # keep.data<-DF[DF$Stn_name %in% keep$stns,] 
# # test<-subset(DF, Stn_name %in% keep$stns)
# # identical(keep.data, test) #true
# # DF<-keep.data
# # DF<-droplevels(DF)
# # table(DF$Stn_name)
# 
# # data looks good... 
# # Now do manipulations/computations with compiled data... 
# MonSum<-MonSum[,1:9]
# 
# n<-dim(MonSum)[1]
# for (j in 1:n){
#   if(MonSum$Act_Stock_Days[j]<7) {MonSum$Critical[j]=1} else 
#     (MonSum$Critical[j]=0)
# }
# save(MonSum, file="MonSum.rsav")
# 
# 
# str(MonSum)  #3,050 observations of 10 variables
# Coal1<-MonSum
# save(Coal1, file="CoalSupply_Mar08-Feb12.rsav")
# save(Coal1, file="Coal1.rsav")
# 
# ############## KEY FIGURE ###############
# p<-ggplot(MonSum, aes(x=Date, y=Act_Stock_Days, group=Date)) 
# p + geom_boxplot() + scale_y_continuous(name='Coal Stock [days]') + scale_x_date(breaks=date_breaks("3 months"), labels=date_format("%b-%y")) + labs(title="Coal Stock for all-India TPS (Mar08-Feb12)") + geom_abline(intercept=7, slope=0, colour="red")
# 
# # geom_smooth(MonSum, aes(group=Yr), method="lm")
# # abline(a=7,b=0, col="red") #doesn't work w. ggplot2()
# ############## KEY FIGURE ###############
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ###################################################
# ### Repeat for DF2
# ###################################################
# ### Import the data files in THREE groups (Mar08-Feb12);  (Mar12-Feb13), and (Mar13-July13) --> ChronMons[1:48], ChronMons[49:60], ChronMons[61:65]
# ### Import and compile SECOND set of .csv's (Mar12-Feb13)
# for(i in 49:60){ 
#   #getData <- function(i) {
#   print(i)
#   theFile <- paste("/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/Fuel Supply/csv_data/coal/",ChronMons[i],".csv", sep="")
#   if (!file.exists(theFile)) next
#   data<-read.table(file=theFile, sep=",", strip.white=TRUE, blank.lines.skip=TRUE, fill=TRUE, skip=14, header=FALSE, check.names=TRUE, comment="@", col.names=theColNames2)
#   data <- subset(data, ! is.na(as.numeric(data$Stn_num)) ) #remove all rows that are not associated with a station number
#   data<-droplevels(data)
#   data$ChronMons<-ChronMons[i]
#   data$Date<-as.Date(paste(15,ChronMons[i], sep=""), format="%d%b%y")
#   
#   if(i==49) {DF2 <- data} else {DF2 <- rbind(DF2,data)}
#   str(data)
# }
# str(DF2)
# save(DF2, file="DF2raw.rsav")
# 
# ### Visually inspect data
# DF2[1:100,]
# DF2<-subset(DF2, select=c(1:9,14))
# dim(DF2)  #9,381
# DF2$Transport<-as.factor(DF2$Transport)
# DF2$Stn_num<-as.integer(DF2$Stn_num)
# DF2$Stn_name<-as.factor(DF2$Stn_name)
# 
# table(DF2$Stn_num) # all nums, but some have few observations...
# 
# levels(DF2$Stn_name)
# length(levels(DF2$Stn_name)) #186 --> mostly Stn_names, but a few erroneous...
# table(DF2$Stn_name) ## get rid of erroneous stations....
# DF2$Stn_name<-as.character(DF2$Stn_name)
# 
# 
# ### remove all rows that are not associated with a valid stn_name
# # If Stn_name can be coerced to a numeric, then is.na=FALSE.
# # If Stn_name cannot be coerced to numeric (e.g. a valid Stn_name), then is.na=TRUE
# test <- subset(DF2, is.na(as.numeric(DF2$Stn_name)) ) #remove rows not associated with a valid stn_name
# table(test$Stn_name)  # this works well but doesn't get the "" blank Stn_names..
# test2<-subset(test, Stn_name != "")
# table(test2$Stn_name)  # Excellent!  But can still combine mulitple records for a single station....
# test2<-droplevels(test2)
# DF2<-test2
# 
# ### combine stations with similar names...
# dummy<-strsplit(as.character(DF2$Stn_name), split=" ")
# DF2$firstname<-laply(dummy, '[[', 1)
# 
# # before using ddply, need to convert columns to as.numeric()
# # convert class to numeric()
# DF2$Capacity<-as.integer(DF2$Capacity)              
# DF2$Norm_Stock_Days<-as.numeric(DF2$Norm_Stock_Days) 
# DF2$Daily_Req_MT<-as.numeric(DF2$Daily_Req_MT)      
# DF2$Import_Stock_MT<-as.numeric(DF2$Import_Stock_MT) #NAs introduced by coercion
# DF2$Indig_Stock_MT<-as.numeric(DF2$Indig_Stock_MT)  #NAs introduced by coercion
# DF2$Act_Stock_Days<-as.numeric(DF2$Act_Stock_Days)  #NAs introduced by coercion
# 
# ## where are the NAs?
# sum(is.na(DF2$Capacity))          #23
# sum(is.na(DF2$Norm_Stock_Days))   #23
# sum(is.na(DF2$Daily_Req_MT))      #23
# sum(is.na(DF2$Import_Stock_MT))   #47
# sum(is.na(DF2$Indig_Stock_MT))    #133
# sum(is.na(DF2$Act_Stock_Days))    #106
# sum(is.na(DF2[,4:9]))  # 364 NA's out of 9,291 observations...
# 
# which(is.na(DF2), arr.ind=TRUE)
# look<-which(is.na(DF2), arr.ind=TRUE)
# head(DF2[look[,1],1:9])
# 
# DF2<-na.omit(DF2) # actually retain 11,847 b.c some NA's in same row....
# dim(DF2) # 9,076 x  11
# 
# # combine stations with similar names and aggregate from quasi-daily to monthly-mean
# test<-ddply(DF2, .(firstname, Stn_num, Transport, Date), numcolwise(mean)) #1310 obs of 10 vars
# head(test)
# test2<-na.omit(test) # 1310 obs of 10 vars
# 
# test2[,7:10]<-round(test2[,7:10], digits=2)
# test2$Capacity<-as.integer(test2$Capacity)
# head(test2)
# #test2<-test2[,-11]  # drop the "keep" variable
# 
# # check values....
# test2$Act_Stock_MT = test2$Import_Stock_MT + test2$Indig_Stock_MT
# test2$Act_Stock_MT<-round(test2$Act_Stock_MT, digits=2)
# test2$test<-round(test2$Act_Stock_MT/test2$Daily_Req_MT, digits=2)
# 
# # visually inspect..
# cbind(test2$Act_Stock_Days, test2$test) # lots of problem data... calculated Act_Stock_MT looks more reliabe....
# 
# # For which rows does calculated and reported Act_Stock_Days match to the nearest tens place (e.g. 24-->20 and 26-->30)
# ok<-which(round(test2$Act_Stock_Days, digits=-1)==round(test2$test, digits=-1))
# keep<-test2[ok,]
# keep<-keep[,1:11]
# head(keep)
# dim(keep) #937 of 1310 rows are OK
# 
# drop<-test2[-ok,]
# head(drop)
# dim(drop)  #373 rows are NOT OK... fix them!
# 
# if(dim(drop)[1] + dim(keep)[1] == dim(test2)[1]) {print("No Missing data")} else
#   {print("Missing data--Check")}
# 
# drop$diff<- drop$Act_Stock_Days - drop$test
# drop2<-drop[order(drop$diff),]
# drop2[1:100,]
# drop2<-drop[order(drop$firstname),]
# drop2[1:100,]
# 
# # which should we beleive?  calculated or reported Act_Stock_Days ??
# # cross-reference with original PDFs... Calcuated is more accurate.  Assign: drop2$Act_Stock_Days <- drop2$test
# drop2$Act_Stock_Days <-drop2$test
# drop2<-drop2[,1:11]
# 
# # re-combine with full dataset...
# if(dim(drop2)[1] + dim(keep)[1] == dim(test2)[1]) {print("No Missing data")} else
# {print("Missing data--Check")}
# 
# MonSum2<-as.data.frame(rbind(keep, drop2))
# MonSum2<-MonSum2[order(MonSum2$firstname),]
# # MonSum2 looks good!
# 
# # double check for NAs --> NONE.
# which(is.na(MonSum2), arr.ind=TRUE)
# look<-which(is.na(MonSum2), arr.ind=TRUE)
# head(MonSum2[look[,1],1:9])
# 
# # Check for complete.cases..
# test<-MonSum2[complete.cases(MonSum2[,]),]
# if(identical(test,MonSum2)) {print("No Missing data")} else
# {print("Missing data--Check")}
# 
# table(MonSum2$firstname)  ## --> Still need to combine CHANDRAPUR entries
# test1<-subset(MonSum2, firstname=="CHANDRAPURA")
# test2<-subset(MonSum2, firstname=="CHANDRAPURA(DVC)")
# test3<-rbind(test1, test2)
# 
# test4<-subset(MonSum2, firstname=="CHANDRAPUR(MAHA"); head(test4)
# test5<-subset(MonSum2, firstname=="CHANDRAPUR(MAHAR"); head(test5)
# test6<-subset(MonSum2, firstname=="CHANDRAPUR(MAHARA"); head(test6)
# test7<-subset(MonSum2, firstname=="CHANDRAPUR(MAHARAS"); head(test7)
# test8<-subset(MonSum2, firstname=="CHANDRAPUR(MAHARASHT"); head(test8)
# test9<-rbind(test4, test5, test6, test7, test8)
# 
# # DROP FROM THE DATA...
# MonSum2<-subset(MonSum2, firstname !="CHANDRAPURA")
# MonSum2<-subset(MonSum2, firstname !="CHANDRAPURA(DVC)")
# MonSum2<-subset(MonSum2, firstname !="CHANDRAPUR(MAHA")
# MonSum2<-subset(MonSum2, firstname !="CHANDRAPUR(MAHAR")
# MonSum2<-subset(MonSum2, firstname !="CHANDRAPUR(MAHARAS")
# MonSum2<-subset(MonSum2, firstname !="CHANDRAPUR(MAHARASHT")
# 
# # Add back in with corrected names..
# test3$firstname<-"CHANDRAPURA"
# test9$firstname<-"CHANDRAPUR (MAHARASHTA)"
# 
# MonSum2<-rbind(MonSum2, test3, test9)
# MonSum2<-MonSum2[order(MonSum2$firstname),]
# 
# look<-which(is.na(MonSum2[,]), arr.ind=TRUE)
# MonSum2[look[,1],]           # No NAs
# MonSum2<-na.omit(MonSum2)
# save(MonSum2, file="MonSum2.rsav")
# 
# # Now do manipulations/computations with compiled data...
# n<-dim(MonSum2)[1]
# for (j in 1:n){
#   if(MonSum2$Act_Stock_Days[j]<7) {MonSum2$Critical[j]=1} else 
#     (MonSum2$Critical[j]=0)
# }
# save(MonSum2, file="MonSum2.rsav")
# 
# ## check for NAs
# which(is.na(MonSum2), arr.ind=TRUE)
# look<-which(is.na(MonSum2), arr.ind=TRUE)
# MonSum2[look[,1],]
# 
# str(MonSum2)  #1,311 observations of 12 variables
# Coal2<-MonSum2
# save(Coal2, file="CoalSupply_Mar12-Feb13.rsav")
# save(Coal2, file="Coal2.rsav")
# 
# load("Coal2.rsav")
# ############## KEY FIGURE ###############
# p<-ggplot(MonSum2, aes(x=Date, y=Act_Stock_Days, group=Date)) 
# 
# p + geom_boxplot() + scale_y_continuous(name='Coal Stock [days]') + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%y")) + labs(title="Coal Stock for all-India TPS (Mar 2012 - Feb 2013)") + geom_abline(intercept=7, slope=0, colour="red")
# 
# # geom_smooth(MonSum, aes(group=Yr), method="lm")
# # abline(a=7,b=0, col="red") #doesn't work w. ggplot2()
# ############## KEY FIGURE ###############


# ###################################################
# ### Repeat for DF3
# ###################################################
# ### Import the data files in THREE groups (Mar08-Feb12);  (Mar12-Feb13), and (Mar13-July13) --> ChronMons[1:48], ChronMons[49:60], ChronMons[61:65]
# ### Import and compile SECOND set of .csv's (Mar12-Jul13)
# # for(i in 61:65){ 
# #   #getData <- function(i) {
# #   print(i)
# #   theFile <- paste("/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/Fuel Supply/csv_data/coal/",ChronMons[i],".csv", sep="")
# #   if (!file.exists(theFile)) next
# #   data<-read.table(file=theFile, sep=",", strip.white=TRUE, blank.lines.skip=TRUE, fill=TRUE, skip=14, header=FALSE, check.names=TRUE, comment="@", col.names=theColNames3)
# #   data <- subset(data, ! is.na(as.numeric(data$Stn_num)) ) #remove all rows that are not associated with a station number
# #   data<-droplevels(data)
# #   data$ChronMons<-ChronMons[i]
# #   data$Date<-as.Date(paste(15,ChronMons[i], sep=""), format="%d%b%y")
# #   
# #   if(i==61) {DF3 <- data} else {DF3 <- rbind(DF3,data)}
# #   str(data)
# # }
# # str(DF3)
# # save(DF3, file="DF3raw.rsav")
# 
# load("DF3raw.rsav")
# ### Visually inspect data
# DF3[1:100,]
# DF3<-subset(DF3, select=c(1:10,15))
# dim(DF3)       #5,101 x 11
# DF3$Transport<-as.factor(DF3$Transport)
# DF3$Stn_num<-as.integer(DF3$Stn_num)
# DF3$Stn_name<-as.factor(DF3$Stn_name)
# 
# table(DF3$Stn_num) # all nums, only Stn_num[100] looks questionable...
# 
# levels(DF3$Stn_name)
# length(levels(DF3$Stn_name)) #186 --> mostly Stn_names, but a few erroneous...
# table(DF3$Stn_name) ## get rid of erroneous stations....
# DF3$Stn_name<-as.character(DF3$Stn_name)
# 
# 
# ### remove all rows that are not associated with a valid stn_name
# # If Stn_name can be coerced to a numeric, then is.na=FALSE.
# # If Stn_name cannot be coerced to numeric (e.g. a valid Stn_name), then is.na=TRUE
# test <- subset(DF3, is.na(as.numeric(DF3$Stn_name)) ) #remove rows not associated with a valid stn_name
# table(test$Stn_name)  # this works well but doesn't get the "" blank Stn_names..
# test2<-subset(test, Stn_name != "")
# table(test2$Stn_name)  # Excellent!  But can still combine mulitple records for a single station....
# test2<-droplevels(test2)
# DF3<-test2
# dim(DF3)  #4,808 x 11
# 
# ### combine stations with similar names...
# dummy<-strsplit(as.character(DF3$Stn_name), split=" ")
# DF3$firstname<-laply(dummy, '[[', 1)
# 
# ## check for NAs
# which(is.na(DF3))
# look<-which(is.na(DF3))
# DF3[look,1:10]
# head(DF3[look,1:10])  # None
# 
# # before using ddply, need to convert columns to as.numeric()
# # convert class to numeric()
# DF3$Capacity<-as.integer(DF3$Capacity)              #NAs introduced by coercion
# DF3$Norm_Stock_Days<-as.numeric(DF3$Norm_Stock_Days) #NAs introduced by coercion
# DF3$Daily_Req_MT<-as.numeric(DF3$Daily_Req_MT)      #NAs introduced by coercion
# DF3$Import_Stock_MT<-as.numeric(DF3$Import_Stock_MT) #NAs introduced by coercion
# DF3$Indig_Stock_MT<-as.numeric(DF3$Indig_Stock_MT)  #NAs introduced by coercion
# DF3$Total_Stock_MT<-as.numeric(DF3$Total_Stock_MT)  #NAs introduced by coercion
# DF3$Act_Stock_Days<-as.numeric(DF3$Act_Stock_Days)  #NAs introduced by coercion
# 
# ## where are the NAs?
# sum(is.na(DF3$Capacity))          #31
# sum(is.na(DF3$Norm_Stock_Days))   #31
# sum(is.na(DF3$Daily_Req_MT))      #46
# sum(is.na(DF3$Import_Stock_MT))   #1074
# sum(is.na(DF3$Indig_Stock_MT))    #229
# sum(is.na(DF3$Act_Stock_Days))    #1301
# sum(is.na(DF3[,4:9]))  # 1592 NA's out of 4808 observations...
# 
# which(is.na(DF3$Import_Stock_MT))
# look<-which(is.na(DF3$Import_Stock_MT))
# DF3[look[1:200],1:10]
# head(DF3[look,1:10])  # messy! omit.na
# 
# DF3<-na.omit(DF3) #
# dim(DF3) # 3505 x  11
# 
# # combine stations with similar names and aggregate from quasi-daily to monthly-mean
# test<-ddply(DF3, .(firstname, Stn_num, Transport, Date), numcolwise(mean)) #1310 obs of 10 vars
# head(test)
# dim(test) #583 obs of 11 vars
# test2<-na.omit(test) # 583 obs of 11 vars
# dim(test2) #583 obs of 11 vars
# 
# test2[,6:11]<-round(test2[,6:11], digits=2)
# test2$Capacity<-as.integer(test2$Capacity)
# head(test2)
# #test2<-test2[,-11]  # drop the "keep" variable
# 
# # check values....
# test2$Act_Stock_MT = test2$Import_Stock_MT + test2$Indig_Stock_MT
# test2$Act_Stock_MT<-round(test2$Act_Stock_MT, digits=2)
# test2$test<-round(test2$Act_Stock_MT/test2$Daily_Req_MT, digits=2)
# 
# # visually inspect..
# cbind(test2$Act_Stock_Days, test2$test) # looks good!
# 
# # For which rows does calculated and reported Act_Stock_Days match to the nearest tens place (e.g. 24-->20 and 26-->30)
# ok<-which(round(test2$Act_Stock_Days, digits=-1)==round(test2$test, digits=-1))
# keep<-test2[ok,]
# keep<-keep[,1:11]
# head(keep)
# dim(keep) #571 of 583 rows are OK
# 
# drop<-test2[-ok,]
# head(drop)
# dim(drop)  #12 rows.  VISUAL INSPECTION REVEALS THESE ARE OK.  
# drop<-drop[,1:11]
# 
# # if(dim(drop)[1] + dim(keep)[1] == dim(test2)[1]) {print("No Missing data")} else
# # {print("Missing data--Check")}
# # 
# # drop$diff<- drop$Act_Stock_Days - drop$test
# # drop2<-drop[order(drop$diff),]
# # drop2[1:100,]
# # drop2<-drop[order(drop$firstname),]
# # drop2[1:100,]
# # 
# # # which should we beleive?  calculated or reported Act_Stock_Days ??
# # # cross-reference with original PDFs... Calcuated is more accurate.  Assign: drop2$Act_Stock_Days <- drop2$test
# # drop2$Act_Stock_Days <-drop2$test
# # drop2<-drop2[,1:11]
# 
# # # re-combine with full dataset...
# # if(dim(drop2)[1] + dim(keep)[1] == dim(test2)[1]) {print("No Missing data")} else
# # {print("Missing data--Check")}
# 
# MonSum3<-as.data.frame(rbind(keep, drop))
# MonSum3<-MonSum3[order(MonSum3$firstname),]
# # MonSum3 looks good!
# 
# # double check for NAs --> NONE.
# which(is.na(MonSum3), arr.ind=TRUE)
# look<-which(is.na(MonSum3), arr.ind=TRUE)
# head(MonSum3[look[,1],1:9])
# 
# # Check for complete.cases..
# test<-MonSum3[complete.cases(MonSum3[,]),]
# if(identical(test,MonSum3)) {print("No Missing data")} else
# {print("Missing data--Check")}
# 
# table(MonSum3$firstname)  ## --> Still need to combine CHANDRAPUR entries
# test1<-subset(MonSum3, firstname=="CHANDRAPURA")
# test2<-subset(MonSum3, firstname=="CHANDRAPURA(DV")
# test3<-subset(MonSum3, firstname=="CHANDRAPURA(DVC")
# test4<-subset(MonSum3, firstname=="CHANDRAPURA(DVC)")
# test5<-rbind(test1, test2, test2, test3, test4)
# test5
# 
# test6<-subset(MonSum3, firstname=="CHANDRAPUR(MAH")
# test7<-subset(MonSum3, firstname=="CHANDRAPUR(MAHA")
# test8<-subset(MonSum3, firstname=="CHANDRAPUR(MAHAR")
# test9<-subset(MonSum3, firstname=="CHANDRAPUR(MAHARA")
# test10<-subset(MonSum3, firstname=="CHANDRAPUR(MAHARAS")
# test11<-subset(MonSum3, firstname=="CHANDRAPUR(MAHARASH")
# test12<-subset(MonSum3, firstname=="CHANDRAPUR(MAHARASHT")
# test13<-subset(MonSum3, firstname=="CHANDRAPUR(MAHARASHTRA")
# test14<-subset(MonSum3, firstname=="CHANDRAPUR(MAHARASHTRA)")
# test15<-rbind(test6, test7, test8, test9, test10, test11, test12, test13, test14)
# test15
# 
# # DROP FROM THE DATA...
# MonSum3<-subset(MonSum3, firstname !="CHANDRAPURA")
# MonSum3<-subset(MonSum3, firstname !="CHANDRAPURA(DV")
# MonSum3<-subset(MonSum3, firstname !="CHANDRAPURA(DVC")
# MonSum3<-subset(MonSum3, firstname !="CHANDRAPURA(DVC)")
# 
# MonSum3<-subset(MonSum3, firstname !="CHANDRAPUR(MAH")
# MonSum3<-subset(MonSum3, firstname !="CHANDRAPUR(MAHA")
# MonSum3<-subset(MonSum3, firstname !="CHANDRAPUR(MAHAR")
# MonSum3<-subset(MonSum3, firstname !="CHANDRAPUR(MAHARA")
# MonSum3<-subset(MonSum3, firstname !="CHANDRAPUR(MAHARAS")
# MonSum3<-subset(MonSum3, firstname !="CHANDRAPUR(MAHARASHT")
# MonSum3<-subset(MonSum3, firstname !="CHANDRAPUR(MAHARASHTRA")
# MonSum3<-subset(MonSum3, firstname !="CHANDRAPUR(MAHARASHTRA)")
# 
# 
# # Add back in with corrected names..
# test5$firstname<-"CHANDRAPURA"
# test15$firstname<-"CHANDRAPUR (MAHARASHTA)"
# 
# MonSum3<-rbind(MonSum3, test5, test15)
# MonSum3<-MonSum3[order(MonSum3$firstname),]
# 
# look<-which(is.na(MonSum3[,]), arr.ind=TRUE)
# MonSum3[look[,1],]           # No NAs
# MonSum3<-na.omit(MonSum3)
# save(MonSum3, file="MonSum3.rsav")
# 
# # Now do manipulations/computations with compiled data...
# n<-dim(MonSum3)[1]
# for (j in 1:n){
#   if(MonSum3$Act_Stock_Days[j]<7) {MonSum3$Critical[j]=1} else 
#     (MonSum3$Critical[j]=0)
# }
# save(MonSum3, file="MonSum3.rsav")
# 
# ## check for NAs
# which(is.na(MonSum3), arr.ind=TRUE)
# look<-which(is.na(MonSum3), arr.ind=TRUE)
# MonSum3[look[,1],]
# 
# str(MonSum3)  #1,311 observations of 12 variables
# Coal3<-MonSum3
# save(Coal3, file="CoalSupply_Mar13-Jul13.rsav")
# save(Coal3, file="Coal3.rsav")
# 
# load("Coal3.rsav")
# ############## KEY FIGURE ###############
# p<-ggplot(MonSum3, aes(x=Date, y=Act_Stock_Days, group=Date)) 
# 
# p + geom_boxplot() + scale_y_continuous(name='Coal Stock [days]') + scale_x_date(breaks=date_breaks("1 months"), labels=date_format("%b-%y")) + labs(title="Coal Stock for all-India TPS (Mar 2013 - July 2013)") + geom_abline(intercept=7, slope=0, colour="red")
# 
# # geom_smooth(MonSum, aes(group=Yr), method="lm")
# # abline(a=7,b=0, col="red") #doesn't work w. ggplot2()
############## KEY FIGURE ###############


######################################
#### COMPILE Coal1, Coal2 and Coal3
#####################################
load("Coal1.rsav")
load("Coal2.rsav")
load("Coal3.rsav")

test2<-subset(MonSum2, select=c(1:7, 11, 10, 12))
test3<-subset(MonSum3, select=c(1:7, 10, 11, 12))
names<-names(test2)
names(test3)<-names

Coal<-rbind(MonSum, test2, test3)
dim(Coal) #4947 observations of 10 variables

save(Coal, file="CoalSupply_Mar08-Jul13.rsav")
save(Coal, file="Coal_FINAL.rsav")

############## KEY FIGURE ###############
load("Coal_FINAL.rsav")
p<-ggplot(Coal, aes(x=Date, y=Act_Stock_Days, group=Date)) 

p1<-ggplot(Coal, aes(x=Date, y=Daily_Req_MT, group=Date)) 


p + geom_boxplot() + scale_y_continuous(name='Coal Stock [days]', limit=c(0,30)) + scale_x_date(breaks=date_breaks("4 months"), labels=date_format("%b-%y")) + labs(title="All-India Coal Stock (Mar 2008 - July 2013)") + geom_abline(intercept=7, slope=0, colour="red")

p1 + geom_boxplot() + scale_y_continuous(name='Coal Stock [days]') + scale_x_date(breaks=date_breaks("4 months"), labels=date_format("%b-%y")) + labs(title="Daily Coal Requirement for All-India TPS (Mar 2008 - July 2013)")


# geom_smooth(Coal, aes(group=Yr), method="lm")
# abline(a=7,b=0, col="red") #doesn't work w. ggplot2()
############## KEY FIGURE ###############




########################################
### after combining MonSum1 MonSum2 and MonSum3
########################################

# # Total number of TPS days with critical supply
# total.m.sum<-aggregate(Critical ~ Date, data=MonSum2, FUN=sum)
# plot(total.m.sum$Date, total.m.sum$Critical)
# ggplot(total.m.sum, aes(x=Date, y=Critical)) + geom_line() + 
#   scale_y_continuous(name='TPS days at Critical Supply') + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%y")) + labs(title="Coal Shortages") 
# 
# #add a year attribute just for fun
# dummy<-strsplit(as.character(MonSum2$Date), split="-")
# MonSum2$Yr<-laply(dummy, '[[', 1)

##########################################
### now subset to CGS in supply chain...
##########################################
# subset(m.sum, m.sum$Stn_name %in% CGSstns)
# #doesn't well b/c not exact matches...
# 
# # use fuzzy logic instead with agrep
# m.sum$CGS_name<-NA  #create an attribute column for matching CGS name (if applicable)
# n<-dim(m.sum)[1]
# for(i in 1:n){
#   index<-agrep(pattern=m.sum$Stn_name[i], x=CGSstns, ignore.case=TRUE)[1]
#   m.sum$CGS_name[i]<-CGSstns[index]
# }
# test<-na.omit(m.sum)
# dim(m.sum)
# dim(test)
# test<-subset(test, Stn_name!="OBRA")
# test<-subset(test, Stn_name!="KOTA")
# test<-subset(test, Stn_name!="TANDA")
# test<-subset(test, Stn_name!="UKAI")
# test<-droplevels(test)
# table(test$Stn_name)
# table(test$CGS_name)
# 
# ## cbind this to IEX MonSum2 to get monthly fuel supply shortages at stations
# ## will want to use merge such that zeros are added for other stations....
# test$CGS_name<-as.factor(test$CGS_name)
# length(levels(test$CGS_name)) # 6






###################################################
#### Natural Gas fuel supply
##################################################
setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")
library(plyr)
library(reshape2)
library(ggplot2)
library(scales)

#options(error=utils::recover)
options(stringsAsFactors=FALSE)

gas<-read.csv("/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/Fuel Supply/gas_all_in_one.csv", header=TRUE)
save(gas, file="gasraw.rsav")

## Notes: 
## 1. gas in units of MMSCMD (million standard cubic feet per day) (as reported)
## 2. Gen in units of MU (GWh).                   (as reported)
## 3. Gas requirement is at 90% PLF.              (as reported)
## 4. Naptha and HSD use in KL (thousand liters)  (as reported)
## 5. gas.supplied.consumed / gas.requiremnet = effective PAF due to fuel constraints (elliot)

## Visually inspect data...
table(gas$name)  # lots of misspellings and imprecise names --> clean up!

## Combine units with same first name (e.g. RITHALA CCPP, RITHALA CCPP I, RITHALA CCPP ***, ETC..)
gas$name<-as.character(gas$name)
dummy<-strsplit(gas$name, split=" ")
gas$firstname<-laply(dummy, '[[', 1)
gas$firstname<-as.factor(gas$firstname)
# save full names for reference
fullname<-gas$name

## Aggregate mulitple units at same plant...
# stationwise<-ddply(gas, .(year, month, firstname, located.in.state), summarize, capacity=sum(capacity), gen=sum(gen), gas.requirement=sum(gas.requirement), gas.allotted=sum(gas.allotted), gas.consumed=sum(gas.supplied.consumed), naptha=sum(naptha), HSD=sum(HSD))

stationwise<-ddply(gas, .(year, month, firstname, located.in.state), numcolwise(sum))

## Create Date attribute
stationwise$DATE<-as.Date(paste(stationwise$year,stationwise$month,"15", sep="-"), format="%Y-%b-%d")
range(stationwise$DATE)

table(stationwise$firstname) # get rid of erroneous stations....


# get rid of erroneous stations....
# If firstname can be coerced to a numeric, then is.na=FALSE.
# If firstname cannot be coerced to numeric (e.g. a valid firstname), then is.na=TRUE
stationwise$firstname<-as.character(stationwise$firstname)
test <- subset(stationwise, is.na(as.numeric(stationwise$firstname)) ) #remove all rows with invalid firstname
table(test$firstname)  # this works well but doesn't get the "" blank firstnames..
test2<-subset(test, firstname != "")
table(test2$firstname)  # Excellent!
test2<-droplevels(test2)
stationwise<-test2

# check for NAs --> NONE
sum(is.na(stationwise)) 
look<-which(is.na(stationwise[,]), arr.ind=TRUE)
stationwise[look[,1],]

# Check for complete.cases..
test<-stationwise[complete.cases(stationwise[,]),]
if(identical(test,stationwise)) {print("No Missing data")} else
{print("Missing data--Check")}
dim(stationwise)[1]-dim(test)[1]
# stationwise<-test # re-assign stationwise to the complete cases only

# Double check for NAs...
sum(is.na(stationwise)) #0

## REMOVING STATIONS WITH FEW OBS NOT RELEVANT HERE... MOST LOOK GOOD!
# ## quality control: check number of observations at each weather station.
# ## Discard stations with less than 90% of the monthly data
# stationwise$firstname<-as.factor(stationwise$firstname)
# nstns<-length(levels(stationwise$firstname))
# stns<-levels(stationwise$firstname)
# stns<-as.data.frame(stns)
# stns$keep<-rep(0,nstns)  # empty vector to be populated in for loop...
# for (i in 1:nstns){
#   if (table(stationwise$firstname)[i]<23) {stns$keep[i]<-"No"} else {stns$keep[i]<-"Yes"}
# }
# 
# keep<-subset(stns, keep=="Yes")
# 
# #now subset "stationwise" to keep only the observations associated with a station that we want to keep
# keep.data<-stationwise[stationwise$firstname %in% keep$stns,] 
# test<-subset(stationwise, firstname %in% keep$stns)
# identical(keep.data, test) #true
# stationwise<-keep.data
# stationwise<-droplevels(stationwise)
# table(stationwise$firstname)
####

### compute effective plant availability factor due to fuel supply constraints
for(i in 1:dim(stationwise)[1]){
  if(stationwise$gas.requirement[i]>0){
    stationwise$effPAF[i]<-round(stationwise$gas.supplied.consumed[i]/stationwise$gas.requirement[i], digits=2)
  }
  if(stationwise$gas.requirement[i]<=0){
    stationwise$effPAF[i]=0
  }
}

head(stationwise)

# check for NAs --> NONE
sum(is.na(stationwise)) 
look<-which(is.na(stationwise[,]), arr.ind=TRUE)
stationwise[look[,1],]

# Check for complete.cases..
test<-stationwise[complete.cases(stationwise[,]),]
if(identical(test,stationwise)) {print("No Missing data")} else
{print("Missing data--Check")}
dim(stationwise)[1]-dim(test)[1]
# stationwise<-test # assign stationwise to the complete cases only
gas<-stationwise
save(gas, file="gas.rsav")

############## KEY FIGURE ###############
p<-ggplot(stationwise, aes(x=DATE, y=effPAF, group=DATE)) + geom_boxplot()
p + scale_y_continuous(name='effective PAF', limit=c(0,1.5)) + scale_x_date(name="time", breaks = date_breaks(width="3 months"), labels=date_format("%b-%Y")) + labs(title="Fuel supply constraints to gas power plants\nAll-India (April 2010-May 2012)") + theme(axis.title = element_text(size = rel(1.3)),axis.text = element_text(size = rel(1.3)), plot.title = element_text(size = rel(1.4))) + geom_smooth(aes(group=year), method="lm")
# if want a singly linear trend fit, use group=1
# + geom_smooth(aes(group=1), method="lm")

# # can also try built-in plot functions
# boxplot(effPAF ~ DATE, data=stationwise, ylim=c(0,1.5), main="Fuel supply constraints to gas power plants\nAll-India (Apr 2010-May 2012)", xlab="time", ylab="effective PAF")
# 
# dates<-levels(as.factor(stationwise$DATE))
# dates<-as.Date(dates, format="%Y-%m-%d")
# dates<-format(dates, "%b-%y")
# axis(1, at = dates, labels=dates)
############## KEY FIGURE ###############






###################################################
### Repeat for hydro...
###################################################
setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")
load("hydroraw.rsav")

#options(error=utils::recover)
options(stringsAsFactors=FALSE)

library(plyr)
library(reshape2)
library(ggplot2)
library(scales)

# files<-list.files("/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/Fuel Supply/csv_data/hydro/"); files
# 
# dummy<-unlist(strsplit(files, "[.]"))
# dummy1<-matrix(dummy, nrow=65, byrow=TRUE)
# uniqueMons<-dummy1[,1]
# 
# #dates<-seq(as.Date("2008-03-15"), as.Date("2013-07-15"), by="months")
# 
# nastrings<-c("DAILY REPORT OF HYDRO RESERVOIRS FOR","FULL","RESERVOIR","LEVEL","DAY LAST",'YEAR','PRESENT',"STATE","TIAL","LEVEL ON",'THE SAME','F.R.L. PRESENT', 'THE MONTH','MIN.DRAW','DOWN','LEVEL','F.R.L','ANNUAL','DESIGN','POTEN-','ENERGY ENERGY','CONTANT CONTANT','AT',"1ST.OF", "EFF.CAP",'CAP','AT','CUMULATIVE','ENERGY',"PRESENT","GEN.FROM")
# colnames<-c("Stn_name","State","FRL.m","FRL.ft","PRL.m","PRL.ft","LYRL.m","LYRL.ft","MinDDL.m","MinDDL.ft","AnnDesign.MU","FRL.MU","PRL.MU","FRL.MCM","FRL.MAFT","PRL.MCM","PRL.MAFT","MonGen.MU")
# 
# #### getData 
# for (i in 1:length(files)) {
#   #getData <- function(i) {
#   print(i)
#   theFile <- paste("/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/Fuel Supply/csv_data/hydro/",files[i],sep="")
#   if (!file.exists(theFile)) next
#   data<-read.table(file=theFile, sep=",", strip.white=TRUE, col.names=colnames, blank.lines.skip=TRUE, fill=TRUE, skip=10, header=FALSE, check.names=TRUE, comment="#", na.strings=nastrings)
#   
#   #   data<-read.table(file=theFile, sep=",", strip.white=TRUE, col.names=colnames, colClasses=c(rep("factor",2), rep("numeric",16)), blank.lines.skip=TRUE, fill=TRUE, skip=10, header=FALSE, check.names=TRUE, comment="#", na.strings=nastrings)
#   
#   # QAQC
#   dim(data)          #1736 x 18
#   test<-na.omit(data)
#   dim(test)          #331 x 18
#   cc<-complete.cases(test)
#   data<-test[cc,]    #331 x 18
#   
#   ## check for NAs
#   print(which(is.na(data), arr.ind=TRUE))  #position of NA's (in any) --> None.
#   look<-which(is.na(data), arr.ind=TRUE)
#   print(data[look[,1],])
#   
#   # Continue with data import...
#   # data <- subset(data, !is.na(as.numeric(Stn_num)) ) #remove all rows that are not associated with a station number
#   
#   data<-droplevels(data)
#   data$uniqueMon<-uniqueMons[i]
#   if(i==1) {hydro <- data} else {hydro <- rbind(hydro,data)}
#   str(data)
# }
# str(hydro)
# save(hydro, file="hydroraw.rsav")


## "hydro" contains mulitiple entries per month, but not necessarily one for each day... thus take monthly mean of each attribute.

## first convert "factor" to "numeric" to compute mean()
test<-hydro
# test[,3:18]<-as.numeric(test[,3:18])
test$FRL.m<-as.numeric(test$FRL.m)
test$FRL.ft<-as.numeric(test$FRL.ft)
test$PRL.m<-as.numeric(test$PRL.m)
test$PRL.ft<-as.numeric(test$PRL.ft)
# test$LYRL.m<-as.numeric(test$LYRL.m)
# test$LYRL.ft<-as.numeric(test$LYRL.ft)  # Warning: NAs introduced by coercion 
test$MinDDL.m<-as.numeric(test$MinDDL.m)
test$MinDDL.ft<-as.numeric(test$MinDDL.ft)
test$AnnDesign.MU<-as.numeric(test$AnnDesign.MU)
test$FRL.MU<-as.numeric(test$FRL.MU)
test$FRL.MCM<-as.numeric(test$FRL.MCM)
test$FRL.MAFT<-as.numeric(test$FRL.MAFT)
test$PRL.MU<-as.numeric(test$PRL.MU)
test$PRL.MCM<-as.numeric(test$PRL.MCM)
test$PRL.MAFT<-as.numeric(test$PRL.MAFT)
test$MonGen.MU<-as.numeric(test$MonGen.MU)

which(is.na(test), arr.ind=TRUE)  #position of NA's --> None.
look<-which(is.na(test), arr.ind=TRUE)
test[look[,1],]

#re-assign test back to hydro after converting colClasses
hydro<-test
head(hydro)

hydro$Date<-as.Date(paste("15", hydro$uniqueMon, sep=""), format="%d%b%y")
#hydro$Date<-format(Date, "%Y-%m-%d")
dummy<-strsplit(as.character(hydro$Date), split="-")
hydro$Year<-laply(dummy, '[[', 1)
hydro$Mon<-laply(dummy, '[[', 2)

test<-subset(hydro, select=c(20:22, 1:2, 3:18))
test2<-ddply(test, .(Date, Stn_name, State), numcolwise(mean))  #monthly mean status of all hydropower stns in India
test2[,4:17]<-round(test2[,4:17], digits=2)

hydro<-test2
save(hydro, file="hydro.rsav")
load("hydro.rsav")
## plot potential energy as a fn of reservoir height
p<-ggplot(hydro, aes(x=PRL.m, y=PRL.MU, group=Stn_name, colour=Stn_name)) 
p + geom_point() + facet_wrap(~Stn_name, scale="free")

## plot potential energy as a fn of reservoir volumne
p<-ggplot(hydro, aes(x=PRL.MCM, y=PRL.MU, group=Stn_name, colour=Stn_name)) 
p + geom_point() + facet_wrap(~Stn_name, scale="free")

## now compute "critical" value.  e.g. if PRL.PE < (0.2*FRL.PE)
n<-dim(hydro)[1]
for (j in 1:n){
  if(hydro$PRL.MU[j]<hydro$FRL.MU[j]*0.20) {hydro$Critical[j]=1} else 
    (hydro$Critical[j]=0)
}

## compute effective PAF e.g. (PRL.MU)/(FRL.MU)
hydro$effPAF<-hydro$PRL.MU/hydro$FRL.MU



# All-India hydropower reservoir energy storage (GWh potential energy)
p<-ggplot(hydro, aes(x=Date, y=PRL.MU, group=Stn_name, colour=Stn_name)) + geom_line()
p + scale_y_continuous(name='Energy Storage (GWh)') + scale_x_date(name="Date", breaks = date_breaks(width="6 months"), labels=date_format("%b-%Y")) + labs(title="Hydropower Reservoir Storage: Potential Energy\nAll-India, March 2008-July 2013")

## Excellent, now aggregate all Stn_name by state
hydro2<-ddply(hydro, .(Date, State), numcolwise(mean))  #monthly mean status of all hydropower stns in India
#hydro2[,3:16]<-round(hydro2[,3:16], digits=2)

p<-ggplot(hydro2, aes(x=Date, y=PRL.MU, group=State, colour=State)) + geom_line()
p + scale_y_continuous(name='Energy Storage (GWh)') + scale_x_date(name="Date", breaks = date_breaks(width="6 months"), labels=date_format("%b-%Y")) + labs(title="Hydropower Reservoir Storage by State\nAll-India, March 2008-July 2013")

# now aggregate all Stations
hydro3<-ddply(hydro, .(Date), numcolwise(mean))  #monthly mean status of all hydropower stns in India
#hydro3[,2:15]<-round(hydro3[,2:15], digits=2)


p<-ggplot(hydro3, aes(x=Date, y=PRL.MU)) + geom_line()
p + scale_y_continuous(name='Energy Storage (GWh)') + scale_x_date(name="Date", breaks = date_breaks(width="4 months"), labels=date_format("%b-%Y")) + labs(title="Hydropower Reservoir Storage\nAll-India, March 2008-July 2013")


############## KEY FIGURE 1 ###############
p<-ggplot(hydro, aes(x=Date, y=PRL.MU, group=Date)) + geom_boxplot()
p + scale_y_continuous(name='Energy Storage (GWh)') + scale_x_date(name="time", breaks = date_breaks(width="4 months"), labels=date_format("%b-%Y")) + labs(title="Hydroelectric Reserve Capacity (Reservoir Potential Energy)\nAll-India, Mar 2008-July 2013") + theme(axis.title = element_text(size = rel(1.3)),axis.text = element_text(size = rel(1.3)), plot.title = element_text(size = rel(1.4))) 
# + geom_smooth(aes(group=1), method="lm")
# if want a singly linear trend fit, use group=1
##################################################

############## KEY FIGURE 2 ###############
p<-ggplot(hydro, aes(x=Date, y=effPAF, group=Date)) + geom_boxplot()
p + scale_y_continuous(name='Available Fraction', limit=c(0,1)) + scale_x_date(name="time", breaks = date_breaks(width="4 months"), labels=date_format("%b-%Y")) + labs(title="Hydropower storage in terms of potential energy\nAll-India, Mar 2008-July 2013") + theme(axis.title = element_text(size = rel(1.1)),axis.text = element_text(size = rel(1)), plot.title = element_text(size = rel(1.2))) + geom_smooth(aes(group=1), method="lm")
# if want a singly linear trend fit, use group=1
##################################################
