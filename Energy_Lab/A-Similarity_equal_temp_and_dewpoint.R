# Similarity analysis (with equal weights of temp and dew.point)</h1>
###############################
# A. filter the data  
###############################
# 1. use cities with both demand and weather data (24)  
data_filter <- function(data){
  if(length(levels(factor(data$YR)))<3) return(paste("Not enough data: ",length(levels(factor(data$YR))),"-year data",sep=""))
  else return(paste("qualified data: ",length(levels(factor(data$YR))),"-year data",sep=""))
}
## Filter the data
# the San Deiego demand data is from the Formatted_US_Demand.csv
# filter the demand data
setwd("/Users/YueGuo/Documents/Energy_Lab/data/")
demand_files <- list.files("demand")
demand_filter_results <- vector("character",length(demand_files))
for(i in 1:length(demand_files)){
  data = read.csv(paste("demand/",demand_files[i],sep=""))
  demand_filter_results[i] = data_filter(data)
}
# filter the weather data
setwd("/Users/YueGuo/Documents/Energy_Lab/data/")
weather_files <- list.files("weather")
weather_filter_results <- vector("character",length(weather_files))
for(i in 1:length(weather_files)){
  data = read.csv(paste("weather/",weather_files[i],sep=""))
  weather_filter_results[i] = data_filter(data)
}
# compare the results
city_names <- c("Abidjan_CotedIvoire","Accra_Ghana","Amman_Jordan","Antigua","Beirut_Lebanon",
                "Chandigarh_India","Dakar_Senegal","Delhi_Discoms","Delhi_India","Kano_Nigeria",
                "Kupang_Indonesia","Manila_Philippine","MbabaneCity_Swaziland","Nairobi_Kenya",
                "NewSouthWales_Australia","NYC_US","Philadelphia_US","Queensland_Australia","SanDiego_US",
                "Singapore_Singapore","SouthAustralia_Australia",
                "Tema_Ghana","Tokyo_Japan","Victoria_Australia")
# show the description of data size:
cbind(city_names,demand_filter_results,weather_filter_results)
# Comment: but there exits years with less months; developed cities have better data

# 2. use cities with no less than 2-year demand data  
# (1)more than 2-year demand data: for building a model(2 years) + test(1 year)  
# (2)no less than 1-year demand data: for similarity analysis  
# (3)NYC's weather data's detail is lost  
# (4)drop Delhi data
delete_list <- c(2,4-1,8-2,9-3,10-4,11-5,16-6,22-7)
for(i in 1:length(delete_list)){
  demand_files = demand_files[-delete_list[i]]
  weather_files = weather_files[-delete_list[i]]
  city_names = city_names[-delete_list[i]]
}
# 16 cities left (8 developing cities, 8 developed cities)
# load the data of chosen cities
data_list_demand <- vector("list",16)
data_list_weather <- vector("list",16)
setwd("/Users/YueGuo/Documents/Energy_Lab/data/")
for(i in 1:16){
  data_list_demand[[i]] = read.csv(paste("demand/",demand_files[i],sep=""))
  data_list_weather[[i]] = read.csv(paste("weather/",weather_files[i],sep=""))
}
# Set same format of data's time for merging
data_Time <- function(data){
  # drop NA's
  data=na.omit(data)
  # set time
  if(names(data)[5]=="HR"){
    #if(nchar(deparse(format(data$hours[1])))>16){
    if(is.null(data$MIN)){
      data$Time = paste(data$YR,"-",data$M,"-",data$D," ",data$HR,":00:00",sep="")
      data$Time = as.POSIXct(data$Time, format="%Y-%m-%d %H:%M:%S") 
    } else {
      data$Time = paste(data$YR,"-",data$M,"-",data$D," ",data$HR,":",data$MIN,":00",sep="")
      data$Time = as.POSIXct(data$Time, format="%Y-%m-%d %H:%M:%S") 
    }
    #else {data$Time = as.POSIXct(paste("20",sub('^..','',as.character(as.POSIXct(data$hours, format="%m/%d/%Y %H:%M"))),sep=""),format="%Y-%m-%d %H:%M:%S")}
  }
  else{
    if(nchar(deparse(format(data$hours[1])))>16){
      data$Time = as.POSIXct(data$hours, format="%Y-%m-%d %H:%M:%S")} 
    else {data$Time = as.POSIXct(paste("20",sub('^..','',as.character(as.POSIXct(data$hours, format="%m/%d/%Y %H:%M"))),sep=""),format="%Y-%m-%d %H:%M:%S")}
  }
  return(data)  
}
# the date
data_Date <- function(data){
  data$Date <- as.POSIXct(substr(as.character(data$Time),1,10), format="%Y-%m-%d")
  return(data)
}
# set Date and Time for all data
for(i in 1:16){
  data_list_weather[[i]] = data_Time(data_list_weather[[i]])
  data_list_demand[[i]] = data_Time(data_list_demand[[i]])
  data_list_weather[[i]] = data_Date(data_list_weather[[i]])
  data_list_demand[[i]] = data_Date(data_list_demand[[i]])
}

###############################
# B. pair the cities
###############################
# The function to get the latest whole year weather data of each city
latest_year_weaData <- function(data){
  if(nrow(subset(data,data$YR==max(data$YR)))<364*24){
    data_use <- subset(data,data$YR==(max(data$YR)-1))
  }else{data_use <- subset(data,data$YR==max(data$YR))}
}
ly_data_list_weather <- vector("list",16)
for(i in 1:16){
  ly_data_list_weather[[i]] = latest_year_weaData(data_list_weather[[i]])
  ly_data_list_weather[[i]]$Time_no_year = substr(as.character(ly_data_list_weather[[i]]$Time),6,19)
}
# The similarity score function:
similarity <- function(data1_use,data2_use){
  # merge the data according to the lastest year data
  compare_data=merge(data1_use[, c("Time_no_year", "TEMP", "DEW.POINT")], 
                     data2_use[, c("Time_no_year", "TEMP", "DEW.POINT")], by="Time_no_year")
  # calculate the similarity score
  simil_score = sum(compare_data[,2]*compare_data[,4])/(sum(compare_data[,2]^2))^.5/(sum(compare_data[,4]^2))^.5 + sum(compare_data[,3]*compare_data[,5])/(sum(compare_data[,3]^2))^.5/(sum(compare_data[,5]^2))^.5
  return(simil_score)
}
# calculate all chosen cities' similarity scores
similarity_matrix <- matrix(0,16,16)
for(i in 1:15){
  for(j in (i+1):16){
    similarity_matrix[i,j] = similarity(ly_data_list_weather[[i]],ly_data_list_weather[[j]])
  }
}
for(i in 1:16){
  for(j in 1:i){
    similarity_matrix[i,j] = similarity_matrix[j,i]
  }
}
diag(similarity_matrix) <- rep(2,16)
rownames(similarity_matrix) <- city_names
colnames(similarity_matrix) <- city_names
## developed - stable GDP growth rate
## developing - unstable GDP growth rate

# find the max scores for a developing city with all developed cities
similar_city <- function(city_name){
  similarity_df <- data.frame(similarity_matrix)
  command <- paste("similarity_df","$",city_name,sep="")
  simils <- eval(parse(text = command))[9:16]
  results <- vector("list",4)
  results[[1]] <- city_names[which.max(simils)+8]
  results[[2]] <- max(simils)
  results[[3]] <- data_list_weather[[which.max(simils)+8]]$LONG[1]
  results[[4]] <- data_list_weather[[which.max(simils)+8]]$LAT[1]
  return(results)
}
similar_city_list <- vector("character",8)
similar_score_list <- vector("character",8)
similar_long_list <- vector("character",8)
similar_lat_list <- vector("character",8)
long_list <- vector("character",8)
lat_list <- vector("character",8)
for(i in 1:8){
  similar_city_list[i] = similar_city(city_names[i])[[1]]
  similar_score_list[i] = similar_city(city_names[i])[[2]]
  similar_long_list[i] = similar_city(city_names[i])[[3]]
  long_list[i] = data_list_weather[[i]]$LONG[1]
  similar_lat_list[i] = similar_city(city_names[i])[[4]]
  lat_list[i] = data_list_weather[[i]]$LAT[1]
}

# show the pairs
compare <- data.frame(cbind(city_names[1:8],long_list,lat_list,similar_long_list,similar_lat_list,similar_city_list,similar_score_list))
names(compare) <- c("city","long","lat","center-long","center-lat","center_city","similar_score")
compare

# show the groups on the map:
# 1.Singapore group
S_sim <- rbind(compare[1,],compare[5:8,])
map_S_sim <- data.frame(cities = c(as.character(S_sim[,1]),"Singapore_Singapore"),
                        long = c(as.numeric(as.character(S_sim[,2])),103.867),
                        lat = c(as.numeric(as.character(S_sim[,3])),1.417)
)
leaflet(data = map_S_sim) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~cities)

# 2.San Diego group
SD_sim <- rbind(compare[2:4,])
map_SD_sim <- data.frame(cities = c(as.character(SD_sim[,1]),"SanDiego_US"),
                         long = c(as.numeric(as.character(SD_sim[,2])),-117.1635),
                         lat = c(as.numeric(as.character(SD_sim[,3])),32.734)
)
leaflet(data = map_SD_sim) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~cities)

# the table of groups
Similarity_tabel <- data.frame(group = c("group1(5 cities)","group2(3 cities)"),
                               developed_city = c("Singapore","San Diego"),
                               developed_city_latitude = c(1.417,32.734),
                               mean_latitude =c(mean(as.numeric(as.character(S_sim$lat))),mean(as.numeric(as.character(SD_sim$lat)))),
                               mean_similarity_score = c(mean(as.numeric(as.character(S_sim$similar_score))),mean(as.numeric(as.character(SD_sim$similar_score)))),
                               climate_zone = c("-","-"))
## the temperature dominate the similarity score

# output demand data that we'll use
save(data_list_demand, file = "DemandData_use.RData")
