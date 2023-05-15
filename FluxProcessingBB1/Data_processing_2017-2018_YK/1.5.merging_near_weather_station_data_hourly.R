### Load library
library(lubridate)
library(tidyverse)
library(gtools)
library(gdata)

### Script for merging all the data downloaded from Environment Canada Burns Bog weather station data (hourly)
### (http://climate.weather.gc.ca/climate_data/hourly_data_e.html?hlyRange=2010-11-19%7C2019-04-21&dlyRange=2011-01-25%7C2019-04-21&mlyRange=%7C&StationID=49088&Prov=BC&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2019&selRowPerPage=25&Line=0&searchMethod=contains&Month=4&Day=21&txtStationName=Delta+Burns+Bog&timeframe=1&Year=2019)
### Once on the website, specify data (end of month to download the data for the full month >> Then Click Go >> Then click Download data & move to ECCC folder)
### The second part of this script was added by Marion in May 2020 because EC changed the format of csv files and therefore they aren't compatible with original script/loop
### The script is now in multiple parts, as EC has changed the format how the data file. When you need files from 2022, the code will have to be modified to include those. 

### Currently updated to  March 2021


rm(list=ls())

# set path

#path <- "./Flux-tower/met_data/met_ancillary/ECCC"
path <- "G:/.shortcut-targets-by-id/1txCh9lZ7VGCujXGvBaJCMVnuxT-65q4K/Micromet Lab/Projects/2014-BB1 Burns Bog/Flux-tower (1)/met_data/met_ancillary/ECCC"
#path <- "/Users/marionnyberg/Google\ Drive/BC/UBC/BB/eccc"
getwd()
setwd(path)

# Road data and merge
raw.files <- Sys.glob("eng-hourly-*.csv")

for (i in raw.files){
	use <- read_csv(i)
	metainfo_row <- which(use == "Date/Time")
	
	use <- read_csv(i,skip = metainfo_row+2)
	
	use <- use[,-c(7,8,9,11,13,15,16,17,19:24)]
	
	if (i == raw.files[1]){
		data <- use
	} else {
		data <- rbind(data,use)
	}
}

# Rename and wind speed unit change (km/h -> m/s)
names(data) <- c("Timestamp", "Year", "Month", "Day", "Time",
								 "AIR_TEMP_2M", "RH_2M", "WIND_DIRECTION", "WIND_VELOCITY_CUP", "PA_2M")
data$WIND_VELOCITY_CUP <- data$WIND_VELOCITY_CUP * 1000/(60*60)
data$WIND_DIRECTION <- data$WIND_DIRECTION * 10

#Reorder the dataset following Timestamp
data <- arrange(data,Timestamp)

# Plot a few environmental variables to ensure everything looks correct
library(openair)
library(plotly)

plot_ly(data = data, x = ~Timestamp, y = ~AIR_TEMP_2M, name = 'Cup', type = 'scatter', mode = 'lines') 
plot_ly(data = data, x = ~Timestamp, y = ~PA_2M, name = 'Cup', type = 'scatter', mode = 'lines') 

windRose(data, ws = "WIND_VELOCITY_CUP", wd = "WIND_DIRECTION")

getwd()
#write.csv(data, paste('ECCC_met_merged','.csv',sep=''),row.names=FALSE) This has been moved to the end of the script


### PART 2
# New EC files (downloaded from October 2019 onwards)
#raw.files2 <- Sys.glob("en_climate_hourly_*.csv")

raw.files2 <- Sys.glob("*-2019_P1H.csv")


for (i in raw.files2){
	use <- read_csv(i)
	#metainfo_row <- which(use == "Date/Time")
	
	#use2 <- read_csv(i,skip = metainfo_row2)
	
	use <- use[,-c(1:4,11:13,15,17,19:21,23:28)]
	
	if (i == raw.files2[1]){
		data2 <- use
	} else {
		data2 <- rbind(data2,use)
	}
}


names(data2) <- c("Timestamp", "Year", "Month", "Day", "Time",
								 "AIR_TEMP_2M", "RH_2M", "WIND_DIRECTION", "WIND_VELOCITY_CUP", "PA_2M")
data2$WIND_VELOCITY_CUP <- data2$WIND_VELOCITY_CUP * 1000/(60*60)
data2$WIND_DIRECTION <- data2$WIND_DIRECTION * 10
data2$Timestamp <- as.POSIXct(data2$Timestamp)
data2$Month <- as.character(data2$Month)
data2$Day <- as.character(data2$Day)

data2 <- arrange(data2,Timestamp)



### PART 3
#Added 09/03/2021 - EC changed file format

raw.files3 <- Sys.glob("*-2020_P1H.csv")

for (i in raw.files3){
	use <- read_csv(i)
	#metainfo_row <- which(use == "Date/Time")
	
	#use2 <- read_csv(i,skip = metainfo_row2)
	
	use <- use[,-c(1:4,11:13,15,17,19:21,23:28)]
	
	if (i == raw.files3[1]){
		data3 <- use
	} else {
		data3 <- rbind(data3,use)
	}
}

names(data3) <- c("Timestamp", "Year", "Month", "Day", "Time",
									"AIR_TEMP_2M", "RH_2M", "WIND_DIRECTION", "WIND_VELOCITY_CUP", "PA_2M")
data3$WIND_VELOCITY_CUP <- data3$WIND_VELOCITY_CUP * 1000/(60*60)
data3$WIND_DIRECTION <- data3$WIND_DIRECTION * 10
data3$Timestamp <- as.POSIXct(data3$Timestamp)
data3$Month <- as.character(data3$Month)
data3$Day <- as.character(data3$Day)

data3 <- arrange(data3,Timestamp)



### PART 4
# 2021 files (which include precipitation now when downloading from EC)
raw.files4 <- Sys.glob("*-2021_P1H.csv")

for (i in raw.files4){
	use <- read_csv(i)
	#metainfo_row <- which(use == "Date/Time")
	
	#use2 <- read_csv(i,skip = metainfo_row2)
	
	use <- use[,-c(1:4,11:13,15:17,19,21:23,25:30)]
	
	if (i == raw.files4[1]){
		data4 <- use
	} else {
		data4 <- rbind(data4,use)
	}
}

names(data4) <- c("Timestamp", "Year", "Month", "Day", "Time",
									"AIR_TEMP_2M", "RH_2M", "WIND_DIRECTION", "WIND_VELOCITY_CUP", "PA_2M")
data4$WIND_VELOCITY_CUP <- data4$WIND_VELOCITY_CUP * 1000/(60*60)
data4$WIND_DIRECTION <- data4$WIND_DIRECTION * 10
data4$Timestamp <- as.POSIXct(data4$Timestamp)
data4$Month <- as.character(data4$Month)
data4$Day <- as.character(data4$Day)

data4 <- arrange(data4,Timestamp)

allyears <- rbind(data, data2, data3, data4) #to merge the old climate data with the new one


write.csv(allyears, paste('ECCC_met_merged','.csv',sep=''),row.names=FALSE) #merge old EC and current EC files together and write csv file

plot_ly(data = allyears, x = ~Timestamp, y = ~AIR_TEMP_2M, name = 'Cup', type = 'scatter', mode = 'lines') 
plot_ly(data = allyears, x = ~Timestamp, y = ~PA_2M, name = 'Cup', type = 'scatter', mode = 'lines') 
