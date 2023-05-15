library(lubridate)
library(tidyverse)
library(zoo)
library(tseries)


rm(list=ls())

##################################################
#
#     GENERATING A 30 MIN CONTINUOUS DATASET

###################################################
#INPUT FILE FOR THIS SCRIPT:"CR1000_merged.csv"
   
        
ts<-read.csv( "./DATA_CR1000/output/CR1000_merged.csv",sep=",",header=TRUE,dec=".")
head(ts)
tail(ts)

######################################################
#GAPFILLING ROWS BY GENERATING A NEW FILE
######################################################

## before starting, check the length of df... shouldn't have NA at the end
## also check format of timestamp

ts$Timestamp <- as.POSIXct(ts$Timestamp,format="%Y-%m-%d %H:%M", tz = "UTC")

#to estimate necessary parameters to generate the new empty dataframe with complete rows 
begining<-as.numeric(ts$Timestamp[1])                           #to find the number that is represented by the first data of our time series
as.POSIXct(begining,origin="1970-01-01 00:00:00",tz="UTC") #to confirm that the begining is the right one
ts$Timestamp[1]
     
## Adding year and julian day columns

ts$year <- year(ts$Timestamp)
ts$jday <- yday(ts$Timestamp)
ts$hour_dec <- hour(ts$Timestamp) + minute(ts$Timestamp)/60
 
       
Ystart<-ts[1,match('year',names(ts))]
Yend<-ts[nrow(ts),match('year',names(ts))]
Dstart<-ts[1,match('jday',names(ts))]
Dend<-ts[nrow(ts),match('jday',names(ts))]
lsyd<-max(ts[ts$year==Ystart,match('jday',names(ts))],na.rm=T) #lsyd=last julian day of start year

Ystart
Yend
Dstart
Dend
lsyd

#Ndays= number of days in our time series 

 #If i AM USING A DATASET INCLUDING DATA FROM 2014 up to 2016:  
 #   if(Ystart==2014 & Yend==2016) {Ndays<-(365-Dstart)+366+Dend}
     
#If I AM USING A DATASET INCLUDING ONLY 2016 DATA (FROM END OF 2015 AND 2016 in UTC):  
#    if(Ystart==2015 & Yend==2016) {Ndays<-365-Dstart+Dend+(365*(Yend-Ystart-1))}

#If I AM USING A DATASET INCLUDING the complete 2016 (starting in Jan2016 and ending in Jan 2017 UTC time):  
#    if(Ystart==2017 & Yend==2018) {Ndays<-(365-(Dstart-1))+Dend}


Ndays<-(lsyd-(Dstart-1))+Dend
Ndays

ts$obs<-c(1:nrow(ts)) 

names(ts)
#CREATING A NEW DATAFRAME WITH CONTINUOUS TIME STEPS
#To generate a serie of all minutes in a day:
Tsteps <-begining+seq(from=0,to=((Ndays)*(60*60*24)),by=(30*60)) # half an hours data from the begining date to Ndays +1 (that way I assure all measured data are included in the new file) -> I did not use this method
DATE <-as.POSIXct(Tsteps,origin="1970-01-01 00:00:00",tz="UTC")

#Confirming length of the new Time string 
#(THE NEW FILE SHOULD END SAME TIME THE TIME SERIES START AND ONE DAY AFTER THE MEASUREMENTS TIME SERIE)
DATE[1]
DATE[length(Tsteps)]
ts$Timestamp[1]
ts$Timestamp[length(ts$Timestamp)]
      
#GENERATING A NEW DATA FRAME WITH CONTINUOUS TIME STEPS and data from THE ORIGINAL ONE
cont.DS<-as.data.frame(DATE)
cont.DS[,c(names(ts)[match( "AIR_TEMP_2M",names(ts)):length(names(ts))])]<-NA   #we will just copy the data from the variable "Batt_volt_Min" in ts (variable 3)
cont.DS$DATE<-DATE
head(cont.DS)

#FILLING THE NEW DATAFRAME WITH DATA FROM THE ORIGINAL DATA FRAME 
for(i in 2:ncol(cont.DS)){  
cont.DS[,i]<-ts[pmatch(cont.DS$DATE,ts$Timestamp),i]           #pmatch look for the observation rows when time columns of both (old and new) dataframes match
                          } 
cont.DS$year <- year(cont.DS$DATE)
cont.DS$jday <- yday(cont.DS$DATE)
cont.DS$hour_dec <- hour(cont.DS$DATE) + minute(cont.DS$DATE)/60


###############################
#APPLYING SYSTEMATIC CORRECTIONS:
################################
DATA<-cont.DS
DATA$obs<-c(1:nrow(cont.DS))
head(DATA)
tail(DATA)
DATA <- DATA[-nrow(DATA),]

###############################
#APPLYING wrong data CORRECTIONS:
################################

## Soil heat flux correction (for Burns Bog 2017-2018 particular case)
# Did not use SHFP_1 for 2017.09.15-2018.03.07 due to bad quality data
#plot original
par(mfrow=c(3,1))
plot(DATA$DATE,DATA$SHFP_1, main = "Soil Heat flux original from CR1000", type ="l", xlab = "Date", ylab= "SHF (W/m2)") 
points(DATA$DATE,DATA$SHFP_2,col='orange',type ="l")
points(DATA$DATE,DATA$SHFP_3,col='blue',type ="l")
legend('top',c('SHF1','SHF2','SHF3'),text.col=c('black','orange','blue'),bty='n')

bad_q <- which(DATA$DATE >= "2017-09-15" & DATA$DATE < "2018-03-08")
DATA$SHFP_1[bad_q] <- NA

#plot after qc
plot(DATA$DATE,DATA$SHFP_1, main = "Soil Heat flux after QC", type ="l", xlab = "Date", ylab= "SHF (W/m2)") 
points(DATA$DATE,DATA$SHFP_2,col='orange',type ="l")
points(DATA$DATE,DATA$SHFP_3,col='blue',type ="l")
legend('top',c('SHF1','SHF2','SHF3'),text.col=c('black','orange','blue'),bty='n')

# From 2018.03.07, SHFP_1 should be corrected by muliplying -1
correction <- which(DATA$DATE >= "2018-03-08")
DATA$SHFP_1[correction] <- DATA$SHFP_1[correction] * -1

#plot after correction
plot(DATA$DATE,DATA$SHFP_1, main = "Soil Heat flux after correction", type ="l", xlab = "Date", ylab= "SHF (W/m2)") 
points(DATA$DATE,DATA$SHFP_2,col='orange',type ="l")
points(DATA$DATE,DATA$SHFP_3,col='blue',type ="l")
legend('top',c('SHF1','SHF2','SHF3'),text.col=c('black','orange','blue'),bty='n')

## Wind speed correction (for Burns Bog 2017-2018 particular case)
# Did not use cup wind speed for 2017.07.04-2018.02.09 due to bad quality data
bad_q <- which(DATA$DATE >= "2017-07-04" & DATA$DATE < "2018-02-10")
DATA$WIND_VELOCITY_CUP[bad_q] <- NA
# Replace this gap using EC wind speed and then remove EC wind speed column
DATA$WIND_VELOCITY_CUP[bad_q] <- DATA$WIND_VELOCITY_EC2[bad_q]
DATA <- DATA[, -grep("WIND_VELOCITY_EC2", colnames(DATA))]


## WTH data from the web should be changed by the data from Johannes
wth <- read_csv("./DATA_CR1000/output/WTH_noisy_removed.csv")
# time fitting
st <- which(wth$Date == DATA$DATE[1])
la <- which(wth$Date == DATA$DATE[nrow(DATA)])
wth_use <- wth[c(st:la),]

#plot original:
par(mfrow=c(3,1))
plot(DATA$DATE,DATA$WTH, main = "WTH from CR1000", xlab = "Date", ylab= "WTH")

# change the data
DATA$WTH <- wth_use$ma_WTH

#plot after correction
plot(DATA$DATE,wth_use$WTH, main = "Corrected WTH by ground level measurment", xlab = "Date", ylab= "WTH")
plot(DATA$DATE,DATA$WTH, main = "Moving average (1day) for old sensor only period (Before 2017-11)", xlab = "Date", ylab= "WTH")


##########################################################################################################################
#Gapfilling using environment canada dataset (Burns Bog site) & climateATUBC (http://ibis.geog.ubc.ca/~achristn/data.html)
##########################################################################################################################
no_gapfilled <- DATA

### 1. environment canada dataset: airT, RH, wind speed
near_station <- read_csv("./DATA_CR1000/output/merged_station_data.csv")
names(near_station)

par(mfrow=c(3,1))

for (i in 1:3){
	var <- names(near_station)[i+5]
	gap_row <- which(is.na(DATA[, grep(var, colnames(DATA))]))
	intersect_row_for_near_station <- which(near_station$Timestamp %in% DATA$DATE[gap_row])
	intersect_row_for_DATA <- which(DATA$DATE %in% near_station$Timestamp[intersect_row_for_near_station])
	DATA[intersect_row_for_DATA, grep(var, colnames(DATA))] <- near_station[intersect_row_for_near_station, i+5]
	
	# linear interporation: environment canada data is 1 hour, so need to interporated to make 30 min dataset
	DATA[, grep(var, colnames(DATA))] <- na.approx(DATA[, grep(var, colnames(DATA))])
	
	#ploting
	plot(DATA$DATE,DATA[, grep(var, colnames(DATA))], type='l',ylab=var,col='red', main= "red: near weather station data")
	points(DATA$DATE,no_gapfilled[, grep(var, colnames(DATA))],col='black',type='l')
	
}


### 2. climateATUBC dataset: Rg, ppt

ubc <- read_csv("./DATA_CR1000/output/climateAtUBC.csv")
# Date character -> Date data
ubc$DATE <- as.POSIXct(ubc$time_stamp,format="%m/%d/%Y %I:%M:%S %p", tz = "UTC")
gap <- which(is.na(ubc$DATE))
ubc$DATE[gap] <- as.POSIXct(ubc$time_stamp,format="%m/%d/%Y", tz = "UTC")[gap]
#subset
st <- which(ubc$DATE== DATA$DATE[1])
la <- which(ubc$DATE == DATA$DATE[nrow(DATA)])
ubc_use <- ubc[c(st:la),]

# Because this data tower is pretty far from the BB tower, I need to check relationship


## 2.1. Rg
#linear regression
a<-round(coef(lm(DATA$SHORTWAVE_IN~ubc_use$global_radiation_main))[2],digits=2) #coef(lm(y~x))[2] is the slope of the regression
b<-round(coef(lm(DATA$SHORTWAVE_IN~ubc_use$global_radiation_main))[1],digits=2)   
r2<-round(summary(lm(DATA$SHORTWAVE_IN~ubc_use$global_radiation_main))$ r.squared,digits=2)
lm_eq<-paste0("y=",a,"x",ifelse(b>0,"+",""),b)
R2<-bquote(R^2 == .(r2)) 

#gapfilling
DATA$SHORTWAVE_IN[which(is.na(DATA$SHORTWAVE_IN))] <- ubc_use$global_radiation_main[which(is.na(DATA$SHORTWAVE_IN))]*a + b

#Plot:
par(mfrow=c(3,1))

plot(ubc_use$global_radiation_main,no_gapfilled$SHORTWAVE_IN, main = "Global radiation (Short wave in)", xlab = "UBC climate site", ylab = "BB flux tower")
abline(0,1,col='grey',lty=2)
abline(lm(DATA$SHORTWAVE_IN~ubc_use$global_radiation_main),col='red')
mtext( lm_eq,line=-2, side = 2)
mtext(R2,line=-4, side = 2)

#Plot time series:
plot(no_gapfilled$SHORTWAVE_IN, main="Rg time series",ylab="Wm-2")
points(ubc_use$global_radiation_main,col='orange')
legend('top',c('BB tower','UBC tower'),text.col=c('black','orange'),bty='n')

# gapfilling plot
plot(DATA$SHORTWAVE_IN,col ="red", main="Rg time series",ylab="Wm-2")
points(no_gapfilled$SHORTWAVE_IN,col='black')
legend('top',c('gap-filled','original'),text.col=c('red','black'),bty='n')

## 2.2. Precipitation
#gapfilling
DATA$PRECIP[which(is.na(DATA$PRECIP))] <- ubc_use$precipitation_main[which(is.na(DATA$PRECIP))]

#plot:
par(mfrow=c(3,1))
plot(DATA$DATE,no_gapfilled$PRECIP, main="Precipitation-BB",ylab="ppt/30min (mm)", type = "h")
plot(DATA$DATE, ubc_use$precipitation_main, main="Precipitation-UBC",ylab="ppt/30min (mm)", type = "h")

plot(DATA$DATE,DATA$PRECIP, main="Precipitation-gapfilled",ylab="ppt/30min (mm)", type = "h", col ="red")
points(DATA$DATE,no_gapfilled$PRECIP,col='black', type = "h")
legend('top',c('gap-filled','original'),text.col=c('red','black'),bty='n')


## 2.3. soil Temp 10cm gap filling

#linear regression
a<-round(coef(lm(no_gapfilled$SOIL_TEMP_10CM~ubc_use$soil_temperature_10cm))[2],digits=2) #coef(lm(y~x))[2] is the slope of the regression
b<-round(coef(lm(no_gapfilled$SOIL_TEMP_10CM~ubc_use$soil_temperature_10cm))[1],digits=2)   
r2<-round(summary(lm(no_gapfilled$SOIL_TEMP_10CM~ubc_use$soil_temperature_10cm))$ r.squared,digits=2)
lm_eq<-paste0("y=",a,"x",ifelse(b>0,"+",""),b)
R2<-bquote(R^2 == .(r2)) 

#gapfilling
DATA$SOIL_TEMP_10CM[which(is.na(no_gapfilled$SOIL_TEMP_10CM))] <- ubc_use$soil_temperature_10cm[which(is.na(no_gapfilled$SOIL_TEMP_10CM))]*a + b

#plot:
par(mfrow=c(3,1))

plot(ubc_use$soil_temperature_10cm,no_gapfilled$SOIL_TEMP_10CM, main = "Soil Temp", xlab = "UBC climate site", ylab = "BB flux tower")
abline(0,1,col='grey',lty=2)
abline(lm(DATA$SOIL_TEMP_10CM~ubc_use$soil_temperature_10cm),col='red')
mtext( lm_eq,line=-2, side = 2)
mtext(R2,line=-4, side = 2)

plot(DATA$DATE,ubc_use$soil_temperature_10cm,col ="orange", main="UBC climate site",ylab="soil T")

# gapfilling plot
plot(DATA$DATE,DATA$SOIL_TEMP_10CM,col ="red", main="Soil Temp time series",ylab="soil T")
points(DATA$DATE,no_gapfilled$SOIL_TEMP_10CM,col='black')
legend('top',c('gap-filled','original'),text.col=c('red','black'),bty='n')

## 2.3.2 soil Temp 5cm gap filling

#linear regression
a<-round(coef(lm(no_gapfilled$SOIL_TEMP_5CM~ubc_use$soil_temperature_10cm))[2],digits=2) #coef(lm(y~x))[2] is the slope of the regression
b<-round(coef(lm(no_gapfilled$SOIL_TEMP_5CM~ubc_use$soil_temperature_10cm))[1],digits=2)   
r2<-round(summary(lm(no_gapfilled$SOIL_TEMP_5CM~ubc_use$soil_temperature_10cm))$ r.squared,digits=2)
lm_eq<-paste0("y=",a,"x",ifelse(b>0,"+",""),b)
R2<-bquote(R^2 == .(r2)) 

#gapfilling
DATA$SOIL_TEMP_5CM[which(is.na(no_gapfilled$SOIL_TEMP_5CM))] <- ubc_use$soil_temperature_10cm[which(is.na(no_gapfilled$SOIL_TEMP_5CM))]*a + b

#plot:
par(mfrow=c(3,1))

plot(ubc_use$soil_temperature_10cm,no_gapfilled$SOIL_TEMP_5CM, main = "Soil Temp", xlab = "UBC climate site", ylab = "BB flux tower")
abline(0,1,col='grey',lty=2)
abline(lm(DATA$SOIL_TEMP_5CM~ubc_use$soil_temperature_10cm),col='red')
mtext( lm_eq,line=-2, side = 2)
mtext(R2,line=-4, side = 2)

plot(DATA$DATE,ubc_use$soil_temperature_10cm,col ="orange", main="UBC climate site",ylab="soil T")

# gapfilling plot
plot(DATA$DATE,DATA$SOIL_TEMP_5CM,col ="red", main="Soil Temp time series",ylab="soil T")
points(DATA$DATE,no_gapfilled$SOIL_TEMP_5CM,col='black')
legend('top',c('gap-filled','original'),text.col=c('red','black'),bty='n')



#################################################################################
### Making new column for net radiation, soil heat flux, available energy, es, vpd
##################################################################################
DATA$NR <- (DATA$SHORTWAVE_IN - DATA$SHORTWAVE_OUT) + 
  (DATA$LONGWAVE_IN - DATA$LONGWAVE_OUT)

DATA$G<-rowMeans(DATA[,grep('SHFP',names(DATA))],na.rm = TRUE)

DATA$AE<-DATA$NR-DATA$G

DATA$es <- 0.611*exp(17.502*DATA$AIR_TEMP_2M/(DATA$AIR_TEMP_2M+240.97))
DATA$VPD <- DATA$es*(1-(DATA$RH_2M/100))

head(DATA)

for (i in 2:ncol(DATA)){
	use <- DATA[,i]
	plot(DATA$DATE,use,ylab = names(DATA)[i])
}

##################################################################
#SAVING THE NEW FILE WITHOUT MISSING ROWS AND CORRECTIONS APPLIED:
##################################################################  

write.csv(DATA,paste('./DATA_CR1000/output/CR1000_gapfilled_corrected','.csv',sep=''),row.names=FALSE)      

