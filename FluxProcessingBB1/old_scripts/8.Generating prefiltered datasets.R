
########################################
# Script design Notes:

#This script is written to:
#  Merge Met AND EC data To Generate final dataset



####################################
#INPUT FILE FOR THIS SCRIPT:  
#"BurnsBog_fluxes_compiled.csv" and 
#"CR1000_gapfilled_corrected.csv"


# clear memory
rm(list=ls())


#############################################################################################
#CODE:

# 1. Reading all the files
###

#############################################################################################
#############################################################################################
#install.packages('plyr')
library(plyr); library(dplyr)


Fluxes<-read.csv(paste("./Flux-tower/EP_outputs/compiled/BurnsBog_fluxes_compiled",".csv",sep = ""),sep=",",header=TRUE,dec=".")  
CR1000<-read.csv(paste('./Flux-tower/met_data/met_merged/cr1000_gapfilled_corrected',".csv",sep = ""),sep=",",header=TRUE,dec=".")

head(Fluxes)
head(CR1000)

##################################################################
#2. Merge CR1000 AND PROCESSED FLUXES FILES
# This generates Site_prefiltered.csv files
##################################################################

#Looking for the time matching between CR1000 and Processed fluxes (start_date to end_date):

Fluxes$DATE<-as.POSIXct(Fluxes$DATE,origin="1970-01-01 00:00:00",tz="UTC")
CR1000$DATE <- as.POSIXct(CR1000$DATE, Origin = "1970-01-01 00:00:00", tz = "UTC")

Fluxes$DATE[1]
CR1000$DATE[1]

Fluxes$obs<-c(1:nrow(Fluxes))

start_date<-Fluxes$DATE[1]

if (Fluxes$DATE[nrow(Fluxes)]>CR1000[nrow(CR1000),1]) { end_date<-CR1000[nrow(CR1000),1] 
}else{
  end_date<-Fluxes$DATE[nrow(Fluxes)]
}

start_date  #start of matching period
end_date    #end of matching period

F_start<-Fluxes[Fluxes$DATE==start_date,match('obs',names(Fluxes))] 
F_end<-Fluxes[Fluxes$DATE==end_date,match('obs',names(Fluxes))]

C_start<-CR1000[CR1000$DATE==start_date,match('obs',names(CR1000))]
C_end<-CR1000[CR1000$DATE==end_date,match('obs',names(CR1000))]

C_end-C_start==F_end-F_start   #If everything goes well this should be TRUE


input_cont<-cbind(Fluxes[F_start:F_end,1:(ncol(Fluxes)-1)],CR1000[C_start:C_end,2:ncol(CR1000)])

#TO ORDER THE INPUT FOR HAVING ALL THE TIME COLUMNS FIRST:
#BB
names(input_cont)
  
input<-cbind(Fluxes[F_start:F_end,match('DATE',names(Fluxes)):match('daytime',names(Fluxes))],
             CR1000[C_start:C_end,match('year',names(CR1000)):match('hour_dec',names(CR1000))],
             Fluxes[F_start:F_end,match('Year_local',names(Fluxes)):match('DOY_local',names(Fluxes))],
             Fluxes[F_start:F_end,match('file_records',names(Fluxes)):match('rssi_77_mean',names(Fluxes))],
             CR1000[C_start:C_end,match('AIR_TEMP_2M',names(CR1000)):match('WIND_VELOCITY_CUP',names(CR1000))],
             CR1000[C_start:C_end,match('NR',names(CR1000)):match('VPD',names(CR1000))])
  

head(input)
input$obs<-c(1:nrow(input))
input$DATE<-as.POSIXct(input$DATE,origin="1970-01-01 00:00:00",tz="UTC")


write.csv(input,paste('./Flux-tower/merged_data/BB_prefiltered','.csv',sep=''),row.names=FALSE)



