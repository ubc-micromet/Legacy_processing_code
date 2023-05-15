
####################################
#INPUT FILE FOR THIS SCRIPT:  "BB_prefiltered"

#OUTPUT FILES: "BB_FILTERED"        
#              "BB_filtering_summary"
#              "BB_flags_summary"
#              "BB_Fluxes_QAQC"
#           

##################################################################
# Filtering FLUXES
# This generates Site_QAQC.csv FILES
##################################################################

          ###############
          # FILTER 1:
          ###############
          ## What it is filtered here??
					##		0.Too high Momentum flux >1
          ##    1.Rainy periods (ch4_flux)
          ##    3.Instrumental failure/calibration period (LE and H20_flux,co2_flux,and ET,ch4_flux)
          ##    4.H, LE and Co2 reliability threshold (H, LE and H20_flux,co2_flux,and ET,ch4_flux ) 
          ##    5.Too high nightime (Rs<10) LE (>50) when qc_LE>0     (LE and H20_fluxand ET ) 
          ##    6.Negative daytime LE ( when Rs>50  and Tair>Tdew)  (LE and H20_flux and ET ) 
          ##    7.qc_flux==2 (quality check from Eddypro) (all fluxes)
          ##    8.signal strength 7200 ( "mean_value_RSSI_LI.7200") < 20 (H, LE and H20_flux,co2_flux and ET ) 
          ##    9.signal strength 7700<20  (ch4_flux) 
          ##    10.Energy constrains daytime: LE>AE or H>AE WHEN qc=1 (H, LE, H20_flux and ET ) 
					##		11.CO2,CH4 remove when Tau,H,H2O flux is not reliable


##FLAGS pending to be applied for co2_flux FILTERING:
          #Notes: 
          #       Co2_flux needs much more work yet to filter by standard deviation and by reasonable limits
          #       Include filtering from licor diagnostic parameters




################
# Code start
##################
# clear memory
rm(list=ls())
########################################

# FILTERING DATASETS
########################################

############################################################
# 1. GENERAL PERIODS TO REMOVE BECAUSE OF MAINTENANCE WORK OR KNOWN INSTRUMENTAL FAILURE
############################################################

############################
#2. Importing prefiltered data:
############################

input<-read.csv( paste('./BB_prefiltered.csv',sep=''))

##########################
##3. Creating flags by site:
#########################

input$DATE<-as.POSIXct(input$DATE,origin="1970-01-01 00:00:00",tz="UTC")
Data_QAQC<-input[,c(1:match("qc_ch4_flux",names(input)),match('ET',names(input)))]


#Renaming signal strength:

    names(input)[names(input)=="mean_value_RSSI_LI.7200"] <- "CO2_signal_strength"
    names(input)[names(input)=="rssi_77_mean"] <- "CH4_signal_strength"
    
#General flags (for several fluxes filtering):
    #When Tau is <1:
    Data_QAQC$tau_flag<-ifelse(input$Tau >= 1,1,0)
    
    #When there is rain and signal strength is <20:
    Data_QAQC$rain_flag<-ifelse(input$PRECIP>0 & !is.na(input$PRECIP) & input$CH4_signal_strength<20,1,0)
    #Instrumental failure/calibration periods:
#  Data_QAQC$inst_flag<-ifelse(input$DATE>=P1_V[1] & input$DATE<=P1_V[2],1,0) # include this if there are more periods ifelse(input$DATE>=P2_V[1] & input$DATE<=P2_V[2]
    #When The licor is too dirty:
    sig_str_min<-50
    Data_QAQC$signal_flag<-ifelse(input$CO2_signal_strength<=sig_str_min,1,0) 
    
    #LE AND H ENERGY CONSTRAINTS (LE>AE (Rn-G) or H>AE when qc=1
   # Data_QAQC$LE_outEC_flag <- ifelse(input$qc_LE>0 & input$LE>input$AE,1,0)
   # Data_QAQC$H_outEC_flag <- ifelse(input$qc_H>0 & input$H > input$AE,1,0) 

 #LE specific flags:
    #Too high nightime LE: 
    Data_QAQC$night_LE_flag<-ifelse( input$SHORTWAVE_IN<10 & input$LE>=50, 1,0)
    #Negative daytime LE: 
    Data_QAQC$day_LE_flag<-ifelse( input$SHORTWAVE_IN>=10 & input$LE<0, 1,0)
    #Out of reasonable boundaries LE:
      LE_MAX<-300
      LE_MIN<--80
    Data_QAQC$lim_LE_flag<-ifelse( input$LE<LE_MIN | input$LE>LE_MAX, 1,0)
    
  
 #H specific flags:
    #Out of reasonable boundaries H:
        H_MAX<-400
        H_MIN<--100
    Data_QAQC$lim_H_flag<-ifelse( input$H<H_MIN | input$H>H_MAX, 1,0)
 #Co2 specific flags:
     #Out of reasonable boundaries CO2 FLUX (according to data observation during 2014-2015)
        CO2_MAX_summer <- 50
        CO2_MIN_summer <- -50
        CO2_MAX_winter <- 10
        CO2_MIN_winter <- -10
        
        winter <- which(input$jday >= 300 | input$jday <= 60)
        summer <- which(!(input$jday >= 300 | input$jday <= 60))
        
        Data_QAQC$lim_co2_flag[summer] <- ifelse( input$co2_flux[summer] < CO2_MIN_summer | input$co2_flux[summer] > CO2_MAX_summer, 1,0)
        Data_QAQC$lim_co2_flag[winter] <- ifelse( input$co2_flux[winter] < CO2_MIN_winter | input$co2_flux[winter] > CO2_MAX_winter, 1,0)
        
       
        
  #Ch4 specific flags:
    #Out of reasonable boundaries Ch4 FLUX (according to data observation during 2014-2015)
    CH4_MAX<-3
    CH4_MIN<--3
    Data_QAQC$lim_ch4_flag<-ifelse( input$ch4_flux<CH4_MIN | input$ch4_flux>CH4_MAX, 1,0)
    
     #When The 7700 is too dirty:
    sig_str_min_ch4<-20
    Data_QAQC$ch4_signal_flag<-ifelse(input$CH4_signal_strength<=sig_str_min_ch4,1,0) 
    

#######################
#4. Applying the filters:
#######################
##To replace all the NA in the flag columns by 0 (so no filter):
FILTER<-Data_QAQC
FILTER[,grep('_flag',names(FILTER))][is.na(FILTER[,grep('_flag',names(FILTER))])]<-0


#Filtering (FILTERED DATA ARE TRANSFORMED INTO -9999
FILTER$Tau_filtered<-ifelse( FILTER$qc_Tau!=2 & FILTER$tau_flag ==0,FILTER$Tau ,-9999) 

FILTER$H_filtered<-ifelse( FILTER$qc_H!=2  &  FILTER$lim_H_flag==0 & FILTER$Tau_filtered != -9999,
													 FILTER$H ,-9999) #& FILTER$H_outEC_flag==0

FILTER$LE_filtered<-ifelse( FILTER$qc_LE!=2 &   
                            FILTER$lim_LE_flag==0 & FILTER$night_LE_flag==0 & FILTER$day_LE_flag==0 & 
                            FILTER$signal_flag==0 & FILTER$Tau_filtered != -9999,
														FILTER$LE ,-9999) # FILTER$LE_outEC_flag==0

FILTER$h2o_flux_filtered<-ifelse( FILTER$qc_h2o_flux!=2  &  FILTER$lim_LE_flag==0 & FILTER$night_LE_flag==0 & 
                                    FILTER$day_LE_flag==0 & FILTER$signal_flag==0 & FILTER$Tau_filtered != -9999,
																	FILTER$h2o_flux,-9999) #& FILTER$LE_outEC_flag==0

FILTER$ET_filtered<-ifelse( FILTER$qc_h2o_flux!=2 & FILTER$lim_LE_flag==0 & FILTER$night_LE_flag==0 & 
                              FILTER$day_LE_flag==0 &  FILTER$signal_flag==0 & FILTER$Tau_filtered != -9999,
														FILTER$ET,-9999) #FILTER$LE_outEC_flag==0 


FILTER$co2_flux_filtered<-ifelse( FILTER$qc_co2_flux!=2  & Data_QAQC$lim_co2_flag==0 & FILTER$signal_flag==0 & 
                                  FILTER$Tau_filtered != -9999,
																	FILTER$co2_flux,-9999) 

FILTER$ch4_flux_filtered<-ifelse( FILTER$qc_ch4_flux!=2  & Data_QAQC$lim_ch4_flag==0 & FILTER$ch4_signal_flag==0 & FILTER$rain_flag==0 & 
                                  FILTER$Tau_filtered != -9999,
																	FILTER$ch4_flux,-9999) 


#Counting the number of observations that have been filtered for each flux by these filters:
N_Tau_FILT1<-length(FILTER$Tau_filtered[!is.na(FILTER$Tau_filtered) & FILTER$Tau_filtered==-9999])
N_H_FILT1<-length(FILTER$H_filtered[!is.na(FILTER$H_filtered) & FILTER$H_filtered==-9999])
N_LE_FILT1<-length(FILTER$LE_filtered[!is.na(FILTER$LE_filtered) & FILTER$LE_filtered==-9999])
N_H2O_FILT1<-length(FILTER$h2o_flux_filtered[!is.na(FILTER$h2o_flux_filtered) & FILTER$h2o_flux_filtered==-9999])
N_ET_FILT1<-length(FILTER$ET_filtered[!is.na(FILTER$ET_filtered) & FILTER$ET_filtered==-9999])
N_CO2_FILT1<-length(FILTER$co2_flux_filtered[!is.na(FILTER$co2_flux_filtered) & FILTER$co2_flux_filtered==-9999])
N_CH4_FILT1<-length(FILTER$ch4_flux_filtered[!is.na(FILTER$ch4_flux_filtered) & FILTER$ch4_flux_filtered==-9999])



N_tot_filt<-c('N_filtered',length(FILTER$date[!is.na(FILTER$date)]),N_Tau_FILT1,N_H_FILT1,N_LE_FILT1,N_H2O_FILT1,N_ET_FILT1,N_CO2_FILT1,N_CH4_FILT1)

N_Tau_QC<-as.vector(table(Data_QAQC$qc_Tau))[3]
N_H_QC<-as.vector(table(Data_QAQC$qc_H))[3]
N_LE_QC<-as.vector(table(Data_QAQC$qc_LE))[3]
N_H2O_QC<-as.vector(table(Data_QAQC$qc_h2o_flux))[3]
N_ET_QC<-as.vector(table(Data_QAQC$qc_h2o_flux))[3]
N_CO2_QC<-as.vector(table(Data_QAQC$qc_co2_flux))[3]
N_CH4_QC<-as.vector(table(Data_QAQC$qc_ch4_flux))[3]
N_qc<-c("qc=2 N",NA,N_Tau_QC,N_H_QC,N_LE_QC,N_H2O_QC,N_ET_QC,N_CO2_QC,N_CH4_QC)

N_flags<-as.vector(colSums (Data_QAQC[,grep('flag',names(Data_QAQC))], na.rm = T))  

Flags_name<-names(Data_QAQC[,grep('flag',names(Data_QAQC))])        
Filtered_name<-c('VARIABLE','Total Obs',"Tau","H","LE","h2o_flux","ET","co2_flux",'ch4_flux')

N_flags<-as.data.frame(cbind(Flags_name,N_flags)) #To estimate the number of flag=1 by condition to be filtered
names(N_flags)<-c('Flag_name','N')

Summary_filt<-as.data.frame(rbind(N_qc,N_tot_filt))
names(Summary_filt)<-Filtered_name

#SHOW SUMMARIES TO CHECK:
Summary_filt
N_flags

write.csv(Summary_filt,paste('./BB_filtering_summary','.csv',sep=''),row.names=FALSE)   
write.csv(N_flags,paste('./BB_flags_summary','.csv',sep=''),row.names=FALSE)   


#Adding Filtered fluxes to Data_QAQC DATASET:
Data_QAQC$Tau_filtered<-ifelse(FILTER$Tau_filtered==-9999,NA,FILTER$Tau_filtered)
Data_QAQC$H_filtered<-ifelse(FILTER$H_filtered==-9999,NA,FILTER$H_filtered)
Data_QAQC$LE_filtered<-ifelse(FILTER$LE_filtered==-9999,NA,FILTER$LE_filtered)
Data_QAQC$co2_flux_filtered<-ifelse(FILTER$co2_flux_filtered==-9999,NA,FILTER$co2_flux_filtered)
Data_QAQC$h2o_flux_filtered<-ifelse(FILTER$h2o_flux_filtered==-9999,NA,FILTER$h2o_flux_filtered)
Data_QAQC$ET_filtered<-ifelse(FILTER$ET_filtered==-9999,NA,FILTER$ET_filtered)
Data_QAQC$ch4_flux_filtered<-ifelse(FILTER$ch4_flux_filtered==-9999,NA,FILTER$ch4_flux_filtered)

#Adding Filtered fluxes to Site_Filtered_files and choosing variables to include in this final dataset:

final<-cbind( input[,c(1,3:match('qc_ch4_flux',names(input)))],Data_QAQC[,match('Tau_filtered',names(Data_QAQC)):match('ch4_flux_filtered',names(Data_QAQC))],
              input[,match('H_strg',names(input)):match('T.',names(input))],input[,match('x_peak',names(input)):match('x_90.',names(input))],input[,match("CH4_signal_strength",names(input)):ncol(input)])

write.csv(Data_QAQC,paste('./BB_Fluxes_QAQC','.csv',sep=''),row.names=FALSE)   
write.csv(final,paste('./BB_FILTERED','.csv',sep=''),row.names=FALSE)

####################
#PLOTING FILTERING:
#####################

plot(Data_QAQC$Tau,type='l',ylab="Tau", main='Filter1')
points(Data_QAQC$Tau_filtered,col='red',type='l')

plot(Data_QAQC$H,type='l',ylab="H", main='Filter1')
points(Data_QAQC$H_filtered,col='red',type='l')

plot(Data_QAQC$LE,type='l',ylab="LE")
points(Data_QAQC$LE_filtered,col='red',type='l')

plot(Data_QAQC$h2o_flux,type='l',ylab="H2Oflux")
points(Data_QAQC$h2o_flux_filtered,col='red',type='l')

plot(Data_QAQC$co2_flux,type='l',ylab="CO2 flux")
points(Data_QAQC$co2_flux_filtered,col='red',type='l')

plot(Data_QAQC$ch4_flux, type='l',ylab = "CH4 flux")
points(Data_QAQC$ch4_flux_filtered,col='red',type='l')

#########################
#PLOT 1:ANNUAL FILTERING
########################

nameplot1<-paste("./filtered_plots/Annual.Filtering.Check",".pdf",sep="")
nameplot1

pdf(file = nameplot1)

for (i in 2017:2018){
    YEAR<-i
    
    par(mfrow=c(4,1))
    plot(final[final$Year_local== YEAR,match("DOY_local",names(final))],final[final$Year_local==YEAR,match("co2_flux",names(final))],
         ylab='CO2_flux (umol m-2 s-1)',xlab='doy',main=paste(YEAR),type='p',ylim=c(-70,70),col='red')
    points(final[ final$Year_local==YEAR,match("DOY_local",names(final))],final[final$Year_local==YEAR,match("co2_flux_filtered",names(final))])
    abline(v=c(1,32,60,91,121,152,182,213,244,274,305,335,365),col="grey",lty=3,lwd=2)
    
    plot(final[ final$Year_local==YEAR,match("DOY_local",names(final))],final[final$Year_local==YEAR,match("LE",names(final))],
         ylab='LE (Wm-2)',xlab='doy',type='p',ylim=c(-200,900),col='red')
    points(final[ final$Year_local==YEAR,match("DOY_local",names(final))],final[final$Year_local==YEAR,match("LE_filtered",names(final))])
    abline(v=c(1,32,60,91,121,152,182,213,244,274,305,335,365),col="grey",lty=3,lwd=2)
    
    plot(final[ final$Year_local==YEAR,match("DOY_local",names(final))],final[final$Year_local==YEAR,match("ET",names(final))],
         ylab='ET (mm)',xlab='doy',type='p',ylim=c(-1,2),col='red')
    points(final[ final$Year_local==YEAR,match("DOY_local",names(final))],final[final$Year_local==YEAR,match("ET_filtered",names(final))])
    abline(v=c(1,32,60,91,121,152,182,213,244,274,305,335,365),col="grey",lty=3,lwd=2)
    
    plot(final[ final$Year_local==YEAR,match("DOY_local",names(final))],final[final$Year_local==YEAR,match("ch4_flux",names(final))],
         ylab='CH4_flux (umol m-2 s-1)',xlab='doy',type='p',ylim=c(-5,5),col='red')
    points(final[ final$Year_local==YEAR,match("DOY_local",names(final))],final[final$Year_local==YEAR,match("ch4_flux_filtered",names(final))])
    abline(v=c(1,32,60,91,121,152,182,213,244,274,305,335,365),col="grey",lty=3,lwd=2)
    
}

dev.off()
  
#########################
#PLOT2:monthly Filtering
#########################

#Most important plot:
#By month of data 

CO2_F_LIM<-c(-25,25)
H20_F_LIM<-c(-10,40)
LE_LIM<-c(-100,300)
H_LIM<-c(-100,300)
CH4_F_LIM<-c(-3,3)

nameplot2<-paste("./filtered_plots/Monthly.Filtering.Checks",".pdf",sep="")
nameplot2
pdf(file = nameplot2)

    for (j in final$Year_local[1]: final$Year_local[nrow(final)]){
  
        year_months<-range(final[ final$Year_local==j,match("month_local",names(final))], na.rm = TRUE)  

    for (i in year_months[1]:year_months[2]){
    
    month<-c("January","February","March","April","May","June","July","August","September","October","November","December")    
    YEAR<-j
    MONTH<-i
    subset<-final[ final$Year_local==YEAR & final$month_local==MONTH,]

par(mfrow=c(4,1),mar=c(0,4,2,0),oma=c(3,1,1,1))

plot(subset[ ,match("DOY_local",names(subset))],subset[,match("co2_flux",names(subset))],
     ylab='CO2_flux (umol m-2 s-1)',xlab='DOY',type='p',ylim=CO2_F_LIM,col='red',main=paste(month[i], YEAR))
points(subset[ ,match("DOY_local",names(subset))], subset[  ,match("co2_flux_filtered",names(subset))])
legend('topleft',c("raw","filtered"),text.col=c('red','black'),bty="n")

plot(subset[ ,match("DOY_local",names(subset))],subset[ ,match("H",names(subset))],
     ylab='H (W m-2)',xlab='DOY',type='p',ylim=H_LIM,col='red') 
points(subset[ ,match("DOY_local",names(subset))],subset[ ,match("H_filtered",names(subset))])
legend('topleft',c("raw","filtered"),text.col=c('red','black'),bty="n")

plot(subset[  ,match("DOY_local",names(subset))],subset[,match("LE",names(subset))],
     ylab='LE (W m-2)',xlab='DOY',type='p',ylim=LE_LIM,col='red') 
points(subset[  ,match("DOY_local",names(subset))],subset[,match("LE_filtered",names(subset))])
legend('topleft',c("raw","filtered"),text.col=c('red','black'),bty="n")

plot(subset[ ,match("DOY_local",names(subset))],subset[,match("ch4_flux",names(subset))],
     ylab='Ch4_flux (umol m-2 s-1)',xlab='DOY',type='p',ylim=CH4_F_LIM,col='red',main=paste(month[i], YEAR))
points(subset[ ,match("DOY_local",names(subset))], subset[  ,match("ch4_flux_filtered",names(subset))])
legend('topleft',c("raw","filtered"),text.col=c('red','black'),bty="n")
}

}
dev.off()

#####################################
#Plots to evaluate balance closure (AFTER FILTERING)
####################################
nameplot3<-paste("./filtered_plots/Energy_balance.Checks",".pdf",sep="")
nameplot3
pdf(file = nameplot3)

LEH<-rowSums(final[,match(c("LE_filtered","H_filtered"),names(final))],na.rm=F)
  LEH_strg1<-rowSums(final[,match(c("LE_strg","H_strg"),names(final))],na.rm=F)
  LEH_strg<-ifelse(is.na(LEH),NA,LEH_strg1)

LEHplusSTRG<-LEH+LEH_strg

#.A Without considering the STORAGE compoonents OF LE and H 
#Checking linear regresion equations:
a<-round(coef(lm(LEH~final$AE))[2],digits=2) #coef(lm(y~x))[2] is the slope of the regression
b<-round(coef(lm(LEH~final$AE))[1],digits=2)   
r2<-round(summary(lm(LEH~final$AE))$ r.squared,digits=2)

lm_eq<-paste0("y=",a,"x",ifelse(b>0,"+",""),b)
R2<-bquote(R^2 == .(r2)) 

#Plot:
par(mfrow=c(1,2)) 
plot(final$AE, main="Energy closure time series",ylab="Wm-2")
points(LEH,col='orange')
abline(v=c(final[ final$DOY==1,match("obs",names(final))]),lty=2,col='grey')
legend('topleft',c('Rn-G','LE+H'),text.col=c('black','orange'),bty='n')



plot(final$AE, main="Energy closure (with S) time series",ylab="Wm-2")
points(LEHplusSTRG,col='orange')
abline(v=c(final[ final$DOY==1,match("obs",names(final))]),lty=2,col='grey')
legend('topleft',c('Rn-G','LE+H+Str'),text.col=c('black','orange'),bty='n')


#Comparison considering vs non considering STORAGE compoonents OF LE and H  for energy balance closure check:
    #.A Without considering the STORAGE compoonents OF LE and H 
#Checking linear regresion equations:
a<-round(coef(lm(LEH~final$AE))[2],digits=2) #coef(lm(y~x))[2] is the slope of the regression
b<-round(coef(lm(LEH~final$AE))[1],digits=2)   
r2<-round(summary(lm(LEH~final$AE))$ r.squared,digits=2)

lm_eq<-paste0("y=",a,"x",ifelse(b>0,"+",""),b)
R2<-bquote(R^2 == .(r2)) 

plot(final$AE,LEH,xlim=c(-100,1000),ylim=c(-100,1000),xlab="Rn-G",ylab="LE+H",main="30min (filtered data) without S term")
abline(0,1,col='grey',lty=2)
abline(lm(LEH ~ final$AE))
mtext( lm_eq,side=1,line=-20,at=200,cex=0.9)
mtext(R2,side=1,line=-19,at=200,cex=0.9)


    #.B Considering the STORAGE compoonents OF LE and H 
#Checking linear regresion equations:
a<-round(coef(lm(LEHplusSTRG~final$AE))[2],digits=2) #coef(lm(y~x))[2] is the slope of the regression
b<-round(coef(lm(LEHplusSTRG~final$AE))[1],digits=2)   
r2<-round(summary(lm(LEHplusSTRG~final$AE))$ r.squared,digits=2)

lm_eq<-paste0("y=",a,"x",ifelse(b>0,"+",""),b)
R2<-bquote(R^2 == .(r2)) 

#Plot:
plot(final$AE,LEHplusSTRG,xlim=c(-100,1000),ylim=c(-100,1000),xlab="Rn-G",ylab="LE+H+S",main="30min (filtered data) with S term")
abline(0,1,col='grey',lty=2)
abline(lm(LEHplusSTRG ~ final$AE))
mtext( lm_eq,side=1,line=-20,at=200,cex=0.9)
mtext(R2,side=1,line=-19,at=200,cex=0.9)




dev.off()

