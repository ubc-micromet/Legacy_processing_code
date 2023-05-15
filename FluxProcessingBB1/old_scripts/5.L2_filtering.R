# This code is for filtering fluxes
# Filtering is modelled after L2 filtering from the UCB Biomet Lab

# Input files:  "BB_L1"

# Output files: "BB_L2"        

# Clear memory ------------------------------------------------------------

rm(list=ls())


# Load libraries ----------------------------------------------------------

library(plotly)
library(readxl)
library(REddyProc)
library(tidyverse)
library(dplyr)
library(lubridate)


# Load non-filtered data --------------------------------------------------

input<-read.csv( paste('./Flux-tower/flux_data/BB_L1.csv',sep=''))
input$DATE<-as.POSIXct(input$DATE,format="%m/%d/%y %H:%M", tz = "UTC")

# Define new data frame
Data_QAQC<-input


# Define L2 filters -------------------------------------------------------

pitch_max <- 7 #[deg]
pitch_min <- -7 #[deg]
# WD -> ADD LATER!
qc_flag <- 1 #[#]
w.ts_cov_max <- 0.5 #[m+1K+1s-1]
ts_var_max <- 3 #[K+2]
mean_value_RSSI_LI.7200_min <- 50 #[#]
h2o_flux_min <- -2 #[mmol+1s-1m-2]
h2o_flux_max <- 15 #[mmol+1s-1m-2]
h2o_var_max <- 20
h2o_mean_max <- 2000 #[mmol/m3]
co2_mean_min <- 12 #[mmol/m3]
co2_var_max <- 100
co2_flux_max <- 50 #[µmol+1s-1m-2]
co2_flux_min <- -60 #[µmol+1s-1m-2]
rssi_77_mean_min <- 20; #[#]
ch4_mean_min <- 1.7 # [ppm]
ch4_var_max <- 0.005
ch4_flux_min <- -1 #[µmol+1s-1m-2]
ch4_flux_max <- 4 #[µmol+1s-1m-2]

# Plot filtering variable of interest to check values
p <- plot_ly(data = Data_QAQC, x = ~DATE, y = ~ch4_var, name = 'pitch', type = 'scatter', mode = 'line') 
#p <- layout(p, yaxis = list(range = c(-10,100)))
p


# Create flags (with the exception of the ustar flag) ---------------------

Data_QAQC$badrot <- ifelse(input$pitch > pitch_max | input$pitch < pitch_min,1,0)
Data_QAQC$badt <- ifelse(abs(input$w.ts_cov) > w.ts_cov_max | input$ts_var > ts_var_max | input$qc_H > qc_flag,1,0)
Data_QAQC$badq <- ifelse(input$h2o_flux < h2o_flux_min | input$h2o_flux > h2o_flux_max | input$h2o_var > h2o_var_max |
                           input$h2o_mean > h2o_mean_max | input$mean_value_RSSI_LI.7200 < mean_value_RSSI_LI.7200_min | input$qc_LE > qc_flag,1,0)
Data_QAQC$badc <- ifelse(input$co2_flux < co2_flux_min | input$co2_flux > co2_flux_max | input$co2_var > co2_var_max |
                           input$co2_mean < co2_mean_min | input$mean_value_RSSI_LI.7200 < mean_value_RSSI_LI.7200_min | input$qc_co2_flux > qc_flag,1,0)
Data_QAQC$badm <- ifelse(input$ch4_flux < ch4_flux_min | input$ch4_flux > ch4_flux_max | input$ch4_var > ch4_var_max |
                           input$ch4_mean < ch4_mean_min | input$rssi_77_mean < rssi_77_mean_min | input$qc_ch4_flux > qc_flag,1,0)

# Filter relevant CO2 variables to run ustar filtering next -------------

Data_QAQC$co2_flux[Data_QAQC$badc == 1] <- NA
Data_QAQC$co2_strg[Data_QAQC$badc == 1 | (input$co2_strg > co2_flux_max | input$co2_strg < co2_flux_min)] <- NA

# Plot filtered CO2 data
plot_ly(data = input, x = ~DATE, y = ~co2_flux, name = 'original', type = 'scatter', mode = 'lines') %>%
  add_trace(data = Data_QAQC, x = ~DATE, y = ~co2_flux, name = 'filtered', mode = 'lines') 

p <- plot_ly(data = input, x = ~DATE, y = ~co2_strg, name = 'original', type = 'scatter', mode = 'lines') %>%
  add_trace(data = Data_QAQC, x = ~DATE, y = ~co2_strg, name = 'filtered', mode = 'lines') 
p <- layout(p, yaxis = list(range = c(-60,50)))
p

# Calculate NEE (i.e. sum of co2_flux & co2_strg) & plot
Data_QAQC$NEE<-rowSums(Data_QAQC[,match(c("co2_flux","co2_strg"),names(Data_QAQC))],na.rm=F)  

plot_ly(data = Data_QAQC, x = ~DATE, y = ~co2_flux, name = 'original', type = 'scatter', mode = 'lines') %>%
  add_trace(data = Data_QAQC, x = ~DATE, y = ~NEE, name = 'filtered', mode = 'lines') 


# Prepare data for ustar filtering using REddyProc ------------------------

# Load met data to
met <- read.csv( paste('./Flux-tower/met_data/met_merged/met_corrected_gapfilled','.csv',sep=""),sep=",",header=TRUE,dec=".")

#Match met and flux time series
met$DATE <- as.POSIXct(met$DATE, Origin = "1970-01-01 00:00:00", tz = "UTC")

# Plot met data if needed
plot_ly(data = met, x = ~DATE, y = ~AIR_TEMP_2M, type = 'scatter', mode = 'lines') 

Data_QAQC$DATE[1]
met$DATE[1]

Data_QAQC$obs<-c(1:nrow(Data_QAQC))

start_date<-Data_QAQC$DATE[1]

if (Data_QAQC$DATE[nrow(Data_QAQC)]>met[nrow(met),1]) { end_date<-met[nrow(met),1] 
}else{
  end_date<-Data_QAQC$DATE[nrow(Data_QAQC)]
}

start_date  #start of matching period
end_date    #end of matching period

F_start<-Data_QAQC[Data_QAQC$DATE==start_date,match('obs',names(Data_QAQC))] 
F_end<-Data_QAQC[Data_QAQC$DATE==end_date,match('obs',names(Data_QAQC))]

M_start<-met[met$DATE==start_date,match('obs',names(met))]
M_end<-met[met$DATE==end_date,match('obs',names(met))]

M_end-M_start==F_end-F_start   #If everything goes well this should be TRUE

# Create output file to use in REddyProc for ustar filtering
output <- cbind(met[M_start:M_end,match(c('year','jday','hour_dec','AIR_TEMP_2M','SHORTWAVE_IN'),names(met))], 
                Data_QAQC[F_start:F_end,match(c('NEE','u.'),names(Data_QAQC))])

# Reorder & rename columns
output <- output[c("year", "jday", "hour_dec","NEE","SHORTWAVE_IN","AIR_TEMP_2M","u.")]
names_output<-c('Year','DoY','Hour','NEE','Rg','Tair','Ustar')
names(output)<-names_output
  
#Adding the units row
UNITS<-list('-','-','-','umol_m-2_s-1','Wm-2','degC','ms-1')

output <- rbind(UNITS,output)

#Transforming missing values in -9999:
output[is.na(output)]<--9999

#Saving the file:
write.table(output, file = paste('./Flux-tower/flux_data/REddyProc_input/for_ustar_filtering','.txt',sep=''),row.names=FALSE,sep='\t')   


# Ustar filtering in REddyProc ------------------------------

# Load data
EddyData.F <- fLoadTXTIntoDataframe("./Flux-tower/flux_data/REddyProc_input/for_ustar_filtering.txt")

# Add time stamp in POSIX time format
EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH',Year.s = 'Year',Day.s = 'DoY',Hour.s = 'Hour')

EddyProc.C <- sEddyProc$new('CA-DBB', EddyDataWithPosix.F,
                            c("NEE","Rg","Tair","Ustar"))

# Sinlge ustar threshold estimate
uStarTh <- EddyProc.C$sEstUstarThreshold()$uStarTh

# ustar threshold decision: separate for each individual year
threshold <- uStarTh %>% filter(aggregationMode == "year")
threshold <- threshold[, c(2,4)]
years <- threshold[, 1]
nyears <- length(years)

Data_QAQC$ustar_thr <- rep(0, nrow(Data_QAQC))
for (i in 1:nyears){
  Data_QAQC$ustar_thr[year(Data_QAQC$DATE) == years[i]] <- threshold[i, 2]
}

# Plot ustar threshold
plot_ly(data = Data_QAQC, x = ~DATE, y = ~ustar_thr, type = 'scatter', mode = 'lines') 


# Create bad ustar flag ---------------------------------------------------

Data_QAQC$badustar <- ifelse(abs(input$u.) < Data_QAQC$ustar_thr,1,0)

plot_ly(data = Data_QAQC, x = ~DATE, y = ~co2_flux, name = 'original', type = 'scatter', mode = 'lines') %>%
  add_trace(data = Data_QAQC, x = ~DATE[badustar == 1], y = ~co2_flux[badustar == 1], name = 'removed', mode = 'markers') 


# Now filter everybody - ADD WIND DIRECTION FILTER! -----------------------

badflux = Data_QAQC$badustar | Data_QAQC$badt | Data_QAQC$badrot
Data_QAQC$L[badflux]  <- NA
Data_QAQC$X.z.d..L[badflux]  <- NA
Data_QAQC$co2_flux[badflux]  <- NA
Data_QAQC$ch4_flux[badflux]  <- NA
Data_QAQC$NEE[badflux]  <- NA
Data_QAQC$h2o_flux[badflux]  <- NA
Data_QAQC$ET[badflux]  <- NA
Data_QAQC$LE[badflux]  <- NA
Data_QAQC$H[badflux]  <- NA

# Filter momentum fluxes for bad rotation angle only
Data_QAQC$Tau[Data_QAQC$badrot] <- NA

# Bad water fluxes
Data_QAQC$h2o_flux[Data_QAQC$badq] <- NA
Data_QAQC$LE[Data_QAQC$badq] <- NA
Data_QAQC$LE[Data_QAQC$ET] <- NA

# Bad CO2 fluxes
Data_QAQC$co2_flux[Data_QAQC$badc] <- NA

# Bad CH4 fluxes
Data_QAQC$ch4_flux[Data_QAQC$badm] <- NA


# Plot L2 fluxes ----------------------------------------------------------
# Turbulence

# Add WD eventually

# ustar
plot_ly(data = input, x = ~DATE, y = ~u., name = 'original', type = 'scatter', mode = 'line') %>%
  add_trace(data = Data_QAQC, x = ~DATE[badustar == 1], y = ~u.[badustar == 1], name = 'filtered', mode = 'markers') 

# z/L
plot_ly(data = input, x = ~DATE, y = ~X.z.d..L, name = 'original', type = 'scatter', mode = 'line') %>%
  add_trace(data = Data_QAQC, x = ~DATE, y = ~X.z.d..L, name = 'retained', mode = 'line') 

# Fluxes
plot_ly(data = input, x = ~DATE, y = ~H, name = 'original', type = 'scatter', mode = 'line') %>%
  add_trace(data = Data_QAQC, x = ~DATE, y = ~H, name = 'retained', mode = 'line')

plot_ly(data = input, x = ~DATE, y = ~LE, name = 'original', type = 'scatter', mode = 'line') %>%
  add_trace(data = Data_QAQC, x = ~DATE, y = ~LE, name = 'retained', mode = 'line')

plot_ly(data = input, x = ~DATE, y = ~co2_flux, name = 'original', type = 'scatter', mode = 'line') %>%
  add_trace(data = Data_QAQC, x = ~DATE, y = ~co2_flux, name = 'retained', mode = 'line')

plot_ly(data = input, x = ~DATE, y = ~ch4_flux, name = 'original', type = 'scatter', mode = 'line') %>%
  add_trace(data = Data_QAQC, x = ~DATE, y = ~ch4_flux, name = 'retained', mode = 'line')

# Bad rotatoion angle
plot_ly(data = input, x = ~DATE, y = ~pitch, name = 'original', type = 'scatter', mode = 'line') %>%
  add_trace(data = Data_QAQC, x = ~DATE[badrot == 1], y = ~pitch[badrot == 1], name = 'filtered', mode = 'markers') 

# Variances
plot_ly(data = input, x = ~DATE, y = ~ts_var, name = 'original', type = 'scatter', mode = 'line') %>%
  add_trace(data = Data_QAQC, x = ~DATE[ts_var > ts_var_max], y = ~ts_var[ts_var > ts_var_max], name = 'filtered', mode = 'markers') 

plot_ly(data = input, x = ~DATE, y = ~h2o_var, name = 'original', type = 'scatter', mode = 'line') %>%
  add_trace(data = Data_QAQC, x = ~DATE[h2o_var > h2o_var_max], y = ~h2o_var[h2o_var > h2o_var_max], name = 'filtered', mode = 'markers') 

p <- plot_ly(data = input, x = ~DATE, y = ~co2_var, name = 'original', type = 'scatter', mode = 'line') %>%
  add_trace(data = Data_QAQC, x = ~DATE[co2_var > co2_var_max], y = ~co2_var[co2_var > co2_var_max], name = 'filtered', mode = 'line') 
p <- layout(p, yaxis = list(range = c(-100,100000)))
p


# Plot energy balance closure -----------------------------------------------------
Ebal_denominator <- met$NR[M_start:M_end]-met$G[M_start:M_end]
Ebal_numerator <- Data_QAQC$H+Data_QAQC$LE

plot(x = Ebal_denominator, y = Ebal_numerator, xlab="Rn-G",ylab="LE+H")

# calculate EB using slope to fit
a<-round(coef(lm(Ebal_numerator~Ebal_denominator))[2],digits=2) #coef(lm(y~x))[2] is the slope of the regression
b<-round(coef(lm(Ebal_numerator~Ebal_denominator))[1],digits=2)   
r2<-round(summary(lm(Ebal_numerator~Ebal_denominator))$ r.squared,digits=2)

lm_eq<-paste0("y=",a,"x",ifelse(b>0,"+",""),b)
R2<-bquote(R^2 == .(r2)) 

abline(0,1)
abline(lm(Ebal_numerator~Ebal_denominator),col='grey',lty=2)
mtext( lm_eq,side=1,line=-20,at=200,cex=0.9)
mtext(R2,side=1,line=-19,at=200,cex=0.9)

# Do diurnal approach - applies equal weight to all times of day

# Find index of first day starting at 00:30
ind <- which(Data_QAQC$min_local == 30 & Data_QAQC$hour_local == 0)
is <- ind[1]

# Find index of last day ending at 00:00
ind <- which(Data_QAQC$min_local == 00 & Data_QAQC$hour_local == 0)
ie <- ind[length(ind)]

Ebal_numerator_diel <- Ebal_numerator[is:ie]
Ebal_denominator_diel <- Ebal_denominator[is:ie]

Ebal_numerator_diel <- t(matrix(Ebal_numerator_diel, 48, length(Ebal_numerator[is:ie])/48))
Ebal_denominator_diel <- t(matrix(Ebal_denominator_diel, 48, length(Ebal_numerator[is:ie])/48))

Ebal_numerator_diurn <- colMeans(x = Ebal_numerator_diel, na.rm = TRUE)
Ebal_denominator_diurn = colMeans(x = Ebal_denominator_diel, na.rm = TRUE)

# Compute closure
Eclosure = sum(Ebal_numerator_diurn)/sum(Ebal_denominator_diurn);
mtext(paste('Diel Avg. Closure = ', round(Eclosure,2)),side=1,line=-2,at=550,cex=0.9)

# Save L2 output ----------------------------------------------------------

write.csv(Data_QAQC,paste('./Flux-tower/flux_data/BB_L2','.csv',sep=''),row.names=FALSE)   


# Now export data for gap-filling and partitioning using REddyProc ------------------------

#Converting units for the right input file format
met$VPD_hPa <- met$VPD*10

# Create output file to use in REddyProc for ustar filtering
output <- cbind(met[M_start:M_end,match(c('year','jday','hour_dec','AIR_TEMP_2M','SHORTWAVE_IN','RH_2M', 'VPD_hPa','SOIL_TEMP_5CM','SOIL_TEMP_10CM','WTH'),names(met))], 
                Data_QAQC[F_start:F_end,match(c('NEE','ch4_flux','LE','H','u.'),names(Data_QAQC))])


# Reorder & rename columns
output <- output[c("year", "jday", "hour_dec","NEE","ch4_flux","LE","H","SHORTWAVE_IN","AIR_TEMP_2M","SOIL_TEMP_5CM","SOIL_TEMP_10CM", "RH_2M","VPD_hPa","u.","WTH")]
names_output<-c('Year','DoY','Hour','NEE','CH4','LE','H','Rg','Tair','Tsoil5cm','Tsoil10cm','rH','VPD','Ustar','WTH')
names(output)<-names_output

#Adding the units row
UNITS<-list('-','-','-','umol_m-2_s-1','umol_m-2_s-1','Wm-2','Wm-2','Wm-2','degC','degC','degC','%','hPa','ms-1','cm')

output <- rbind(UNITS,output)

#Transforming missing values in -9999:
output[is.na(output)]<--9999

#Saving the file:
write.table(output, file = paste('./Flux-tower/flux_data/REddyProc_input/for_gap_filling_partitioning','.txt',sep=''),row.names=FALSE,sep='\t')   




