#2018-08-16 11:49 --> 2018-10-16 9:33(or 2018-11-01) raw values differ greatly from preceeding values suggesting that the sensor was moved. 
#Therefore may need a different calibration. The folder WTH in Flux-tower>met_data has the files in this script. 

# 05/06/2020
# To calibrate the sensor and reference to the top of the well, first read in the csv that contains manual measurements 
# taken by JE and continuous pressure transducer measurements - "WT.csv" 

dir <- "G:/.shortcut-targets-by-id/1txCh9lZ7VGCujXGvBaJCMVnuxT-65q4K/Micromet Lab/Projects/2014-BB1 Burns Bog/Flux-tower"

WT <- read.csv(paste0(dir, "/met_data/WTH/WT.csv"))
lm <- lm(WT_man ~ WT_PT, WT) #Calculate the slope and offset for the pressure transducer measurements
summary(lm)
slope <- -0.90252 #corrects for inaccuracies due to pressure transducer measurements
intercept <- 82.47439 #intercept to correct for the offset
Pipe <- 4.606 #height of well pipe a.s.l

#Now read in continuous pressure transducer and snow depth measurements from the data logger
BBWPT <- read.csv(paste0(dir, "/met_data/cr1000/BBWPT.csv"))
BBWPT <- BBWPT[-c(4)] #remove extra empty column at end
names(BBWPT) <- c("DATE", "WTH", "BH")
BBWPT$DATE <- as.POSIXct(BBWPT$DATE, format="%Y-%m-%d %H:%M:%S")

PT_correc <- which(BBWPT$DATE >= "2018-08-09 04:00:00") #This is where WTH_noisy_removed stops and therefore all pressure transducer data after needs to be corrected
cground_correc <- which(BBWPT$DATE >="2018-08-09 00:00:00" & BBWPT$DATE <= "2019-01-01 00:00:00") #This is where the cground measurement is used rather than bog height from the snow sensor
BH_correc <- which(BBWPT$DATE >= "2018-12-06 13:30:00") #This is where snow depth sensor measurements start

BBWPT$WTH_corr[PT_correc] <- Pipe - ((slope * BBWPT$WTH[PT_correc] + intercept)/100) # Apply correction to raw pressure transducer/logger data

plot(BBWPT$DATE, BBWPT$WTH_corr)

#Now WTH height has been corrected, however it is not referenced to bog height:

# To correct snow depth so that it is referenced to mean sea level: 
# First read in the csv that contains manual measurements alongside the raw sensor data. G_raw is the raw sensor data and G_manual are the manual measurements taken by JE.
# The date and time corresponds to the time the manual measurements, and the closest 30 minute measurement from the sensor is matched to it. 
BH <- read.csv(paste0(dir, "/met_data/WTH/BH.csv"))

lm <- lm(G_manual ~ G_raw, BH)
summary (lm)
slope <- 1.03

BBWPT$BH_corr <- (BBWPT$BH/100) * slope

Bref <- 3.452 #This is the bog height calculated when the GNSS survey was done at 2020-03-18 12:20

ind <- which(BBWPT$DATE =="2020-03-18 12:30") #Use the raw sensor data from when the GNSS survey was done
BBraw <- BBWPT$BH[ind]

offset <- Bref-(BBraw/100)

BBWPT$BH_corr_offset <- BBWPT$BH_corr + offset #Apply offset to the calibrated sensor measurements. 

plot(BBWPT$DATE[BH_correc], BBWPT$BH_corr_offset[BH_correc])


# TO CALCULATE WTH RELATIVE TO GROUND HEIGHT (I.E. SNOW DEPTH SENSOR MEASURMENTS) AND CGROUND MEASUREMENTS
#BBWPT$WTH_rel <- (BBWPT$BH_corr_offset - BBWPT$WTH_corr) 
BBWPT$WTH_rel[BH_correc] <- ( BBWPT$WTH_corr[BH_correc] - BBWPT$BH_corr_offset[BH_correc])


BBWPT$WTH_rel[BH_correc] <- ( BBWPT$BH_corr_offset[BH_correc] - BBWPT$WTH_corr[BH_correc])

# TO CALCULATE WTH RELATIVE TO CGROUND MEASUREMENTS 
cground <- 3.493

#BBWPT$WTH_rel[cground_correc] <- cground - BBWPT$WTH_corr[cground_correc]	
BBWPT$WTH_rel[cground_correc] <- (BBWPT$WTH_corr[cground_correc])	- cground

plot(BBWPT$DATE[PT_correc], BBWPT$WTH_rel[PT_correc])

WTH_new <- BBWPT %>%
	filter(DATE>= "2018-08-09 04:30:00" )

plot_ly(data = WTH_new, x = ~DATE, y = ~WTH_rel*100, name = 'Original', type = 'scatter', mode = 'lines')

write.csv(WTH_new, paste0(dir, "/met_data/WTH/WTH_new.csv")) 
