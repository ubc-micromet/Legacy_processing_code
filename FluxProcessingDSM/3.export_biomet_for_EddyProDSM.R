# Script written to export biomet data for EddyPro

library(readr)
library(tidyverse)

rm(list=ls())

# Load data
rdir <- '/Users/ziyi/Google Drive/.shortcut-targets-by-id/1txCh9lZ7VGCujXGvBaJCMVnuxT-65q4K/Micromet Lab/Projects/2021-DSM Delta salt marsh/Flux-tower'
siteName <-'DSM'
data <- read_csv(paste(rdir,'/met_data/met_merged/met_corrected_gapfilled',siteName,'.csv',sep=''))
data$DATE <- as.POSIXct(data$DATE, format='%Y-%m-%d %H%M')

names(data)[names(data) == "DATE"] <- "TIMESTAMP_1"
data$TIMESTAMP_1 <- format(data$TIMESTAMP_1,format='%Y-%m-%d %H%M')

# Convert temperature unit from C to K
data$TA_1_1_1 <- data$TA_1_1_1 + 273.15
data$TA_1_2_1 <- data$TA_1_2_1 + 273.15

data$TS_1_1_1 <- data$TS_1_1_1 + 273.15
data$TS_1_2_1 <- data$TS_1_2_1 + 273.15
data$TS_1_3_1 <- data$TS_1_3_1 + 273.15
data$TS_1_4_1 <- data$TS_1_4_1 + 273.15

data$TS_2_1_1 <- data$TS_2_1_1 + 273.15
data$TS_2_2_1 <- data$TS_2_2_1 + 273.15
data$TS_2_3_1 <- data$TS_2_3_1 + 273.15
data$TS_2_4_1 <- data$TS_2_4_1 + 273.15

data$TW_1_1_1 <- data$TW_1_1_1 + 273.15

# Replace NA with -9999
data[is.na(data)] <- -9999

# Add units
unit_str <- c("yyyy-mm-dd HHMM", "K", "K", "W/m^2 nm", "W/m^2 nm sr", "W/m^2 nm", 
              "W/m^2 nm sr", "unitless", "mV", "micromol/(m2s)", "micromol/(m2s)", 
              "micromol/(m2s)", "micromol/(m2s)", "mm", "W/m^2 nm", "W/m^2 nm sr", 
              "W/m^2 nm", "W/m^2 nm sr", "unitless", "kPa", "W/m2", "W/m2", "W/m2", 
              "W/m2", "%", "%", "W/m2", "W/m2", "W/m2", "K", "K", "K", 
              "K", "K", "K", "K", "K", "milliS/cm", "deg", "mg/l", 
              "%", "m", "K", "ms-1", "unitless", "yyyy", "ddd", "ddd", "unitless", 
              "unitless", "W/m2", "W/m2", "W/m2", "kPa", "kPa")

#unit_str <- c("yyyy-mm-dd HHMM", "degC", "degC", "W/m^2 nm", "W/m^2 nm sr", "W/m^2 nm", 
#              "W/m^2 nm sr", "unitless", "mV", "micromol/(m2s)", "micromol/(m2s)", 
#              "micromol/(m2s)", "micromol/(m2s)", "mm", "W/m^2 nm", "W/m^2 nm sr", 
#              "W/m^2 nm", "W/m^2 nm sr", "unitless", "kPa", "W/m2", "W/m2", "W/m2", 
#              "W/m2", "%", "%", "W/m2", "W/m2", "W/m2", "degC", "degC", "degC", 
#              "degC", "degC", "degC", "degC", "degC", "microS/cm", "deg", "mg/l", 
#              "%", "m", "degC", "ms-1", "unitless", "yyyy", "ddd", "ddd", "unitless", 
#              "unitless", "W/m2", "W/m2", "W/m2", "kPa", "kPa")

data <- rbind(unit_str, data)

# Export
write.table(data, paste(rdir,"/met_data/biomet_for_EP_",siteName,".txt",sep=""),sep = ",", row.names=FALSE, quote=FALSE)


