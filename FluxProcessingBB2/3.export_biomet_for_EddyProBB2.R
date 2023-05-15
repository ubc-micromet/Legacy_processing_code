# Script written to export biomet data for EddyPro

library(readr)
library(tidyverse)

rm(list=ls())

# Load data
#data <- read_csv('./Flux-tower/met_data/met_merged/met_corrected_gapfilled.csv')
data <- read_csv('/Users/marionnyberg/Google\ Drive/Micromet\ Lab/Projects/2019-Burns\ Bog\ 2/Flux-tower/met_data/met_merged/met_corrected_gapfilledBB2.csv') 

# Change data class to posixct
#data$DATE <- as.POSIXct(data$DATE, format='%Y-%m-%d %H%M')

# Extract variables of interest
data2 <- data %>% 
	select(DATE, AIR_TEMP_2M, PA_1.5M)

data2$DATE <- format(data2$DATE,format='%Y-%m-%d %H%M')

# Replace NA with -9999
data2[is.na(data2)] <- -9999

# Rename variables of interest
names(data2) <- c("TIMESTAMP_1", "Ta", "Pa")

# Add units
data2 <- rbind(c("yyyy-mm-dd HHMM", "C", "kPa"), data2)

# Export
#write.table(data2, "./Flux-tower/EP_outputs/biomet_for_EP.txt",
#						sep = ",", row.names=FALSE, quote=FALSE)

#export location 
write.table(data2, "/Users/marionnyberg/Google\ Drive/Micromet\ Lab/Projects/2019-Burns\ Bog\ 2/Flux-tower/EP_outputs/biomet_for_EP_BB2.txt",
						sep = ",", row.names=FALSE, quote=FALSE)





