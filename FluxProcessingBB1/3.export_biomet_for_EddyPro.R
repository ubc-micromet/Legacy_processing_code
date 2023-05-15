# Script written to export biomet data for EddyPro

library(readr)
library(tidyverse)

rm(list=ls())

dir <- "G:/.shortcut-targets-by-id/1txCh9lZ7VGCujXGvBaJCMVnuxT-65q4K/Micromet Lab/Projects/2014-BB1 Burns Bog/Flux-tower (1)"

# Load data
#data <- read_csv('./Flux-tower/met_data/met_merged/met_corrected_gapfilled.csv')
data <- read_csv(paste0(dir, '/met_data/met_merged/met_corrected_gapfilled.csv')) 

# Extract variables of interest
data2 <- data %>% 
  select(DATE, AIR_TEMP_2M, PA_2M)

data2$DATE <- format(data2$DATE,format='%Y-%m-%d %H%M')

# Replace NA with -9999
data2[is.na(data2)] <- -9999

# Rename variables of interest
names(data2) <- c("TIMESTAMP_1", "Ta", "Pa")

# Add units
data2 <- rbind(c("yyyy-mm-dd HHMM", "C", "kPa"), data2)

# Export
write.table(data2, paste0(dir, "/EP_outputs/biomet_for_EP.txt"), row.names=FALSE, quote=FALSE)





