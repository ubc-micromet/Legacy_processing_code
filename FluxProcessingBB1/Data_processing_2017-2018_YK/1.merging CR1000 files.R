### Script for merging all the data downloaded from the UBC online plotting
### Only has data from cr1000

### Modified from Yeonik Kim by Sara Knox (April 2019) 

rm(list=ls())

library(dplyr)
library(gtools)
library(gdata)

# set path
dir <- "./Personal Content/Projects/2014-Burns Bog/Data/cr1000"
getwd()
setwd(dir)

raw.files <- Sys.glob("*.csv")

# Initialize with Air temperature file - longest data record
cr1000 <- read.csv("BBDTA.csv",
                   header = TRUE,
                   sep = ",")
colnames(cr1000)[1] <- "Timestamp"
cr1000$Timestamp <- as.POSIXct(cr1000$Timestamp, format = "%Y-%m-%d %H:%M")

# Remova NA values (coinciding with daylight savings)
cr1000 <- cr1000[!is.na(cr1000$Timestamp), ]

# Now add other variable to dataframe
for (i in raw.files[2:length(raw.files)]) {
  sample <- read.csv(i,
                     header = TRUE,
                     sep = ",")
  colnames(sample)[1] <- "Timestamp"
  sample$Timestamp <- as.POSIXct(sample$Timestamp, format = "%Y-%m-%d %H:%M")
  
  # Remova NA values (coinciding with daylight savings)
  sample <- sample[!is.na(sample$Timestamp), ]
  
  cr1000 <-merge(x=cr1000,y=sample,by="Timestamp",all=TRUE)
}

cr1000$Timestamp <- as.POSIXct(cr1000$Timestamp, format = "%Y-%m-%d %H:%M")

names(cr1000)
names(cr1000) <- c("Timestamp", "AIR_TEMP_2M", "AIR_TEMP_38CM",
                   "ORP_10CM", "ORP_30CM", "INCOMING_PAR", "REFLECTED_PAR",
                   "PRECIP", "SHORTWAVE_IN", "SHORTWAVE_OUT",
                   "LONGWAVE_IN", "LONGWAVE_OUT", "RH_2M", "RH_38CM",
                    "SHFP_1", "SHFP_2", "SHFP_3", "SVWC", "SOIL_TEMP_5CM",
                   "SOIL_TEMP_10CM", "SOIL_TEMP_50CM", "WTH", "WATER_TEMP_10CM",
                   "WATER_TEMP_30CM", "WIND_VELOCITY_CUP", "WIND_VELOCITY_EC1", "WIND_VELOCITY_EC2")

cr1000 <- cr1000[, -grep("WIND_VELOCITY_EC1", colnames(cr1000))]
cr1000 <- cr1000[, -grep("WIND_VELOCITY_EC2", colnames(cr1000))]

head(cr1000)
str(cr1000)

write.csv(cr1000, paste('../met_merged/cr1000_merged','.csv',sep=''),row.names=FALSE)

#-----------------------------------------------------------------------------------------------------------------
## Plot data to ensure everything looks correct
library(ggplot2)
library(tidyr)

# Create year & DOY variables
cr1000 <- cr1000 %>%
  mutate(year = as.numeric(format(Timestamp,'%Y')),
         DOY = as.numeric(format(Timestamp,'%j')))

cr1000$year <- factor(cr1000$year)

#-----------------------------------------------------------------------------------------------------------------
# Air temperature - full time series
TA <- cr1000 %>%
  select(Timestamp, AIR_TEMP_2M, AIR_TEMP_38CM) %>%
  gather(key = "variable", value = "value", -Timestamp)
head(TA, 3)

ggplot(TA, aes(x = Timestamp, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

# Air temperature - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = AIR_TEMP_2M, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

ggplot(data = cr1000, mapping = aes(x = DOY, y = AIR_TEMP_38CM, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# Oxidation, reduction potential - full time series
ORP <- cr1000 %>%
  select(Timestamp, ORP_10CM, ORP_30CM) %>%
  gather(key = "variable", value = "value", -Timestamp)
head(ORP, 3)

ggplot(ORP, aes(x = Timestamp, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

# ORP - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = ORP_10CM, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

ggplot(data = cr1000, mapping = aes(x = DOY, y = ORP_30CM, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# PAR - full time series
PAR <- cr1000 %>%
  select(Timestamp, INCOMING_PAR, REFLECTED_PAR) %>%
  gather(key = "variable", value = "value", -Timestamp)
head(PAR, 3)

ggplot(PAR, aes(x = Timestamp, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

# PAR - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = INCOMING_PAR, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

ggplot(data = cr1000, mapping = aes(x = DOY, y = REFLECTED_PAR, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# PRECIP - full time series
ggplot(data = cr1000, mapping = aes(x = Timestamp, y = PRECIP, na.rm = TRUE)) +
  geom_line()

# PRECIP - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = PRECIP, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# SVWC - full time series
ggplot(data = cr1000, mapping = aes(x = Timestamp, y = SVWC, na.rm = TRUE)) +
  geom_line()

# SVWC - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = SVWC, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# WTH - full time series
ggplot(data = cr1000, mapping = aes(x = Timestamp, y = WTH, na.rm = TRUE)) +
  geom_line()

# WTH - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = WTH, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# SW - full time series
SW <- cr1000 %>%
  select(Timestamp, SHORTWAVE_IN, SHORTWAVE_OUT) %>%
  gather(key = "variable", value = "value", -Timestamp)
head(PAR, 3)

ggplot(SW, aes(x = Timestamp, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

# SW - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = SHORTWAVE_IN, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

ggplot(data = cr1000, mapping = aes(x = DOY, y = SHORTWAVE_OUT, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# LW - full time series
LW <- cr1000 %>%
  select(Timestamp, LONGWAVE_IN, LONGWAVE_OUT) %>%
  gather(key = "variable", value = "value", -Timestamp)
head(PAR, 3)

ggplot(LW, aes(x = Timestamp, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

# LW - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = LONGWAVE_IN, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

ggplot(data = cr1000, mapping = aes(x = DOY, y = LONGWAVE_OUT, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# RH - full time series
RH <- cr1000 %>%
  select(Timestamp, RH_2M, RH_38CM) %>%
  gather(key = "variable", value = "value", -Timestamp)
head(RH, 3)

ggplot(RH, aes(x = Timestamp, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

# RH - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = RH_2M, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

ggplot(data = cr1000, mapping = aes(x = DOY, y = RH_38CM, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# SHFP - full time series
SHFP <- cr1000 %>%
  select(Timestamp, SHFP_1, SHFP_2, SHFP_3) %>%
  gather(key = "variable", value = "value", -Timestamp)
head(SHFP, 3)

ggplot(SHFP, aes(x = Timestamp, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "darkorange2")) +
  theme_minimal()

# SHFP - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = SHFP_1, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

ggplot(data = cr1000, mapping = aes(x = DOY, y = SHFP_2, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

ggplot(data = cr1000, mapping = aes(x = DOY, y = SHFP_3, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# TS - full time series
TS <- cr1000 %>%
  select(Timestamp, SOIL_TEMP_5CM, SOIL_TEMP_10CM, SOIL_TEMP_50CM) %>%
  gather(key = "variable", value = "value", -Timestamp)
head(TS, 3)

ggplot(TS, aes(x = Timestamp, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "darkorange2")) +
  theme_minimal()

# TS - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = SOIL_TEMP_5CM, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

ggplot(data = cr1000, mapping = aes(x = DOY, y = SOIL_TEMP_10CM, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

ggplot(data = cr1000, mapping = aes(x = DOY, y = SOIL_TEMP_50CM, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# WATER TEMP - full time series
TW <- cr1000 %>%
  select(Timestamp, WATER_TEMP_10CM, WATER_TEMP_30CM) %>%
  gather(key = "variable", value = "value", -Timestamp)
head(TW, 3)

ggplot(TW, aes(x = Timestamp, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

# TW - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = WATER_TEMP_10CM, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

ggplot(data = cr1000, mapping = aes(x = DOY, y = WATER_TEMP_30CM, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# WS - full time series
ggplot(data = cr1000, mapping = aes(x = Timestamp, y = WIND_VELOCITY_CUP, na.rm = TRUE)) +
  geom_line()

# WS - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = WIND_VELOCITY_CUP, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))



