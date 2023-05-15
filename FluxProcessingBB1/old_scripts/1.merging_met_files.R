### Script for merging all the data downloaded from the UBC online plotting
### Only has data from cr1000

### Modified from Yeonik Kim by Sara Knox (April 2019) 

rm(list=ls())

library(dplyr)
library(gtools)
library(gdata)

# set path
getwd()
dir <- "./Flux-tower/met_data/cr1000"
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
                   "PRECIP", "PA_EC1", "PA_EC2_TOTAL","PA_EC2_AIR","PA_2M","SHORTWAVE_IN", "SHORTWAVE_OUT",
                   "LONGWAVE_IN", "LONGWAVE_OUT", "RH_2M", "RH_38CM",
                    "SHFP_1", "SHFP_2", "SHFP_3", "SVWC", "SOIL_TEMP_5CM",
                   "SOIL_TEMP_10CM", "SOIL_TEMP_50CM", "WTH", "WATER_TEMP_10CM",
                   "WATER_TEMP_30CM", "WIND_VELOCITY_CUP", "WIND_VELOCITY_EC1", "WIND_VELOCITY_EC2")


# Convert all NaN to NA
is.nan.data.frame <- function(cr1000)
  do.call(cbind, lapply(cr1000, is.nan))

cr1000[is.nan(cr1000)] <- NA

cr1000 <- cr1000[, -grep("WIND_VELOCITY_EC1", colnames(cr1000))]
cr1000 <- cr1000[, -grep("PA_EC2_TOTAL", colnames(cr1000))]
#cr1000 <- cr1000[, -grep("WIND_VELOCITY_EC2", colnames(cr1000))]

head(cr1000)
str(cr1000)

#-----------------------------------------------------------------------------------------------------------------
## Plot data to ensure everything looks correct
library(ggplot2)
library(tidyr)
library(plotly)

# Create year & DOY variables
cr1000 <- cr1000 %>%
  mutate(year = as.numeric(format(Timestamp,'%Y')),
         DOY = as.numeric(format(Timestamp,'%j')))

cr1000$year <- factor(cr1000$year)

#-----------------------------------------------------------------------------------------------------------------
# Air temperature - full time series
plot_ly(data = cr1000, x = ~Timestamp, y = ~AIR_TEMP_38CM, name = '38 cm', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~AIR_TEMP_2M, name = '2 m', mode = 'lines')

# Air temperature - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = AIR_TEMP_2M, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

ggplot(data = cr1000, mapping = aes(x = DOY, y = AIR_TEMP_38CM, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# Oxidation, reduction potential - full time series
plot_ly(data = cr1000, x = ~Timestamp, y = ~ORP_10CM, name = '10 cm', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~ORP_30CM, name = '30 cm', mode = 'lines')

# ORP - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = ORP_10CM, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

ggplot(data = cr1000, mapping = aes(x = DOY, y = ORP_30CM, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# PAR - full time series
plot_ly(data = cr1000, x = ~Timestamp, y = ~INCOMING_PAR, name = 'Incoming', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~REFLECTED_PAR, name = 'Outgoing', mode = 'lines')

# PAR - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = INCOMING_PAR, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

ggplot(data = cr1000, mapping = aes(x = DOY, y = REFLECTED_PAR, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# PA - full time series
plot_ly(data = cr1000, x = ~Timestamp, y = ~PA_EC1, name = 'PA_EC1', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~PA_EC2_AIR, name = 'PA EC2', mode = 'lines') %>%
  add_trace(y = ~PA_2M, name = 'PA 2M', mode = 'lines')

# PA - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = INCOMING_PAR, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

ggplot(data = cr1000, mapping = aes(x = DOY, y = REFLECTED_PAR, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# PRECIP - full time series
plot_ly(data = cr1000, x = ~Timestamp, y = ~PRECIP, type = 'scatter', mode = 'lines') 

# PRECIP - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = PRECIP, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# SVWC - full time series - look into further using Johannes' code!
plot_ly(data = cr1000, x = ~Timestamp, y = ~SVWC, type = 'scatter', mode = 'lines') 

# SVWC - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = SVWC, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# WTH - full time series - look into further using Johannes' code!
plot_ly(data = cr1000, x = ~Timestamp, y = ~WTH, type = 'scatter', mode = 'lines') 

# WTH - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = WTH, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# SW - full time series
plot_ly(data = cr1000, x = ~Timestamp, y = ~SHORTWAVE_IN, name = 'Incoming', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~SHORTWAVE_OUT, name = 'Outgoing', mode = 'lines')

# SW - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = SHORTWAVE_IN, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

ggplot(data = cr1000, mapping = aes(x = DOY, y = SHORTWAVE_OUT, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# LW - full time series
plot_ly(data = cr1000, x = ~Timestamp, y = ~LONGWAVE_IN, name = 'Incoming', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~SHORTWAVE_OUT, name = 'LONGWAVE_OUT', mode = 'lines')

# LW - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = LONGWAVE_IN, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

ggplot(data = cr1000, mapping = aes(x = DOY, y = LONGWAVE_OUT, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# RH - full time series
plot_ly(data = cr1000, x = ~Timestamp, y = ~RH_2M, name = '2 m', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~RH_38CM, name = '38 cm', mode = 'lines')

# RH - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = RH_2M, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

ggplot(data = cr1000, mapping = aes(x = DOY, y = RH_38CM, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# SHFP - full time series
plot_ly(data = cr1000, x = ~Timestamp, y = ~SHFP_1, name = 'SHFP 1', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~SHFP_2, name = 'SHFP 2', mode = 'lines') %>%
  add_trace(y = ~SHFP_3, name = 'SHFP 3', mode = 'lines')

# SHFP - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = SHFP_1, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

ggplot(data = cr1000, mapping = aes(x = DOY, y = SHFP_2, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

ggplot(data = cr1000, mapping = aes(x = DOY, y = SHFP_3, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# TS - full time series
plot_ly(data = cr1000, x = ~Timestamp, y = ~SOIL_TEMP_5CM, name = '5 cm', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~SOIL_TEMP_10CM, name = '10 cm', mode = 'lines') %>%
  add_trace(y = ~SOIL_TEMP_50CM, name = '50 cm', mode = 'lines')

# TS - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = SOIL_TEMP_5CM, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

ggplot(data = cr1000, mapping = aes(x = DOY, y = SOIL_TEMP_10CM, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

ggplot(data = cr1000, mapping = aes(x = DOY, y = SOIL_TEMP_50CM, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# WATER TEMP - full time series
plot_ly(data = cr1000, x = ~Timestamp, y = ~WATER_TEMP_10CM, name = '10 cm', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~WATER_TEMP_30CM, name = '30 cm', mode = 'lines')

# TW - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = WATER_TEMP_10CM, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

ggplot(data = cr1000, mapping = aes(x = DOY, y = WATER_TEMP_30CM, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

#-----------------------------------------------------------------------------------------------------------------
# WS - full time series
# WATER TEMP - full time series
plot_ly(data = cr1000, x = ~Timestamp, y = ~WIND_VELOCITY_CUP, name = 'Cup', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~WIND_VELOCITY_EC2, name = 'EC 2', mode = 'lines')

# WS - by year
ggplot(data = cr1000, mapping = aes(x = DOY, y = WIND_VELOCITY_CUP, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

ggplot(data = cr1000, mapping = aes(x = DOY, y = WIND_VELOCITY_EC2, na.rm = TRUE)) +
  geom_line(aes(colour=year, group=year))

# Save output
write.csv(cr1000, paste('../met_merged/met_merged','.csv',sep=''),row.names=FALSE)


