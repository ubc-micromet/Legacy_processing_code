library(REddyProc)
library(tidyverse)
library(dplyr)

rm(list=ls())


# Load data ---------------------------------------------------------------

EddyData.F <- fLoadTXTIntoDataframe("./Flux-tower/flux_data/REddyProc_input/for_gap_filling_partitioning.txt")


# Add time stamp in POSIX time format -------------------------------------
EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH',Year.s = 'Year',Day.s = 'DoY',Hour.s = 'Hour')


# Initalize R5 reference class sEddyProc for post-processing of eddy data with the variables needed for post-processing later -------------------------------------
EddyProc.C <- sEddyProc$new('CA-BB', EddyDataWithPosix.F,
														c("NEE","CH4","LE","H","Rg","Tair","Tsoil5cm","Tsoil10cm","rH","VPD","Ustar","WTH"))


# Use MDS for gap-filling NEE, H, and LE ----------------------------------

EddyProc.C$sMDSGapFill('NEE', FillAll.b = TRUE)
EddyProc.C$sMDSGapFill('LE', FillAll.b = TRUE)
EddyProc.C$sMDSGapFill('H', FillAll.b = TRUE)

# ADD CH4 gap-filling!

# Gap-filling met data for partitioning  ----------------------------------

EddyProc.C$sSetLocationInfo(Lat_deg.n = 49.12933611, Long_deg.n = -122.98500000, TimeZone_h.n = -8)
EddyProc.C$sMDSGapFill('Tair', FillAll.b = FALSE)
EddyProc.C$sMDSGapFill('VPD', FillAll.b = FALSE)
EddyProc.C$sMDSGapFill('Rg', FillAll.b = FALSE)
EddyProc.C$sMDSGapFill('WTH', FillAll.b = FALSE)
EddyProc.C$sMDSGapFill('Tsoil10cm', FillAll.b = FALSE)
EddyProc.C$sMDSGapFill('Tsoil5cm', FillAll.b = FALSE)

# Apply nighttime gap-filling algorithm -----------------------------------

EddyProc.C$sMRFluxPartition()

# Apply daytime gap-filling algorithm -----------------------------------
EddyProc.C$sGLFluxPartition()


# Output results -----------------------------------

FilledEddyData.F <- EddyProc.C$sExportResults()
names(FilledEddyData.F)


# Plot results -----------------------------------
par(mfrow=c(2,1))
EddyProc.C$sPlotFingerprintY('NEE_f', Year.i = 2018)
EddyProc.C$sPlotFingerprintY('NEE_f', Year.i = 2019)
EddyProc.C$sPlotFingerprintY('LE_f', Year.i = 2018)
EddyProc.C$sPlotFingerprintY('LE_f', Year.i = 2019)
EddyProc.C$sPlotFingerprintY('H_f', Year.i = 2018)
EddyProc.C$sPlotFingerprintY('H_f', Year.i = 2019)
EddyProc.C$sPlotFingerprintY('GPP_f', Year.i = 2017)
EddyProc.C$sPlotFingerprintY('GPP_f', Year.i = 2018)
EddyProc.C$sPlotFingerprintY('Reco', Year.i = 2017)
EddyProc.C$sPlotFingerprintY('Reco', Year.i = 2018)
EddyProc.C$sPlotFingerprintY('GPP_DT', Year.i = 2017)
EddyProc.C$sPlotFingerprintY('GPP_DT', Year.i = 2018)
EddyProc.C$sPlotFingerprintY('Reco_DT', Year.i = 2017)
EddyProc.C$sPlotFingerprintY('Reco_DT', Year.i = 2018)


# daily sums - CHECK & FIX UNITS!
setwd("./Flux-tower/flux_data")
EddyProc.C$sPlotDailySums(Var.s = 'LE_f',Format.s = "png", unit.s = "W/m2")
EddyProc.C$sPlotDailySums(Var.s = 'H_f',Format.s = "png", unit.s = "W/m2")
EddyProc.C$sPlotDailySums(Var.s = 'NEE_f',Format.s = "png", unit.s = "gC/m2/day")
EddyProc.C$sPlotDailySums(Var.s = 'GPP_f',Format.s = "png", unit.s = "gC/m2/day")
EddyProc.C$sPlotDailySums(Var.s = 'Reco',Format.s = "png", unit.s = "gC/m2/day")
EddyProc.C$sPlotDailySums(Var.s = 'GPP_DT',Format.s = "png", unit.s = "gC/m2/day")
EddyProc.C$sPlotDailySums(Var.s = 'Reco_DT',Format.s = "png", unit.s = "gC/m2/day")


# Save full REddyPro output -----------------------------------------------

write_csv(FilledEddyData.F,"./BB_REddyProc_gapfilled_partition_fulloutput.csv")


# Save only essential variables -------------------------------------------

essential_variables <- grep("NEE_f$|GPP_f$|GPP_DT.*$|Reco|Reco_DT.*$|LE_f$|H_f$|CH4_f$",
														names(EddyProc.C$sExportResults()), value = TRUE)
essential_variables

# Remove WTH
essential_variables <- str_remove(essential_variables, "WTH_.*")
essential_variables

essential <- FilledEddyData.F[,which(names(FilledEddyData.F) %in% essential_variables)]


# Combine essential variables with L2 data and save output --------------------------------
L2 <- read.csv(paste("./BB_L2",".csv",sep = ""),sep=",",header=TRUE,dec=".")  

head(L2)
head(essential)

# Check to make sure files are of the same length and if TRUE, append to L2
if (nrow(L2) == nrow(essential)){
  # If TRUE, Combine
  L3 <- cbind(L2,essential)
  write_csv(L3,"./BB_L3.csv")
}





