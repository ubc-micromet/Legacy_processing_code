library(REddyProc)
library(tidyverse)
library(dplyr)

rm(list=ls())

#+++ Load data with 1 header and 1 unit row from (tab-delimited) text file
EddyData.F <- fLoadTXTIntoDataframe("./for_gapfilling.txt")


#+++ Add time stamp in POSIX time format
EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH',Year.s = 'Year',Day.s = 'DoY',Hour.s = 'Hour')

#+++ Initalize R5 reference class sEddyProc for post-processing of eddy data
#+++ with the variables needed for post-processing later
EddyProc.C <- sEddyProc$new('CA-BB', EddyDataWithPosix.F,
														c("NEE","CH4","LE","H","Rg","Tair","Tsoil5cm","Tsoil10cm","rH","VPD","Ustar","WTH" ))

# ustar distribution analysis
uStarTh <- EddyProc.C$sEstUstarThresholdDistribution(
	nSample = 200L, probs = c(0.05,0.5,0.95))
 write_csv(uStarTh, "./BB_uStarTh.csv")

 
 
# ustar threshold decision: whole data 5% is used.
uStarThAnnual <- usGetAnnualSeasonUStarMap(uStarTh)[-c(2,5)]
threshold <- uStarTh %>% filter(aggregationMode == "single") %>% select("5%","50%")
threshold <- as.numeric(threshold)
uStarThAnnual$U05 <- threshold[1]
uStarThAnnual$U50 <- threshold[2]
uStarSuffixes <- colnames(uStarThAnnual)[-1]

EddyProc.C$sMDSGapFillAfterUStarDistr(FluxVar.s = "NEE",
																			UstarThres.df = uStarThAnnual,
																			V1.s = "Rg", T1.n = 50,
																			V2.s = "VPD", T2.n = 5, V3.s = "Tair",
																			T3.n = 2.5,
																			UstarSuffix.V.s = uStarSuffixes,
																			FillAll = FALSE
)
EddyProc.C$sMDSGapFillAfterUStarDistr(FluxVar.s = "LE",
																			UstarThres.df = uStarThAnnual,
																			V1.s = "Rg", T1.n = 50,
																			V2.s = "VPD", T2.n = 5, V3.s = "Tair",
																			T3.n = 2.5,
																			UstarSuffix.V.s = uStarSuffixes,
																			FillAll = FALSE
)
EddyProc.C$sMDSGapFillAfterUStarDistr(FluxVar.s = "H",
																			UstarThres.df = uStarThAnnual,
																			V1.s = "Rg", T1.n = 50,
																			V2.s = "VPD", T2.n = 5, V3.s = "Tair",
																			T3.n = 2.5,
																			UstarSuffix.V.s = uStarSuffixes,
																			FillAll = FALSE
)
EddyProc.C$sMDSGapFillAfterUStarDistr(FluxVar.s = "CH4",
																			UstarThres.df = uStarThAnnual,
																			V1.s = "Tsoil10cm", T1.n = 2,
																			V2.s = "Tair", T2.n = 2.5, 
																			V3.s = "WTH",	T3.n = 1,
																			UstarSuffix.V.s = uStarSuffixes,
																			FillAll = FALSE
)

grep("NEE_.*_f$",names(EddyProc.C$sExportResults()), value = TRUE)
grep("LE_.*_f$",names(EddyProc.C$sExportResults()), value = TRUE)
grep("H_.*_f$",names(EddyProc.C$sExportResults()), value = TRUE)
grep("CH4_.*_f$",names(EddyProc.C$sExportResults()), value = TRUE)

FilledEddyData.F <- EddyProc.C$sExportResults()
names(FilledEddyData.F)


EddyProc.C$sSetLocationInfo(Lat_deg.n = 49.12933611, Long_deg.n = -122.98500000, TimeZone_h.n = -8)
EddyProc.C$sMDSGapFill('Tair', FillAll.b = FALSE)
EddyProc.C$sMDSGapFill('VPD', FillAll.b = FALSE)
EddyProc.C$sMDSGapFill('Rg', FillAll.b = FALSE)
EddyProc.C$sMDSGapFill('WTH', FillAll.b = FALSE)
EddyProc.C$sMDSGapFill('Tsoil10cm', FillAll.b = FALSE)
EddyProc.C$sMDSGapFill('Tsoil5cm', FillAll.b = FALSE)


FilledEddyData.F <- EddyProc.C$sExportResults()
names(FilledEddyData.F)

#night time method
resPart <- lapply(uStarSuffixes, function(suffix){
	EddyProc.C$sMRFluxPartition(Suffix.s = suffix)
})


#day time method
resPart <- lapply(uStarSuffixes, function(suffix){
	EddyProc.C$sGLFluxPartition(Suffix.s = suffix)
})


grep("GPP|Reco",names(EddyProc.C$sExportResults()), value = TRUE)

FilledEddyData.F <- EddyProc.C$sExportResults()
names(FilledEddyData.F)

write_csv(FilledEddyData.F,"./BB_REddyProc_gapfilled_fulloutput.csv")


######## ploting
par(mfrow=c(2,1))
EddyProc.C$sPlotFingerprintY('GPP_U50_f', Year.i = 2017)
EddyProc.C$sPlotFingerprintY('GPP_U50_f', Year.i = 2018)
EddyProc.C$sPlotFingerprintY('Reco_U50', Year.i = 2017)
EddyProc.C$sPlotFingerprintY('Reco_U50', Year.i = 2018)
EddyProc.C$sPlotFingerprintY('GPP_DT_U50', Year.i = 2017)
EddyProc.C$sPlotFingerprintY('GPP_DT_U50', Year.i = 2018)
EddyProc.C$sPlotFingerprintY('Reco_DT_U50', Year.i = 2017)
EddyProc.C$sPlotFingerprintY('Reco_DT_U50', Year.i = 2018)
EddyProc.C$sPlotFingerprintY('NEE_U50_f', Year.i = 2017)
EddyProc.C$sPlotFingerprintY('NEE_U50_f', Year.i = 2018)
EddyProc.C$sPlotFingerprintY('LE_U50_f', Year.i = 2017)
EddyProc.C$sPlotFingerprintY('LE_U50_f', Year.i = 2018)
EddyProc.C$sPlotFingerprintY('H_U50_f', Year.i = 2017)
EddyProc.C$sPlotFingerprintY('H_U50_f', Year.i = 2018)
EddyProc.C$sPlotFingerprintY('CH4_U50_f', Year.i = 2017)
EddyProc.C$sPlotFingerprintY('CH4_U50_f', Year.i = 2018)

# daily sums
EddyProc.C$sPlotDailySums(Var.s = 'CH4_U50_f', Format.s = "png", unit.s = "gC/m2/day")
EddyProc.C$sPlotDailySums(Var.s = 'LE_U50_f',Format.s = "png", unit.s = "gC/m2/day")
EddyProc.C$sPlotDailySums(Var.s = 'H_U50_f',Format.s = "png", unit.s = "gC/m2/day")
EddyProc.C$sPlotDailySums(Var.s = 'NEE_U50_f',Format.s = "png", unit.s = "gC/m2/day")
EddyProc.C$sPlotDailySums(Var.s = 'GPP_U50_f',Format.s = "png", unit.s = "gC/m2/day")
EddyProc.C$sPlotDailySums(Var.s = 'Reco_U50',Format.s = "png", unit.s = "gC/m2/day")
EddyProc.C$sPlotDailySums(Var.s = 'GPP_DT_U50',Format.s = "png", unit.s = "gC/m2/day")
EddyProc.C$sPlotDailySums(Var.s = 'Reco_DT_U50',Format.s = "png", unit.s = "gC/m2/day")

#Annual GPP-NT
GPPAggCO2 <- sapply( uStarSuffixes, function(suffix) {
	GPPHalfHour <- FilledEddyData.F[[paste0("GPP_",suffix,"_f")]]
	mean(GPPHalfHour[1:17568], na.rm = TRUE)
})
molarMass <- 12.011
GPPAgg <- GPPAggCO2 * 1e-6 * molarMass * 3600*24*365
print(GPPAgg)

#Annual RE-NT 
RecoAggCO2 <- sapply( uStarSuffixes, function(suffix) {
	RecoHalfHour <- FilledEddyData.F[[paste0("Reco_",suffix)]]
	mean(RecoHalfHour[1:17568], na.rm = TRUE)
})
molarMass <- 12.011
RecoAgg <- RecoAggCO2 * 1e-6 * molarMass * 3600*24*365
print(RecoAgg)

#Annual GPP-DT
GPPAggCO2 <- sapply( uStarSuffixes, function(suffix) {
	GPPHalfHour <- FilledEddyData.F[[paste0("GPP_DT_",suffix)]]
	mean(GPPHalfHour[1:17568], na.rm = TRUE)
})
molarMass <- 12.011
GPPAgg <- GPPAggCO2 * 1e-6 * molarMass * 3600*24*365
print(GPPAgg)

#Annual RE-DT 
RecoAggCO2 <- sapply( uStarSuffixes, function(suffix) {
	RecoHalfHour <- FilledEddyData.F[[paste0("Reco_DT_",suffix)]]
	mean(RecoHalfHour[1:17568], na.rm = TRUE)
})
molarMass <- 12.011
RecoAgg <- RecoAggCO2 * 1e-6 * molarMass * 3600*24*365
print(RecoAgg)

#Annual NEE
NEEAggCO2 <- sapply( uStarSuffixes, function(suffix) {
	NEEHalfHour <- FilledEddyData.F[[paste0("NEE_",suffix,"_f")]]
	mean(NEEHalfHour[1:17568], na.rm = TRUE)
})
molarMass <- 12.011
NEEAgg <- NEEAggCO2 * 1e-6 * molarMass * 3600*24*365
print(NEEAgg)

#NEE vs GPP+RE plot
par(mfrow=c(2,1))
plot(-FilledEddyData.F$GPP_U05_f+FilledEddyData.F$Reco_U05,FilledEddyData.F$NEE_U05_f,main="Nighttime method",
		 xlab="GPP+RE",ylab="NEE",xlim=c(-20,20),ylim=c(-20,20))
abline(0,1,col='grey',lty=2)

plot(-FilledEddyData.F$GPP_DT_U05+FilledEddyData.F$Reco_DT_U05,FilledEddyData.F$NEE_U05_f,main="Daytime method",
		 xlab="GPP+RE",ylab="NEE",xlim=c(-20,20),ylim=c(-20,20))
abline(0,1,col='grey',lty=2)


essential_variables <- grep("NEE.*_f$|GPP.*_f$|GPP_DT.*$|Reco|Reco_DT.*$|LE.*_f$|H.*_f$|CH4.*_f$",
														names(EddyProc.C$sExportResults()), value = TRUE)

essential_variables
(essential_variables <- essential_variables[-c(16,17,20,21)]) #SD data remove

essential <- FilledEddyData.F[,which(names(FilledEddyData.F) %in% essential_variables)]
essential2 <- cbind(EddyDataWithPosix.F, essential)
write_csv(essential2,"./BB_REddyProc_gapfilled_essential.csv")
