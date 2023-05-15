############################################################################
# Burns Bog 2015
#
# Johannes Exler
# Graduate student
# Department of Geography 
# University of British Columbia
# 
# Email: j.exler@alumni.ubc.ca
#
# Flux tower 
# Quality check water table flux tower site
#
# Comments:
# - Quality check and gap infill for soil moisture data
# - Linearly gap fills short gaps up to 240 min
# - Assumes saturation during winter (Nov.1 - Apr.1) when no manual WT available (2014, 2015).
#   If manual WT available based on these measurements in concert with manual ground elevation.
# - Winter 2017 measured in hummock. Good data to compare hummock vs. hollow, 
#   but saturation needs to be assumed for hollow, thus measurements replaced.
#   Before winter 2017 SM sensor not moved, min. values ~87.5% during winter, but mostly missing. More realistic.
# - July 13, 2017 SM sensor moved back to hollow at 14:05.
# - Compare summer 2015 and 2016 to summer 2017. Latter mostly hummock moisture, 
#   which is approx. 10% and 15% drier than 2015 and 2016 respectively
# - Drop around Jan. 15, 2017 due to ice. See field notes and Hg.
############################################################################
rm(list = ls())  


### Packages ###
library(xts)
library(lubridate)



### Set work directory and load input files, prepare data ###
setwd("E:/University/PhD/BurnsBog/FieldData/FluxTower/Codes/Processing")
download.path <- "E:/University/PhD/BurnsBog/FieldData/FluxTower/Data/InputData/BurnsBogTower/"
exp.path <- "E:/University/PhD/BurnsBog/FieldData/FluxTower/Data/OutputData/SoilMoisture/"
correct.path <- "E:/University/PhD/BurnsBog/FieldData/FluxTower/Data/InputData/FluxParameter/"
imp.path <- "E:/University/PhD/BurnsBog/FieldData/FluxTower/Data/OutputData/"


# Download flux tower data from the internet
#URL <- "http://ibis.geog.ubc.ca/~epicc/webdata/resources/csv/BBSMA.csv"
#download.file(URL, destfile = paste0(download.path,"BBFluxSoilMoist.csv"))



### Options ###
# PLAUSIBILITY TEST: if plaus = T test will be executed. Define upper and lower limit for dry/wet period.
# Required result to keep data: TRUE
plaus <- T
plaus.del <- T        	# T if not plausible data should be replaced by NA.
plaus.min <- 60  	# Water vol.SM  [%]
plaus.max <- 95   



# CONSISTENCY TEST: if cons.sd/cons.thres = T test will be executed. Test uses standard deviation
# from sd.period of prior values or a threshold to the one prior value. Define for dry/wet period. 
# Required result to keep data: TRUE
cons.del <- T   # T if not consistent data should be replaced by NA.
cons.thres <- T    		# Alternatively the data set can be tested by applying a threshold [%]
const.threshold <- 10	# Threshold in [%]


# STABILITY TEST: if stab = T test will be executed. Define min. variation and 
# time period within which stability is supposed to be tested. Define for dry/wet period.
# Required result to keep data: FALSE
stab <- T
stab.del <- T			# Delete stable data?
stab.min <- 0.5			# Threshold in [%]
stab.time <- 48 		# Period [h]


# Gap filling
na.thres <- 2880				# Max length for linear interpolation unit: mins

# Data aggregation
# max.na.day <- 6				# If more than max.na.day values missing per day mean WT for day is NA (6 = 6h)
# max.na.month <- 6			# See max.na.day, evaluated for missing values aggregated daily values -> unit: days
month.day <- 15				# Day of month for monthly data date

# Statistics 
hyd.yr.0 <-	format(ISOdatetime(2015, 10, 01, 00, 00, 00, tz = "Etc/GMT+8"), format = "%m-%d %H:%M", tz = "Etc/GMT+8")		# Start of hydrologic year
hyd.yr.0.5 <- format(ISOdatetime(2015, 4, 01, 00, 00, 00, tz = "Etc/GMT+8"), format = "%m-%d %H:%M", tz = "Etc/GMT+8") 		# Split of hydrologic year

# Yes == T
exp <- T			# Export data?
agg.daily <- T 		# Aggregate to daily means?
agg.monthly <- T	# Aggregate to monthly means?
exp.gap <- T		# Export gap overview?
exp.stat <- T

# Export
exp.SM.30min.data <- "SMFilled30min.csv"
exp.SM.hour.data <- "SMFilledHour.csv"
exp.SM.day.data <- "SMFilledDay.csv"
exp.SM.month.data <- "SMFilledMonth.csv"
exp.SM.gap <- "SMGap30min.csv"
exp.SM.gap.portion <- "SMGapPortion30min.csv"
exp.SM.stats <- "SMStatsDay.csv"
exp.SM_WTdepth <- "SM_WTdepth.pdf"





#---------------------------------------------------------------------------
# Start code
#---------------------------------------------------------------------------  
# Load data set
dat <- read.csv(paste0(download.path, "BBFluxSoilMoist_QC.csv"))  # "BBFluxSoilMoist.csv" if not quality checked by BBFluxEbalWbal_QC.r code
colnames(dat) <- c("Date", "vol.SM")
dat$Date <- as.POSIXct(dat$Date, format = "%Y-%m-%d %H:%M", tz = "Etc/GMT+8")
int <- as.numeric(dat$Date[2] - dat$Date[1], unit = "mins")


# Load manual corrections 
correct <- read.csv(paste0(correct.path, "SMcorrect.csv"))
correct$StartDate <- as.POSIXct(as.character(correct$StartDate), format = "%Y-%m-%d %H:%M", tz = "Etc/GMT+8")
correct$EndDate <- as.POSIXct(as.character(correct$StartDate), format = "%Y-%m-%d %H:%M", tz = "Etc/GMT+8")



###--- wt ---###
# Manual data
wt <- read.csv(paste0(imp.path, "Groundwater/BBFluxGW.csv"))
wt$Date <- as.POSIXct(wt$Date, format = "%Y-%m-%d", tz = "Etc/GMT+8")
wt <- wt[, c("Date", "Cwell", "Cground")]
colnames(wt) <- c("Date", "WTheight", "Cground")
wt$Cground <- wt$Cground - 0.055  # Offset ground elevation, as located on hummock, to get hollow elevation
wt$WTground <- wt$WTheight - wt$Cground





#---------------------------------------------------------------------------
# Quality checks
#---------------------------------------------------------------------------  
### Plausibility ###  
if (plaus == T){
	plaus.min <- plaus.min
	plaus.max <- plaus.max
	
  dat$Plausibility <- rep(NA, length(dat$vol.SM))
  
  for (i in 1: length(dat$Date)) {
      dat$Plausibility[i] <- (dat$vol.SM[i] >= plaus.min) & (dat$vol.SM[i] <= plaus.max)
  }
}



### Consistency test ###
if (cons.thres == T){
	dat$Consistency <- rep(NA, length(dat$vol.SM))
	diff <- dat$vol.SM[1:(length(dat$vol.SM)-1)] - dat$vol.SM[2:(length(dat$vol.SM))]
      
	# Query and flag
	dat$Consistency <- c(abs(diff) < const.threshold, NA)
	}
    
    
    
### Stability test ###
if (stab == T){
	Stability <- c()
      
	# First values
	Stability[1: (stab.time * 2)] <-abs(max(dat$vol.SM[1:stab.time], na.rm = T)-min(dat$vol.SM[1:stab.time], na.rm = T))
      
	# Loop for remaining data set
	for (i in (stab.time+1):length(dat$vol.SM)){
        
		# Check if stab.time only NA or not
        stab.period <- dat$vol.SM[(i-(stab.time-1)):(i)]
        stab.test <- length(which(is.na(stab.period) == F))
        
        Stability[i] <- if(stab.test > 2) {
          
        abs(max(stab.period, na.rm = T)-min(stab.period, na.rm = T))
          
        } else {
          stab.min + 1	# Since no information is available, we assume no stability
        }
      }
      
      # Query and flag
      dat$Stability <- (Stability < stab.min)
      
}
    
  
  
  
### Correct data set if quality standards are not met ###
# Data NA, if not plausible.
if (plaus.del == T) {
    dat$vol.SM <- ifelse(dat$Plausibility == F, NA, dat$vol.SM)
}
  
  
# Data NA, if not consistent.
if (cons.del == T) {
	dat$vol.SM <- ifelse(dat$Consistency == F, NA, dat$vol.SM)
}
  
  
# Data NA, if stable.
if (stab.del == T) {
	dat$vol.SM <- ifelse(dat$Stability == T, NA, dat$vol.SM)
}
  
  
### Manual correction ###
x <- which(dat$Date %in% correct$StartDate)
dat$vol.SM[x] <- NA





#---------------------------------------------------------------------------
# Find data gaps
#--------------------------------------------------------------------------- 
x <- which(!is.na(dat$vol.SM))
x <- dat[x,]
# Gap position
y <- difftime(x$Date[2:nrow(x)], x$Date[1:(nrow(x)-1)], units = "mins")
y <- c(NA, y)  # Add NA, as first value cannot be evaluated
pos.gap <- which(as.numeric(y) > int) - 1
   
   
t1.date <- x$Date[pos.gap]
t2.date <- x$Date[pos.gap + 1]
   
t1 <- which(dat$Date %in% t1.date)
t2 <- which(dat$Date %in% t2.date)

leng.time <- as.numeric(t2.date - t1.date, units = "mins")
leng.val <- (t2 - t1) - 1
   
gap <- data.frame(t1.date, t2.date, leng.time, t1, t2, leng.val)

 
# Add gap when first value NA
test.na <- min(which(!is.na(dat$vol.SM)))
test.na <- test.na == 1
   
if(!test.na) {
    t1 <- 1
    t1.date <- dat$Date[t1]
    t2 <- which(!is.na(dat$vol.SM))[1]
    t2.date <- dat$Date[t2]
   
	leng.time <- as.numeric(t2.date - t1.date, units = "mins")
	leng.val <- (t2 - t1) - 1

    new.row <- data.frame(t1.date, t2.date, leng.time, t1, t2, leng.val)
    gap <- rbind(new.row, gap)
}

 
# Add gap up to last predicted value
test.na <- max(which(!is.na(dat$vol.SM)))
test.na <- test.na == nrow(dat)
   
if(!test.na) {
    t1 <- max(which(!is.na(dat$vol.SM)))
    t1.date <- dat$Date[t1]
    t2 <- nrow(dat)
    t2.date <- dat$Date[nrow(dat)]
    
	leng.time <- as.numeric(t2.date - t1.date, units = "mins")
	leng.val <- (t2 - t1) - 1

    new.row <- data.frame(t1.date, t2.date, leng.time, t1, t2, leng.val)
    gap <- rbind(gap, new.row)
}
colnames(gap) <- c("StartDate", "EndDate", "Lengthmin", "StartPos", "EndPos", "Length")  
  
  
  
  
  
#---------------------------------------------------------------------------
# vol.SM infill short gaps 
#---------------------------------------------------------------------------  
# Linear interpolation, short gaps
dat$vol.SM.fill <- dat$vol.SM
na.test <- which(gap$Lengthmin <= na.thres)

if(!(nrow(dat) %in% gap$EndPos)){  
	na.test.length <- length(na.test)
	} else {
	na.test.length <- length(na.test) - 1
	}
	
for(bb in 1:na.test.length){

	leng <- gap$Length[na.test[bb]]
	x1 <- gap$StartPos[na.test[bb]]
	x2 <- gap$EndPos[na.test[bb]]

	# Values
	y1 <- dat$vol.SM[x1]
	y2 <- dat$vol.SM[x2]

	# Infill
	z <- (y2 - y1) / (leng + 1)
	z <- seq(y1, y2, by = z)
	dat$vol.SM[x1:x2] <- dat$vol.SM.fill[x1:x2] <- round(z, digits = 2)

# End for(bb in 1:length(na.test))
}  





#---------------------------------------------------------------------------
# vol.SM infill winter time gaps (see comments at beginning for details)
#---------------------------------------------------------------------------  
###--- Calculate saturation as mean of max. 1% soil moisture on record ---###
o <- max(dat$vol.SM.fill, na.rm = T) - min(dat$vol.SM.fill, na.rm = T)
o <- max(dat$vol.SM.fill, na.rm = T) - 0.01 * o  # Threshold above which max. 1% of SM on record
theta.sat <- mean(dat$vol.SM.fill[which(dat$vol.SM.fill > o)], na.rm = T)  # Calculates mean of max. 1% of SM on record, assumed to be saturation 



### Gap fill wintertime years of 2014 and 2015 until summer 2016 ###
# Based on time of year, as no WT available
m <- is.na(dat$vol.SM.fill) == T & month(dat$Date) >= 11 & year(dat$Date) %in% c(2014, 2015)
n <- is.na(dat$vol.SM.fill) == T & month(dat$Date) < 4 & year(dat$Date) %in% c(2014, 2015, 2016)
dat$vol.SM.fill <- ifelse(m | n, theta.sat, dat$vol.SM.fill)



### Gap fill wintertime starting fall 2016 ###
# Based on WT. Manual measurements linearly interpolated for approximation
# Merge data 
dat <- merge(dat, wt, all.x = T)



###--- Linearly interpolate manual measurements ---###
m <- which(!is.na(wt$WTground))[1]
m <- ymd_hms(paste(wt$Date[m], "00:00:00"), tz = "Etc/GMT+8")
m <- which(dat$Date == m)

n <- which(!is.na(wt$WTground))[length(which(!is.na(wt$WTground)))]
n <- ymd_hms(paste(wt$Date[n], "00:00:00"), tz = "Etc/GMT+8")
n <- which(dat$Date == n)

dat$WTground[m:n] <- na.approx(dat$WTground[m:n], rule = 2)
dat$WTheight[m:n] <- na.approx(dat$WTheight[m:n], rule = 2)
dat$Cground[m:n] <- na.approx(dat$Cground[m:n], rule = 2)


# Gap fill
start.date <- which(!is.na(wt$WTground))[1]
start.date <- ymd_hms(paste(wt$Date[start.date], "00:00:00"), tz = "Etc/GMT+8")
dat$vol.SM.fill <- ifelse(dat$Date > start.date & dat$WTground >= 0 & !is.na(dat$WTground), theta.sat, dat$vol.SM.fill)





#---------------------------------------------------------------------------
# Plot soil moisture, WT height, ground elevation and WT depth
#---------------------------------------------------------------------------  
# Export data
x <- paste0(exp.path, exp.SM_WTdepth)
pdf(x, height = 8.5, width = 11)

# Plot options
par(mfrow = c(3,1))
par(mar = c(1, 5, 1, 4))


### Soil moisture ###
plot(dat$Date, dat$vol.SM.fill,
     type = "l",
     xaxt = "n",
     xlab = "",
     ylab = "Vol. SM (%)",
	 ylim = c(60, 100),
	 lwd = 2,
	 cex.lab = 1.5,
     cex.axis = 1.3
)

# Add dates when SM sensor was moved
#m <- which(dat$Date == ymd("2016-09-29"))
#points(dat$Date[m], dat$vol.SM.fill[m], pch = 15, col = "gray", cex = 2)

#m <- which(dat$Date == ymd("2017-07-13"))
#points(dat$Date[m], dat$vol.SM.fill[m], pch = 15, col = "gray", cex = 2)

# Add x axis
n <- seq(dat$Date[1], dat$Date[length(dat$Date)], by = "month")
axis(1, dat$Date, at = n, 
     labels = rep("",length(n)),
     cex.lab = 1.5, cex.axis = 1.3) 


### Ground level ###
plot(dat$Date, dat$Cground,
     type = "l",
	 xaxt = "n",
	 yaxt = "n",
     xlab = "",
     ylab = "Elevation (m)",
     col = "black",
	 ylim = c(3.15, 3.5),
     lwd = 2,
	 cex.lab = 1.5,
     cex.axis = 1.3
)

# Add water table height
lines(dat$Date, dat$WTheight, lwd = 2, col = "gray")

# Add axis for ground level
axis(4, at = seq(3.15, 3.5, by = 0.05), cex.axis = 1.3)
axis(2, at = seq(3.15, 3.5, by = 0.05), cex.axis = 1.3)

# Add x axis
n <- seq(dat$Date[1], dat$Date[length(dat$Date)], by = "month")
axis(1, dat$Date, at = n, 
     labels = rep("",length(n)),
     cex.lab = 1.5, cex.axis = 1.3) 
	 
	 
legend("topleft",
	legend = c("WT height", "Ground level"),
	lwd = c(2, 2),
	col = c("gray", "black"),
	bty = "n",
	cex = 2
	)
	 
	 
### WT depth ###
par(mar = c(4, 5, 1, 4))
plot(dat$Date, dat$WTground,
     type = "l",
     xaxt = "n",
	 yaxt = "n",
     xlab = "",
     ylab = "WT depth (m)",
     lwd = 2,
	 cex.lab = 1.5,
     cex.axis = 1.3
)
abline(h = 0, lty = 2)

# Add axis for water table depth
axis(4, at = seq(-0.065, 0.025, by = 0.01), cex.axis = 1.3)
axis(2, at = seq(-0.065, 0.025, by = 0.01), cex.axis = 1.3)


# Add x axis
n <- seq(dat$Date[1], dat$Date[length(dat$Date)], by = "month")
axis(1, dat$Date, at = n, 
     labels = rep("",length(n)),
     cex.lab = 1.5, cex.axis = 1.3) 
	 
m <- seq(dat$Date[1], dat$Date[length(dat$Date)], by = "quarter")
axis(1, dat$Date, at = m, 
     labels = as.character(m, format = "%b %y"),
     cex.lab = 1.5, cex.axis = 1.3) 

# End .pdf
dev.off()	




### Prepare output data frame ###
dat <- data.frame(dat$Date, dat$vol.SM, dat$vol.SM.fill)
colnames(dat) <- c("Date", "vol.SM", "vol.SM_fill")  
dat.30min <- dat



###--- Calculate gap portion of data set ---###
v <- c("Missing before", "Missing after", "Total values", "Ratio before [%]", "Ratio after [%]")
x.bef <- round(sum(gap$Length, na.rm = T))
x.aft <- length(which(is.na(dat$vol.SM)))
y <- round(nrow(dat))
z.bef <- round((x.bef/y) * 100)
z.aft <- round((x.aft/y) * 100)
gap.portion <- data.frame(v, c(x.bef, x.aft, y, z.bef, z.aft))
colnames(gap.portion) <- c("","")




  
#---------------------------------------------------------------------------
# Aggregate data
#--------------------------------------------------------------------------- 
### SM aggregate to hourly mean ###
for (aa in 2:ncol(dat)) {
  o <- xts(dat[,aa], dat[,"Date"])
  end <- endpoints(o, "hours", 1) 
  
  o <- period.apply(o, end, mean, na.rm = T)
  o <- data.frame(Date=index(o), coredata(o))      # Convert xts back to data frame
  colnames(o) <- c("Date", "vol.SM")
  
  # Round time to hour
  if (aa == 2) {
    dat.hour <- o
    dat.hour$Date <- format(strptime("1970-01-01", "%Y-%m-%d", tz="Etc/GMT+8") + floor(as.numeric(dat.hour$Date)/3600)*3600,"%Y-%m-%d %H:%M:%S", tz="Etc/GMT+8")
    dat.hour$Date <- as.POSIXct(dat.hour$Date, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+8")
  } else {
  dat.hour<- cbind(dat.hour, o$vol.SM)
  }
}
dat.hour[, -1] <- round(dat.hour[, -1], digits = 2)
colnames(dat.hour) <- c("Date", "vol.SM", "vol.SM_fill")


### SM aggregate to daily mean ###
if(agg.daily == T)  {
for (aa in 2:ncol(dat.hour)) {
  o <- xts(dat.hour[, aa], dat.hour[,"Date"])
  end <- endpoints(o, "days", 1) 
  
  o <- period.apply(o, end, mean, na.rm = T)
  o <- data.frame(Date=index(o), coredata(o))      # Convert xts back to data frame
  colnames(o) <- c("Date", "vol.SM")
  
	
	  if (aa == 2) {
		dat.day <- o
	   dat.day$Date <- as.POSIXct(strftime(dat.day$Date, format = "%Y-%m-%d", tz = "Etc/GMT+8"), tz = "Etc/GMT+8") # Remove time from Date
	  } else {
	  dat.day <- cbind(dat.day, o$vol.SM)
	  }
	 }
	dat.day[, -1] <- round(dat.day[, -1], digits = 2)
	colnames(dat.day) <- c("Date", "vol.SM", "vol.SM_fill")
   
}
 

### SM aggregate to monthly mean ###
if(agg.monthly == T)  {
for (aa in 2:ncol(dat.day)) {
  o <- xts(dat.day[, aa], dat.day[,"Date"])
  end <- endpoints(o, "months", 1) 
  
  o <- period.apply(o, end, mean, na.rm = T)
  o <- data.frame(Date=index(o), coredata(o))      # Convert xts back to data frame
  colnames(o) <- c("Date", "vol.SM")
  
	
	  if (aa == 2) {
		dat.month <- o
		  date.temp <- as.character(dat.month$Date)
			substr(date.temp, 9, 10) <- as.character(month.day)
			dat.month$Date <- as.POSIXct(date.temp, format = "%Y-%m-%d", tz = "Etc/GMT+8")
	  } else {
	  dat.month <- cbind(dat.month, o$vol.SM)
	  }
	 }
	dat.month[, -1] <- round(dat.month[, -1], digits = 2)
	colnames(dat.month) <- c("Date", "vol.SM", "vol.SM_fill")
   
}





#---------------------------------------------------------------------------
# SoilMoist summary statistics
#---------------------------------------------------------------------------
dat <- dat.day
name <- colnames(dat)[-1]


# Create vector with split dates for water year
x <- sort(c(1, which(format(dat$Date, format = "%m-%d %H:%M", tz = "Etc/GMT+8") == hyd.yr.0), which(format(dat$Date, format = "%m-%d %H:%M", tz = "Etc/GMT+8") == hyd.yr.0.5)))
y <- as.numeric(unique(format(dat$Date, format = "%Y")))


stats <- data.frame(Water.year = NA, Min = NA, Max = NA, Mean = NA, Sum = NA, Sd = NA)
colnames(stats) <- c("Water year", "minvol.SM", "maxvol.SM", "meanvol.SM", "sumvol.SM", "sdvol.SM")
for(bb in 1:length(x)){
	if(bb < length(x)){
		dat.temp <- dat[x[bb]:x[bb+1],]
		date.temp <- paste(format(dat$Date[c(x[bb], x[bb+1])], format = "%b.%y"), collapse = " - ")
	} else {
		dat.temp <- dat[x[bb]:nrow(dat),]
		date.temp <- paste(format(dat$Date[c(x[bb], nrow(dat))], format = "%b.%y"), collapse = " - ")
	}

	dat.min <- round(min(dat.temp[,name], na.rm = T), digits = 2)
	dat.max <- round(max(dat.temp[,name], na.rm = T), digits = 2)
	dat.mean <- round(mean(dat.temp[,name], na.rm = T), digits = 2)
	dat.sum <- round(sum(dat.temp[,name], na.rm = T), digits = 2)
	dat.sd <- round(sd(dat.temp[,name], na.rm = T), digits = 2)
	
	stats <- rbind(stats, c(date.temp, dat.min, dat.max, dat.mean, dat.sum, dat.sd))
	
	
}
stats <- stats[-1,]
  
#---------------------------------------------------------------------------
# Plot
#---------------------------------------------------------------------------
#plot(dat$Date, dat$vol.SM, 
#	type = "p",
#	xlab = "Date",
#	ylab = "vol.SM [mm]",
#	ylim = c(plaus.min, plaus.max)
#)

#abline(h = 0, lwd = 2)
#abline(h = plaus.min, lty = 2)
#abline(h = plaus.max, lty = 2)

  
#text(x = dat$Date[600], y = 120, 
#	labels = "Soil surface", cex = 0.8)
  
#lines(dat$Date, dat$vol.SM,
#	col = "green",
#	lwd = 2,
#	lend = "butt"
#)
  
#legend("bottomright",
#	legend = c("raw", "approved", "limit"),
#	pch = c(1, NA, NA),
#	lty = c(NA, 1, 2),
#	col = c("black", "green", "black"), 
#	cex = 0.6
#)
  
  
  
### Export flagged data sets ###
if(exp == T){
	write.csv(dat.30min, paste0(exp.path, exp.SM.30min.data), row.names = F)
	write.csv(dat.hour, paste0(exp.path, exp.SM.hour.data), row.names = F)
	write.csv(dat.day, paste0(exp.path, exp.SM.day.data), row.names = F)
	write.csv(dat.month, paste0(exp.path, exp.SM.month.data), row.names = F)
}

if(exp.gap == T) {
write.csv(gap, paste0(exp.path, exp.SM.gap), row.names = F)
write.csv(gap.portion, paste0(exp.path, exp.SM.gap.portion), row.names = F)
}

if(exp.stat == T) {
	write.csv(stats, paste0(exp.path, exp.SM.stats), row.names = F)
}