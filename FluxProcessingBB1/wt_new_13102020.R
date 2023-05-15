rm(list = ls())


### Packages 
library(tidyverse)
library(lubridate)


### Set directories 
#setwd("D:/University/PhD/BurnsBog/FieldData/FluxTower/WTBB1_MarionNyberg/")
setwd("/Users/marionnyberg/Google\ Drive/Micromet\ Lab/People/2019-Marion\ Nyberg/WTH_test")

#imp.path <- "D:/University/PhD/BurnsBog/FieldData/FluxTower/WTBB1_MarionNyberg/"
imp.path<- "/Users/marionnyberg/Google\ Drive/Micromet\ Lab/People/2019-Marion\ Nyberg/WTH_test"

#exp.path <- "D:/University/PhD/BurnsBog/FieldData/FluxTower/WTBB1_MarionNyberg/"
exp.path <- "/Users/marionnyberg/Google\ Drive/Micromet\ Lab/People/2019-Marion\ Nyberg/WTH_test"

### Export
exp.WT.ref <- "BB_WT_BB1_georef.csv"
exp.lm.metric <- "BB_WT_BB1_lmMetric.csv"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data import 
# - Loads automated pressure transducer and bog surface elevation data 
#   along with manual water table elevation and bog surface elevation data 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define constants
w.top <- 4.473 # Central well top elevation from GNSS survey in m a.s.l.
w.top <- 4.606
wt_man <- w.top - 1.21

# Load automated data
dat.auto <- read.csv(paste0(imp.path, "/BBWPT.csv")) %>% 
  rename(Date = Time..PST.,
         WT_auto = Water.Table.Height...1.00m.,
         BH_auto = Bog.Height..1.00m.) %>%
  mutate(Date = ymd_hms(Date, tz = "Etc/GMT-8"), # tz without DST as used by BB1
         WT_auto = WT_auto/100,  # Convert cm to m
         BH_auto = BH_auto/100  
  )

# Load manual WT data
dat.WT.man <- read.csv(paste0(imp.path, "/WT.csv")) %>%
  rename(Date = DATE) %>%
  select(!matches("WT_PT")) %>%
  mutate(Date = round_date(ymd_hm(Date, tz = "America/Los_Angeles"), "30 minutes")) %>% # tz with DST for manual measurements
  mutate(WT_man_gref = w.top - (WT_man/100))  %>% # Geo-references manual measurements. Converts cm to m
  left_join(y = dat.auto[, c("Date", "WT_auto")], by = "Date")  # Merge manual and automated observations

###--- Geo-reference automated measurements
# - Uses lm()-offset to calculate offset to be applied to automated
#   measurements for geo-referencing elevation
# - Uses lm()-slope to calibrate automated measurements
###---

lm.metrics <- data.frame()



### lm() WT 
###
mod.lm <- lm("WT_man_gref ~ WT_auto", na.action = na.exclude , data = dat.WT.man)

y.pred <- predict.lm(mod.lm, newdata = dat.WT.man)  # Predict data for residual

# Calculate target functions
res <- dat.WT.man[, "WT_man_gref"] - y.pred

mbe <- mean(res, na.rm = T)
rmse <- sqrt(mean(res^2, na.rm = T))
mae <- mean(abs(res), na.rm = T)

adj.R2 <- round(summary(mod.lm)$adj.r.squared, digits = 4)
R2 <- round(summary(mod.lm)$r.squared, digits = 4)
mbe <- round(mbe, digits = 4)
rmse <- round(rmse, digits = 4)
mae <- round(mae, digits = 4)
intercept.wt <- intercept <- round(coef(mod.lm)[1], digits = 4)
slope.wt <- slope <- round(coef(mod.lm)[2], digits = 4)

# Sample size
size <- length(which(!is.na(res)))

# Merge in one row to be added to output df
Model <- "WT_auto correction"

o <- data.frame(Model, mbe, rmse, mae, R2, adj.R2, intercept, slope, size)
lm.metrics <- rbind(lm.metrics, o)


# Plot
dat.WT.man %>%
  ggplot()+
  xlab(bquote(WT[auto])) +
  ylab(bquote(WT[man_gref])) +
  coord_fixed() +
  geom_abline(intercept = intercept.wt, slope = slope.wt, col = "red", size = 1.2) +
  geom_point(aes(x = WT_auto, y = WT_man_gref)) +
  theme_bw()


### Apply correction 
# - Applies correction to automated data
###
# Apply correction to automated WT data
# Define start and end of correction period
start <- ymd_hms("2018-08-09 04:00:00", tz = "Etc/GMT-8")


# If you want to keep measurements from before "start", you need to change these lines
dat.auto <- dat.auto %>% filter(Date >= start) %>%
  mutate(WT_auto_gref = (WT_auto * slope.wt) + intercept.wt) 


dat.auto <- dat.auto %>% 
  mutate(WTD_auto_gref_offset = WT_auto_gref + (23.3/100))
ggplot(dat.auto, aes(Date, WTD_auto_gref_offset)) + geom_line() + ylab("WT (m) with an offset of 23.3cm")

dat.auto <- dat.auto %>% 
  mutate(WTD_marion = (WT_auto_gref - wt_man))
ggplot(dat.auto, aes(Date, WTD_marion)) + geom_line() + ylab("")

