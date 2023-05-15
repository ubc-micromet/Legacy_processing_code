
library(readr)

#setwd('../Flux-tower/met_data/cr1000/')
setwd('G:/.shortcut-targets-by-id/1txCh9lZ7VGCujXGvBaJCMVnuxT-65q4K/Micromet Lab/Projects/2014-BB1 Burns Bog/Flux-tower (1)/met_data/cr1000') #Marion's wd

# Read the URL.
#url <- "http://ibis.geog.ubc.ca/~epicc/webdata/resources/csv/"
url <- "https://ibis.geog.ubc.ca/~micromet/webdata/resources/csv/" #Change/update URL so that it is getting the latest met data

# Define files to download
files <- c("BBDTA.csv", "BBORP.csv", "BBPAR.csv",
  "BBPCT.csv","BBRAD.csv","BBRHA.csv","BBSHA.csv","BBSMA.csv","BBSTA.csv","BBWPT.csv","BBWTA.csv","BBWVA.csv","BBPSA.csv")

# Loops through files to download
for(i in 1:length(files)){
  download.file(paste("https://ibis.geog.ubc.ca/~micromet/webdata/resources/csv/",files[i],sep=""), 
                destfile = paste(getwd(),"/",files[i],sep=""))
  }