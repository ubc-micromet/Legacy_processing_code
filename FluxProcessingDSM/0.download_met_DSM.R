
library(readr)

rdir <-"/Users/ziyi/Google Drive/.shortcut-targets-by-id/1txCh9lZ7VGCujXGvBaJCMVnuxT-65q4K/Micromet Lab/Projects/2021-DSM Delta salt marsh/Flux-tower/met_data/cr1000"
setwd(rdir) # workig directory
siteID <- 'DSM'

# Read the URL.
url <- "https://ibis.geog.ubc.ca/~micromet/webdata/resources/csv/" #Change/update URL so that it is getting the latest met data

# Define files to download
files <- c("DTA.csv", "ORP.csv", "PAR.csv",
  "PCT.csv","RAD.csv","RHA.csv","SHA.csv","STA.csv",
  "WPT.csv","WTA.csv","WpH.csv","WCd.csv","WDO.csv",
  "NDVI.csv","PRI.csv","WVA.csv","WDA.csv","PSA.csv")

# Loops through files to download
for(i in 1:length(files)){
  download.file(paste(url,siteID,files[i],sep=""),destfile = paste(getwd(),"/",siteID,files[i],sep=""))
}
