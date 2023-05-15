# Import previous data
v1<-read.csv(paste('/Users/sara/Google Drive/UBC/Micromet Lab/People/2019-Marion Nyberg/Data/AMF_CA-DBB_BASE_HH_2.csv',sep=','))

v1$TIMESTAMP_END <- as.character(v1$TIMESTAMP_END)  # Class was numeric so converted to character
v1$TIMESTAMP_END <- as.POSIXct(v1$TIMESTAMP_END, format="%Y%m%d%H%M", tz="UTC")

v1[v1 == -9999] <- NA
v1[v1 == -10000] <- NA

# Import new data
v2<-read.csv(paste('/Users/sara/Google Drive/UBC/Micromet Lab/Projects/2014-Burns Bog/Flux-tower/flux_data/BB_L2.csv',sep=','))

v2$DATE <- as.character(v2$DATE)  # Class was numeric so converted to character
v2$DATE <- as.POSIXct(v2$DATE, format="%Y-%m-%d %H:%M:%S", tz="UTC")

v2[v2 == -9999] <- NA
v2[v2 == -10000] <- NA

# Compare with re-processed data
plot_ly(data = v1, x = ~TIMESTAMP_END, y = ~FC, name = 'Old', type = 'scatter', mode = 'markers',marker = list(size = 3)) %>%
  add_trace(data = v2, x = ~DATE, y = ~co2_flux, name = 'New', mode = 'markers',marker = list(size = 3)) %>%
  layout(yaxis = list(range = c(-40, 30))) %>% 
  toWebGL()

# FIX 2015 units 
plot_ly(data = v1, x = ~TIMESTAMP_END, y = ~FCH4*1000, name = 'Old', type = 'scatter', mode = 'markers',marker = list(size = 3)) %>%
  add_trace(data = v2, x = ~DATE, y = ~ch4_flux*1000, name = 'New', mode = 'markers',marker = list(size = 3)) %>%
  layout(yaxis = list(range = c(-300, 800))) %>% 
  toWebGL()

plot_ly(data = v2, x = ~DATE, y = ~ch4_flux*1000, name = 'New', type = 'scatter', mode = 'markers',marker = list(size = 3)) %>%
  add_trace(data = v1, x = ~TIMESTAMP_END, y = ~FCH4*1000, name = 'Old', mode = 'markers',marker = list(size = 3)) %>%
  layout(yaxis = list(range = c(-300, 800))) %>% 
  toWebGL()

plot_ly(data = v1, x = ~TIMESTAMP_END, y = ~H, name = 'Old', type = 'scatter', mode = 'markers',marker = list(size = 3)) %>%
  add_trace(data = v2, x = ~DATE, y = ~H, name = 'New', mode = 'markers',marker = list(size = 3)) %>%
  layout(yaxis = list(range = c(-300, 800))) %>% 
  toWebGL()

plot_ly(data = v1, x = ~TIMESTAMP_END, y = ~LE, name = 'Old', type = 'scatter', mode = 'markers',marker = list(size = 3)) %>%
  add_trace(data = v2, x = ~DATE, y = ~LE, name = 'New', mode = 'markers',marker = list(size = 3)) %>%
  layout(yaxis = list(range = c(-300, 800))) %>% 
  toWebGL()


