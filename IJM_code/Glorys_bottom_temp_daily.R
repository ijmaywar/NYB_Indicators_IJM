library(dplyr)
library(terra)
library(rerddap)
library(ncdf4)
library(rgdal)
library(lubridate)
library(nngeo)
library(reshape2)

# Open the NetCDF connection
Glorys_raw_1993_2010 <- nc_open("/Users/ian/Desktop/*NYB Indicators/CalculateIndicators/WaterTemperature/Data/Glorys/cmems_mod_glo_phy_my_0.083deg_P1D-m_1731938799535.nc")
Glorys_raw_2011_2020 <- nc_open("/Users/ian/Desktop/*NYB Indicators/CalculateIndicators/WaterTemperature/Data/Glorys/cmems_mod_glo_phy_my_0.083deg_P1D-m_1731939015424.nc")
Glorys_raw_2021_2024 <- nc_open("/Users/ian/Desktop/*NYB Indicators/CalculateIndicators/WaterTemperature/Data/Glorys/cmems_mod_glo_phy_myint_0.083deg_P1D-m_1731939096588.nc")
  
# Extract the bottomT values and add the lon/lat/time dimension names
res_1 <- ncvar_get(Glorys_raw_1993_2010, varid = "bottomT")
dimnames(res_1) <- list(lon = Glorys_raw_1993_2010$dim$longitude$vals,
                      lat = Glorys_raw_1993_2010$dim$latitude$vals,
                      t = Glorys_raw_1993_2010$dim$time$vals)
# Convert the data into a 'long' dataframe for use in the 'tidyverse' ecosystem
res_1 <- as.data.frame(reshape2::melt(res_1, value.name = "bottomT"), row.names = NULL) %>% 
  mutate(t = as.Date(as.POSIXct(t, origin = "1970-01-01 00:00:00")))
# Close the NetCDF connection and finish
nc_close(Glorys_raw_1993_2010)

# Extract the SST values and add the lon/lat/time dimension names
res_2 <- ncvar_get(Glorys_raw_2011_2020, varid = "bottomT")
dimnames(res_2) <- list(lon = Glorys_raw_2011_2020$dim$longitude$vals,
                        lat = Glorys_raw_2011_2020$dim$latitude$vals,
                        t = Glorys_raw_2011_2020$dim$time$vals)
# Convert the data into a 'long' dataframe for use in the 'tidyverse' ecosystem
res_2 <- as.data.frame(reshape2::melt(res_2, value.name = "bottomT"), row.names = NULL) %>% 
  mutate(t = as.Date(as.POSIXct(t, origin = "1970-01-01 00:00:00")))
# Close the NetCDF connection and finish
nc_close(Glorys_raw_2011_2020)

# Open the NetCDF connection
# Extract the SST values and add the lon/lat/time dimension names
res_3 <- ncvar_get(Glorys_raw_2021_2024, varid = "bottomT")
dimnames(res_3) <- list(lon = Glorys_raw_2021_2024$dim$longitude$vals,
                        lat = Glorys_raw_2021_2024$dim$latitude$vals,
                        t = Glorys_raw_2021_2024$dim$time$vals)
# Convert the data into a 'long' dataframe for use in the 'tidyverse' ecosystem
res_3 <- as.data.frame(reshape2::melt(res_3, value.name = "bottomT"), row.names = NULL) %>% 
  mutate(t = as.Date(as.POSIXct(t, origin = "1970-01-01 00:00:00")))
# Close the NetCDF connection and finish
nc_close(Glorys_raw_2021_2024)

bbb <- rbind(res_1,res_2,res_3)

#### First, use a single net cdf file to make a 'key' for clipping and area assigmment
res_3$lat_lon<-paste(res_3$lat, res_3$lon, sep = "_")#this is just a unique id for each grid point
res_3_sub<-res_3[!duplicated(res_3$lat_lon),]#get rid of all duplicate dmeasuremnts at a point

setwd("/Users/ian/Documents/GitHub/NYB_Indicators_Calculations-main/CalculateIndicators/Rfunctions/")
source("LabelPoints.R")

lazylab<-klab(res_3_sub$lat, res_3_sub$lon)
tlabs<-data.frame(res_3_sub, lazylab)
tlabs$EPU<-as.character(tlabs$EPU)
tlabs[is.na(tlabs$EPU),"EPU"]<-"Not in NES"
tlabs$NYB<-as.character(tlabs$NYB)
tlabs[is.na(tlabs$NYB),"NYB"]<-"Not in NYB"

tlabs<-tlabs[,c(5,6,7)]

bbb$lat_lon<-paste(bbb$lat, bbb$lon, sep = "_") #takes awhile, makes a unique lat-long identifier

### remove points not within EPU AND not in NYB
bbb<-bbb[bbb$lat_lon %in% tlabs[tlabs$EPU == "Not in NES" & tlabs$NYB == "Not in NYB", "lat_lon"] == FALSE,]#get rid of points not in an EPU or in the NYB
nrow(bbb)
sum(is.na(bbb$bottomT))
bbb$year<-year(bbb$t)#add year
bbb$month<-month(bbb$t)#add month numeric
bbb$day<-day(bbb$t)#add day of month
bbb$yday<-yday(bbb$t)#add yearday

### bbb is the final dataset to work with for summary stats calculations
bbb<-merge(bbb,tlabs, by = "lat_lon")
colnames(bbb)[which(colnames(bbb)=="bottomT")] <- "temp"

nrow(bbb)
sum(is.na(bbb$temp))

setwd("/Users/ian/Desktop/*NYB Indicators/CalculateIndicators/WaterTemperature/Data/")
write.csv(bbb,'Glorys_bottomT_Nov_18_2024.csv')
