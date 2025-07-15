################################################################################
#
# Kurt Heim's code modified by IJM
#
# Create Stratification Indicator for all seasons
# Data: Use World Ocean Data (HowToGetWOD_data.Rmd)
# Data: Combine with SEAWOLF CTD casts (ProcessCTD_SeaWolf.R)
# Fit GAM and use year effect as indicator
#
# LAST UPDATED: Nov 22, 2024
#
################################################################################

rm(list = ls())

library(dplyr)
library(rgdal)
library(lubridate)
library(mgcv)
library(effects)
library(mgcViz)
library(stringr)
#####load required functions and shapefiles
setwd("/Users/ian/Documents/GitHub/NYB_Indicators_Calculations-main/CalculateIndicators/Rfunctions")
source("LabelPoints.R")

#######Load the datasets
setwd("/Users/ian/Desktop/*NYB Indicators/CalculateIndicators/WaterTemperature/Data")
ddd<-read.csv("WOD_CTD_format_D50_Oct16_2024.csv", header = TRUE)
ddd$date<-as.Date(ddd$date)
ddd$data_source<-"WOD"

sea<-read.csv("CTD_seawolf_Oct16_2024.csv")
sea$date<-as.Date(sea$date)
sea$data_source<-"SEAWOLF"

#####Combine the two datasets
common_cols <- intersect(colnames(ddd), colnames(sea))
ddd<-rbind(
  ddd %>% dplyr::select(all_of(common_cols)), 
  sea %>% dplyr::select(all_of(common_cols))
)

#cacluate startification index for each CTD cast
ddd$strat<-ddd$c50_den/1000-ddd$surf_den/1000

#Format date
ddd$date<-as.Date(ddd$date)
ddd$year<-year(ddd$date)
ddd$month<-month(ddd$date)
ddd$day<-yday(ddd$date)
ddd$year_fac<-factor(ddd$year)
ddd$yday<-yday(ddd$date)


###use the function to label
lazylab<-klab(ddd$lat, ddd$lon)
ndf<-data.frame(ddd, lazylab)
ndf$EPU<-as.character(ndf$EPU)
###Label casts not in an EPU
ndf[is.na(ndf$EPU),"EPU"]<-"Not in NES"


###remove values that are funky
hist(ndf$strat)
ndf<-ndf[ndf$strat > 0,]#####REMOVE NEGATIVE VALUES
hist(ndf$strat)#some are very far on left tail...but

ndf$season <- NA
ndf[ndf$yday %in% 1:90 == TRUE,]$season <- "Winter"
ndf[ndf$yday %in% 91:181 == TRUE,]$season <- "Spring"
ndf[ndf$yday %in% 182:273 == TRUE,]$season <- "Summer"
ndf[ndf$yday %in% 274:366 == TRUE,]$season <- "Fall"


for (szn in c("Winter","Spring","Summer","Fall")) {
  
  ########ANALYSIS FOR THE NYB##################################################
  NYB <- ndf[ndf$NYB %in% "NYB" == TRUE,]
  NYB <- NYB %>% filter(season==szn)
  minN <- 1 ##min number of obs per year (you choose) to retain data for an estimate. 
  ntab<-table(NYB$year_fac)#make a table
  ntab
  remove_these<-names(ntab[ntab < minN])#remove below minN
  ###subdat1 removes data for years with less than minNcasts and is used in analysis
  subdat1<-NYB[NYB$year_fac %in% remove_these == FALSE,]
  nrow(subdat1)#this is how many casts are used
  
  #seawolf data vs. WOD data
  table(NYB$data_source, NYB$year)
  table(NYB$data_source)
  
  #####Calculate NYB indciaotr with a GAM
  NYB_m1<-gam(log(strat) ~ s(lon,lat, k = 100) + s(yday) +  year_fac, 
              data = subdat1)
  summary(NYB_m1)#check out model
  gam.check(NYB_m1)
  par(mfrow = c(1,2))
  plot(NYB_m1)#month effect is interesting
  
  ####Grab the coefficient for the indicator
  regexp <- "[[:digit:]]+"
  index_vals<-NYB_m1$coefficients[grepl("year_fac",names(NYB_m1$coefficients))]
  years<-as.numeric(str_extract(names(index_vals), regexp))
  index_1<-data.frame(years, index_vals)#THe first year of the data is the baseline
  #so need to add in first year as a 0
  indexNYB<-rbind(data.frame(years = min(subdat1$year, na.rm = TRUE),index_vals = 0), index_1)
  
  ###Format for use 
  fNYB<-data.frame(Year = indexNYB$years,
                   Variable = "Strat_summer_insitu",
                   Val = scale(indexNYB$index_vals, scale = FALSE),
                   Loc = "NYB",
                   season = szn,
                   Intercept = NYB_m1$coefficients[1],
                   N = as.vector(table(subdat1$year)))
  plot(fNYB$Year, fNYB$Val, type = "b")
  
  #######Analysis for the MAB###################################################
  #repeat steps above but with data for only the MAB
  NYB<-ndf[ndf$EPU %in% "MAB" == TRUE,]
  NYB <- NYB %>% filter(season==szn)
  minN <- 1 ##min number of obs per year
  ntab<-table(NYB$year_fac)#make a table
  ntab
  remove_these<-names(ntab[ntab < minN])#remove below minN
  subdat1<-NYB[NYB$year_fac %in% remove_these == FALSE,]
  nrow(subdat1)
  
  ######Calculate indicator with a GAM
  MAB_m1<-gam(log(strat) ~ s(lon,lat, k = 100) + s(yday) +  year_fac, 
              data = subdat1)
  summary(MAB_m1)#check out model
  gam.check(MAB_m1)
  par(mfrow = c(1,2))
  plot(MAB_m1)
  
  #######Grab the coefficient for the indicator
  regexp <- "[[:digit:]]+"
  index_vals<-MAB_m1$coefficients[grepl("year_fac",names(MAB_m1$coefficients))]
  years<-as.numeric(str_extract(names(index_vals), regexp))
  index_1<-data.frame(years, index_vals)#THe first year of the data is the baseline
  #so need to add it in
  indexMAB<-rbind(data.frame(years = min(subdat1$year, na.rm = TRUE),index_vals = 0), index_1)
  
  
  MAB<-data.frame(Year = indexMAB$years,
                  Variable = "Strat_summer_insitu",
                  Val = scale(indexMAB$index_vals, scale = FALSE),
                  Loc = "MAB",
                  season = szn,
                  Intercept = MAB_m1$coefficients[1],
                  N = as.vector(table(subdat1$year)))
  
  plot(MAB$Year, MAB$Val, type = "b")
  
  ########Analysis for the whole NES############################################
  NYB<-ndf[ndf$EPU %in% "Not in NES" == FALSE,]
  NYB <- NYB %>% filter(season == szn)
  NYB<-NYB[is.na(NYB$year) == FALSE,]
  minN <- 1 ##min number of obs per year
  ntab<-table(NYB$year_fac)#make a table
  ntab
  remove_these<-names(ntab[ntab < minN])#remove below minN
  subdat1<-NYB[NYB$year_fac %in% remove_these == FALSE,]
  nrow(subdat1)
  
  ######DO THE ANALYSIS#############
  NES_m1<-gam(log(strat) ~ s(lon,lat, k = 100) + s(yday) +  year_fac, 
              data = subdat1)
  summary(NES_m1)#check out model
  gam.check(NES_m1)
  par(mfrow = c(1,2))
  plot(NES_m1)
  
  #########Grab the coefficient for the indicator
  library(stringr)
  regexp <- "[[:digit:]]+"
  index_vals<-NES_m1$coefficients[grepl("year_fac",names(NES_m1$coefficients))]
  years<-as.numeric(str_extract(names(index_vals), regexp))
  index_1<-data.frame(years, index_vals)#THe first year of the data is the baseline
  #so need to add it in
  indexNES<-rbind(data.frame(years = min(subdat1$year, na.rm = TRUE),index_vals = 0), index_1)
  
  
  NES<-data.frame(Year = indexNES$years,
                  Variable = "Strat_summer_insitu",
                  Val = scale(indexNES$index_vals, scale = FALSE),
                  Loc = "NES",
                  season = szn,
                  Intercept = NES_m1$coefficients[1],
                  N = as.vector(table(subdat1$year)))
  plot(NES$Year, NES$Val, type = "l")
  
  ####PLOT and COMPARE THE TWO####
  plot(MAB$Year, MAB$Val, type = "b")
  points(fNYB$Year, fNYB$Val, type = "b", col = "red")
  points(NES$Year, NES$Val, type = "b", col = "green")
  abline(h=0)
  abline(v = 2012)
  
  #####Write to disk
  fff_szn<-rbind(MAB,fNYB,NES)
  
  if (szn == "Winter"){
    fff <- fff_szn
  } else {
    fff <- rbind(fff,fff_szn)
  }
}

###write to csv
setwd("/Users/ian/Desktop/*NYB Indicators/Final_timeseries")
write.csv(fff, "Strat_insitu_seasonally_OCT_22_2024.csv")



