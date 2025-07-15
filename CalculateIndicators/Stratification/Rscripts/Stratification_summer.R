#Purpose: Create Stratificaiton Indicator for SUMMER only
#Data: Use World Ocean Data (HowToGetWOD_data.Rmd)
#Data: Combine with SEAWOLF CTD casts (ProcessCTD_SeaWolf.R)
#Fit GAM and use year effect as indicator
#Kurt Heim, Last edited AUG 10, 2020
library(rgdal)
library(lubridate)
library(mgcv)
library(effects)
library(mgcViz)
library(stringr)
#####load required functiosn and shapefiles
setwd("/Users/ian/Documents/GitHub/NYB_Indicators_Calculations-main/CalculateIndicators/Rfunctions")
source("LabelPoints.R")

#######Load the datasets
setwd("/Users/ian/Desktop/NYB Indicators/CalculateIndicators/WaterTemperature/Data")
ddd<-read.csv("WOD_CTD_format_D50_Oct16_2024.csv", header = TRUE)
ddd$date<-as.Date(ddd$date)
ddd$data_source<-"WOD"

sea<-read.csv("CTD_seawolf_Oct16_2024.csv")
sea$date<-as.Date(sea$date)
sea$data_source<-"SEAWOLF"

#####Combine the two datasets
common_cols <- intersect(colnames(ddd), colnames(sea))
ddd<-rbind(
  subset(ddd, select = common_cols), 
  subset(sea, select = common_cols)
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
ndf[ndf$yday %in% 274:365 == TRUE,]$season <- "Fall"


for (szn in c("Winter","Spring","Summer","Fall")) {
  
  ########ANALYSIS FOR THE NYB##################################################
  NYB <- ndf[ndf$NYB %in% "NYB" == TRUE & ndf$season==szn,]
  minN<-10##min number of obs per year (you choose) to retain data for an estimate. 
  ntab<-table(NYB_szn$year_fac)#make a table
  ntab
  remove_these<-names(ntab[ntab < minN])#remove below minN
  ###subdat1 removes data for years with less than minNcasts and is used in analysis
  subdat1<-NYB_szn[NYB_szn$year_fac %in% remove_these == FALSE,]
  nrow(subdat1)#this is how many casts are used
  
  # ######Display Data Density + TRIM
  # table(subdat1$year)#casts per year for data used in analysis
  # table(NYB_szn$year)#cast per year for whole NYB
  # #plot of each year with number of casts per month
  # par(mfrow=c(5,5))
  # yearz<-unique(NYB_szn$year)
  # for(i in 1:length(yearz)){
  #   #i = 1
  #   daYear<-NYB_szn[NYB_szn$year == yearz[i],]
  #   barplot(table(daYear$month), main = paste(yearz[i], " ", nrow(daYear)))
  # }
  # dev.off()
  
  #seawolf data vs. WOD data
  table(NYB_szn$data_source, NYB_szn$year)
  table(NYB_szn$data_source)
  
  
  #####Calculate NYB indciaotr with a GAM
  NYB_szn_m1<-gam(log(strat) ~ s(lon,lat, k = 100) + s(yday) +  year_fac, 
              data = subdat1)
  summary(NYB_szn_m1)#check out model
  gam.check(NYB_szn_m1)
  par(mfrow = c(1,2))
  plot(NYB_szn_m1)#month effect is interesting
  
  ####Grab the coefficient for the indicator
  regexp <- "[[:digit:]]+"
  index_vals<-NYB_szn_m1$coefficients[grepl("year_fac",names(NYB_szn_m1$coefficients))]
  years<-as.numeric(str_extract(names(index_vals), regexp))
  index_1<-data.frame(years, index_vals)#THe first year of the data is the baseline
  #so need to add in first year as a 0
  indexNYB_szn<-rbind(data.frame(years = min(subdat1$year, na.rm = TRUE),index_vals = 0), index_1)
  
  
  ###Format for use 
  fNYB_szn<-data.frame(Year = indexNYB_szn$years,
                   Variable = "Strat_summer_insitu",
                   Val = scale(indexNYB_szn$index_vals, scale = FALSE),
                   Loc = "NYB",
                   season = szn,
                   Intercept = NYB_szn_m1$coefficients[1],
                   N = as.vector(table(subdat1$year)))
  plot(fNYB_szn$Year, fNYB_szn$Val, type = "b")
  
  #######Analysis for the MAB###################################################
  #repeat steps above but with data for only the MAB
  NYB<-ndf[ndf$EPU %in% "MAB" == TRUE & ndf$season==szn,]
  minN<-10##min number of obs per year
  ntab<-table(NYB_szn$year_fac)#make a table
  ntab
  remove_these<-names(ntab[ntab < minN])#remove below minN
  subdat1<-NYB_szn[NYB_szn$year_fac %in% remove_these == FALSE,]
  nrow(subdat1)
  
  
  # ######Display Data Density + TRIM
  # table(subdat1$year)
  # table(NYB_szn$year)
  # par(mfrow=c(5,5))
  # yearz<-unique(NYB_szn$year)
  # for(i in 1:length(yearz)){
  #   #i = 1
  #   daYear<-NYB_szn[NYB_szn$year == yearz[i],]
  #   barplot(table(daYear$month), main = paste(yearz[i], " ", nrow(daYear)))
  # }
  # dev.off()
  
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
  
  
  MAB_szn<-data.frame(Year = indexMAB$years,
                  Variable = "Strat_summer_insitu",
                  Val = scale(indexMAB$index_vals, scale = FALSE),
                  Loc = "MAB",
                  season = szn,
                  Intercept = MAB_m1$coefficients[1],
                  N = as.vector(table(subdat1$year)))
  
  plot(MAB_szn$Year, MAB_szn$Val, type = "b")
  
  ########Analysis for the whole NES############################################
  NYB<-ndf[ndf$EPU %in% "Not in NES" == FALSE & ndf$season==szn,]
  NYB_szn<-NYB_szn[is.na(NYB_szn$year) == FALSE,]
  minN<-10##min number of obs per year
  ntab<-table(NYB_szn$year_fac)#make a table
  ntab
  remove_these<-names(ntab[ntab < minN])#remove below minN
  subdat1<-NYB_szn[NYB_szn$year_fac %in% remove_these == FALSE,]
  nrow(subdat1)
  
  
  # ######Display Data Density + TRIM
  # table(subdat1$year)
  # table(NYB_szn$year)
  # par(mfrow=c(5,5))
  # yearz<-unique(NYB_szn$year)
  # for(i in 1:length(yearz)){
  #   #i = 1
  #   daYear<-NYB_szn[NYB_szn$year == yearz[i],]
  #   barplot(table(daYear$month), main = paste(yearz[i], " ", nrow(daYear)))
  # }
  # dev.off()
  
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
  
  
  NES_szn<-data.frame(Year = indexNES$years,
                  Variable = "Strat_summer_insitu",
                  Val = scale(indexNES$index_vals, scale = FALSE),
                  Loc = "NES",
                  season = szn,
                  Intercept = NES_m1$coefficients[1],
                  N = as.vector(table(subdat1$year)))
  plot(NES_szn$Year, NES_szn$Val, type = "l")
  
  ####PLOT and COMPARE THE TWO####
  plot(MAB_szn$Year, MAB_szn$Val, type = "b")
  points(fNYB_szn$Year, fNYB_szn$Val, type = "b", col = "red")
  points(NES_szn$Year, NES_szn$Val, type = "b", col = "green")
  abline(h=0)
  abline(v = 2012)
  
  
  #####Write to disk
  fff_szn<-rbind(MAB_szn,fNYB_szn, NES_szn)
  
  if (szn == "Winter"){
    fff <- fff_szn
  } else {
    fff <- rbind(fff,fff_szn)
  }
}

###write to csv
setwd("/Users/ian/Desktop/NYB Indicators/Final_timeseries")
write.csv(fff, "Strat_insitu_seasonally_OCT_22_2024.csv")



