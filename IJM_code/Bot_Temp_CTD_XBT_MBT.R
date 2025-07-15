################################################################################
#
#   IJM edits to try to create BT_insitu file with season and intercept
#
################################################################################

rm(list = ls())

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
setwd("~/Desktop/*NYB Indicators/CalculateIndicators/WaterTemperature/Data")
ddd_CTD <- read.csv("WOD_CTD_format_D50_Oct16_2024.csv", header = TRUE)
ddd_XBT <- read.csv("WOD_XBT_format_D50_OCT_23_2024.csv", header = TRUE)
ddd_MBT <- read.csv("WOD_MBT_format_D50_NOV_17_2024.csv", header = TRUE)

common_cols <- intersect(colnames(ddd_CTD), colnames(ddd_MBT))
ddd<-rbind(
  subset(ddd_CTD, select = common_cols), 
  subset(ddd_MBT, select = common_cols)
)

common_cols <- intersect(colnames(ddd), colnames(ddd_XBT))
ddd<-rbind(
  subset(ddd, select = common_cols), 
  subset(ddd_XBT, select = common_cols)
)

# ddd <- read.csv("/Users/ian/Downloads/WOD_CTD_XBT_MBT_GLD_August30_2021.csv",header=TRUE)
# colnames(ddd)[18] <- "bot_temp"

ddd$date <- as.character(ddd$date)
ddd$date<-as.Date(ddd$date,tryFormats = c("%Y-%m-%d"))
# ddd$date<-as.Date(ddd$date,tryFormats = c("%Y%m%d"))
ddd$date<-as.Date(ddd$date)
ddd$data_source<-"WOD"

sea<-read.csv("CTD_seawolf_Oct16_2024.csv")
sea$date<-as.Date(sea$date)
sea$data_source<-"SEAWOLF"


common_cols <- intersect(colnames(ddd), colnames(sea))
ddd<-rbind(
  subset(ddd, select = common_cols), 
  subset(sea, select = common_cols)
)

#Format date
ddd$date<-as.Date(ddd$date)
ddd$year<-year(ddd$date)
ddd$month<-month(ddd$date)
ddd$day<-yday(ddd$date)
ddd$year_fac<-factor(ddd$year)

###use the function to label
lazylab<-klab(ddd$lat, ddd$lon)
ndf<-data.frame(ddd, lazylab)
ndf$EPU<-as.character(ndf$EPU)
###Label casts not in an EPU
ndf[is.na(ndf$EPU),"EPU"]<-"Not in NES"

ndf$season <- NA
ndf[ndf$day %in% 1:90 == TRUE,]$season <- "winter"
ndf[ndf$day %in% 91:181 == TRUE,]$season <- "spring"
ndf[ndf$day %in% 182:273 == TRUE,]$season <- "summer"
ndf[ndf$day %in% 274:366 == TRUE,]$season <- "fall"

for (szn in c("winter","spring","summer","fall")) {
########ANALYSIS FOR THE NYB##########
NYB<-ndf[ndf$NYB %in% "NYB" == TRUE,]
nrow(NYB)
NYB<-NYB[NYB$season == szn,]
nrow(NYB)##22775 (old) now 38088 - when I did this it was 9399...
minN<-1##min number of obs per year (you choose) to retain data for an estimate. 
ntab<-table(NYB$year_fac)#make a table
ntab
remove_these<-names(ntab[ntab < minN])#remove below minN
###subdat1 removes data for years with less than minNcasts and is used in analysis
subdat1<-NYB[NYB$year_fac %in% remove_these == FALSE,]
nrow(subdat1)#this is how many casts are used -> 38186 - for IJM: 9384

######Display Data Density + TRIM
table(subdat1$year)#casts per year for data used in analysis
table(NYB$year)#cast per year for whole NYB
#plot of each year with number of casts per month
par(mfrow=c(2,2))
yearz<-sort(na.omit(unique(NYB$year)))
for(i in 1:length(yearz)){
  #i = 1
  daYear<-NYB[NYB$year == yearz[i],]
  barplot(table(daYear$month), main = paste(yearz[i], " ", nrow(daYear)))
}
dev.off()

#seawolf data vs. WOD data
table(NYB$data_source, NYB$year)
table(NYB$data_source)


subdat1<-subdat1[subdat1$bot_temp >0,]

#####Investigate warm temps
hist(subdat1[subdat1$data_source == "SEAWOLF","bot_temp"])
hist(subdat1[subdat1$data_source == "WOD","bot_temp"])
#The XBT data have some very warm temps.
#hist(subdat1[subdat1$data_source == "WOD_xbt","bot_temp"])

###I will censor the XBT data to the warmest temps observed at the other more reliable datasets
#warmWOD<-max(subdat1[subdat1$data_source == "WOD","bot_temp"], na.rm = TRUE)
#warmXBT<-subdat1[subdat1$data_source == "WOD_xbt" & subdat1$bot_temp > warmWOD,]

#subdat1<-subdat1[subdat1$bot_temp < warmWOD,]
subdat1<-subdat1[subdat1$bot_temp > 0,]
nrow(subdat1)
hist(subdat1$bot_temp)

#####Calculate NYB indicator with a GAM
# LEFT OFF HERE...SOMTHING IS WRONG WITH THIS. WHAT SHOULD K BE? 
nrow(unique(subdat1[, c("lon", "lat","month","year_fac")]))

NYB_m1<-gam(bot_temp ~ s(lon,lat,k=100) + s(month,k=3) +  year_fac,  data = subdat1)
summary(NYB_m1)#check out model
gam.check(NYB_m1)
par(mfrow = c(1,2))
plot(NYB_m1)#month effect is interesting

plot(NYB_m1$fitted.values, NYB_m1$y, pch = 19, cex = .3,
     col = factor(subdat1$data_source))
abline(0,1)

####Grab the coefficient for the indicator
regexp <- "[[:digit:]]+"
index_vals<-NYB_m1$coefficients[grepl("year_fac",names(NYB_m1$coefficients))]
years<-as.numeric(str_extract(names(index_vals), regexp))
index_1<-data.frame(years, index_vals)#THe first year of the data is the baseline
#so need to add in first year as a 0
indexNYB<-rbind(data.frame(years = min(subdat1$year, na.rm = TRUE),index_vals = 0), index_1)

###Format for use 
fNYB_szn<-data.frame(Year = indexNYB$years,
                 Variable = "BT_insitu",
                 Val = scale(indexNYB$index_vals, scale = FALSE),
                 Loc = "NYB",
                 season = szn,
                 Intercept = NYB_m1$coefficients[1],
                 N = as.vector(table(subdat1$year)))
plot(fNYB_szn$Year, scale(fNYB_szn$Val, scale = FALSE), type = "b")
abline(h = 0)
abline(v=2003)

if (szn=="winter") {
  fNYB <- fNYB_szn
} else {
  fNYB <- rbind(fNYB,fNYB_szn)
}

}

#saving just for NYB indicator time series
setwd("/Users/ian/Desktop/*NYB Indicators/Final_timeseries")
write.csv(fNYB, "BT_insitu_Nov_17_2024.csv")

