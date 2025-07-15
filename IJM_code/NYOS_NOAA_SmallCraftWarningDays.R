################################################################################
# Ian's version of the NYOS NOAA Small Craft Warning Days code
# 
# IJM edited code taken from NYOS_NOAA_BUOYDATA.R by Ellie Heywood and 
# Wind_new.Rmd by Laura Gruenburg
#
# Summary of edits made:
# The IDs of ISL and TT were switched
# - Exploring lack of data and how that may effect results
# - Average winds bewteen 33 and 34 knots were not being classified
# - Tropical Storm Warnings were not considered
# - Created new plots
################################################################################

rm(list = ls())

# Set environment --------------------------------------------------------------

library(rnoaa) # package to access the NOAA Buoy Data Center
library(stringr)
library(ncdf4)
library(lubridate)
library(tidyverse)
library(seas)
library(ggplot2)
library(dplyr)

# Download data ----------------------------------------------------------------

# This is no longer working so download manually by searching for the historical
# data for the stations at https://www.ndbc.noaa.gov/ 
# 
# getallyears<-function(buoy_id){
#   first_year <- buoy(dataset = 'stdmet', buoyid = buoy_id[[1]],
#                      year = buoy_id[[2]][1])$data#get first year of data
#   for(theyear in buoy_id[[2]][2:length(buoy_id[[2]])]){#get the rest of the years of data
#     first_year<-rbind(first_year,
#                       buoy(dataset = 'stdmet', buoyid = buoy_id[[1]],
#                            year = theyear)$data)#stick them together
#   }
#   first_year #print final dataset
# }
# 
# end_year<-2023 # designate the end year for last year of data
# 
# buoy_nyh_dl<-list("44065", 2008:end_year)#NY HARBOR 2008 to present
# buoy_isl_dl<-list("44025", 1991:end_year)#Long Island 30 miles offshore from ISLIP
# buoy_tt_dl<-list("44066",2009:end_year)#Texas Tower, 75 M from Long Beach, NJ. 2009 to present

buoy_path <- "/Users/ian/Desktop/*NYB Indicators/Final_Timeseries_Figures/Timeseries_Files_2024/NOAA_Buoys/"

# NY HARBOR
setwd(paste0(buoy_path,"44065/"))
NYH_files <- list.files(pattern = "\\.txt$")    
combined_data <- do.call(rbind, lapply(NYH_files, read.table, header = FALSE))

header_split <- strsplit(readLines(NYH_files[1], n = 2), "\\s+")
col_names <- mapply(function(x, y) ifelse(y == "", x, paste(x, y, sep = "_")),
                    header_split[[1]], header_split[[2]])

colnames(combined_data) <- col_names
b_nyh <- combined_data
b_nyh$time <- ymd_hm(
  paste(b_nyh[,'#YY_#yr'], b_nyh$MM_mo, b_nyh$DD_dy, b_nyh$hh_hr, b_nyh$mm_mn)
)
b_nyh$datetime <- lubridate::ymd_hms(format(as.POSIXct(b_nyh$time), 
                                            format = "%Y-%m-%d  %H:%M:%S"))

# Long Island 
# THIS ONE NEEDS WORK
setwd(paste0(buoy_path,"44025/"))
ISL_files <- list.files(pattern = "\\.txt$")    
cd_1 <- do.call(rbind, lapply(ISL_files[1:9], read.table, header = FALSE, skip = 1)) 
cd_1 <- cbind(cd_1[, 1:4], min = 0, cd_1[, 5:16], tide=NA)
colnames(cd_1) <- c(colnames(cd_1)[1:4],colnames(cd_1)[6:17],"V17","V18")
cd_2_1 <- read.table(ISL_files[10], header = FALSE, skip = 1, fill = TRUE, nrows=4953) # w/ empty tide col - entries missing
cd_2_1 <- cbind(cd_2_1[, 1:4], min = 0, cd_2_1[, 5:16], tide=NA)
colnames(cd_2_1) <- c(colnames(cd_2_1)[1:4],colnames(cd_2_1)[6:17],"V17","V18")
cd_2_2 <- read.table(ISL_files[10], header = FALSE, fill = TRUE, skip=4954) # w/ empty tide col
cd_2_2 <- cbind(cd_2_2[, 1:4], min = 0, cd_2_2[, 5:17])
colnames(cd_2_2) <- c(colnames(cd_2_2)[1:4],colnames(cd_2_2)[6:18],"V18")
cd_3 <- do.call(rbind, lapply(ISL_files[11:14], read.table, header = FALSE, skip = 1)) # w/ tide col
cd_3 <- cbind(cd_3[, 1:4], min = 0, cd_3[, 5:17])
colnames(cd_3) <- c(colnames(cd_3)[1:4],colnames(cd_3)[6:18],"V18")
cd_4 <- do.call(rbind, lapply(ISL_files[15:length(ISL_files)], read.table, header = FALSE, skip = 1)) # w/ mins
combined_data <- rbind(cd_1,cd_2_1,cd_2_2,cd_3,cd_4)

header_split <- strsplit(readLines(ISL_files[length(ISL_files)], n = 2), "\\s+")
col_names <- mapply(function(x, y) ifelse(y == "", x, paste(x, y, sep = "_")),
                    header_split[[1]], header_split[[2]])

colnames(combined_data) <- col_names
b_isl <- combined_data
b_isl$time <- ymd_hm(
  paste(b_isl[,'#YY_#yr'], b_isl$MM_mo, b_isl$DD_dy, b_isl$hh_hr, b_isl$mm_mn)
)
b_isl$datetime <- lubridate::ymd_hms(format(as.POSIXct(b_isl$time), 
                                           format = "%Y-%m-%d  %H:%M:%S"))

# Texas Tower
setwd(paste0(buoy_path,"44066/"))
TT_files <- list.files(pattern = "\\.txt$")    
combined_data <- do.call(rbind, lapply(TT_files, read.table, header = FALSE))

header_split <- strsplit(readLines(TT_files[1], n = 2), "\\s+")
col_names <- mapply(function(x, y) ifelse(y == "", x, paste(x, y, sep = "_")),
                    header_split[[1]], header_split[[2]])

colnames(combined_data) <- col_names
b_tt <- combined_data
b_tt$time <- ymd_hm(
  paste(b_tt[,'#YY_#yr'], b_tt$MM_mo, b_tt$DD_dy, b_tt$hh_hr, b_tt$mm_mn)
)
b_tt$datetime <- lubridate::ymd_hms(format(as.POSIXct(b_tt$time), 
                                           format = "%Y-%m-%d  %H:%M:%S"))


# Transform data ---------------------------------------------------------------

# Texas Tower
# buoy_tt_dl#this is your input to the function
# texas_tower_buoy <- getallyears(buoy_tt_dl) 
# 
# b_tt <- texas_tower_buoy
# 
# datetime <- lubridate::ymd_hms(format(as.POSIXct(b_tt$time), format = "%Y-%m-%d  %H:%M:%S"))
# b_tt$datetime<-datetime
which(is.na(b_tt$datetime))

b_tt$year<-year(b_tt$datetime)

b_tt$yday<-yday(b_tt$datetime)
length(unique(b_tt$yday))

b_tt$month<-month(b_tt$datetime, label = TRUE)
str(b_tt) # the dataset has a buncha shit

colnames(b_tt)[colnames(b_tt) == "WDIR_degT"] <- "wind_dir"
colnames(b_tt)[colnames(b_tt) == "WSPD_m/s"] <- "wind_spd"

# NY Harbor
# buoy_nyh_dl
# ny_harbor_buoy <- getallyears(buoy_nyh_dl) 
# 
# b_nyh <- ny_harbor_buoy
# 
# datetime<-as.POSIXct(strptime(b_nyh$time,"%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
# b_nyh$datetime<-datetime
which(is.na(b_nyh$datetime))

b_nyh$year<-year(b_nyh$datetime)

b_nyh$yday<-yday(b_nyh$datetime)
length(unique(b_nyh$yday))

b_nyh$month<-month(b_nyh$datetime, label = TRUE)
str(b_nyh) # the dataset has a buncha shit

colnames(b_nyh)[colnames(b_nyh) == "WDIR_degT"] <- "wind_dir"
colnames(b_nyh)[colnames(b_nyh) == "WSPD_m/s"] <- "wind_spd"

# South of Islip
# buoy_isl_dl
# islip_buoy <- getallyears(buoy_isl_dl) 
# 
# b_isl <- islip_buoy
# 
# datetime<-as.POSIXct(strptime(b_isl$time,"%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
# b_isl$datetime<-datetime
which(is.na(b_tt$datetime))

b_isl$year<-year(b_isl$datetime)
b_isl$yday<-yday(b_isl$datetime)
length(unique(b_nyh$yday))
b_isl$month<-month(b_isl$datetime, label = TRUE)
str(b_isl) # the dataset has a buncha shit

colnames(b_isl)[colnames(b_isl) == "WDIR_degT"] <- "wind_dir"
colnames(b_isl)[colnames(b_isl) == "WSPD_m/s"] <- "wind_spd"

# Turn months into numbers
b_nyh$month <- format(as.Date(b_nyh$datetime), "%m")
b_tt$month  <- format(as.Date(b_tt$datetime), "%m")
b_isl$month <- format(as.Date(b_isl$datetime), "%m")

# SAVE BUOY CSVs ---------------------------------------------------------------

setwd("/Users/ian/Desktop/*NYB Indicators/Final_Timeseries_Figures/Timeseries_Files_2024/")
write.csv(b_nyh,"Wind_b_nyh_Oct_15_2024.csv")
write.csv(b_tt,"Wind_b_tt_Oct_15_2024.csv")
write.csv(b_isl,"Wind_b_isl_Oct_15_2024.csv")


# Transform data for plotting --------------------------------------------------

################################################################################
# New York Harbor
daily_wind_nyh <- b_nyh %>% group_by(year, yday) %>%
  summarize(wind = mean(wind_spd)*(1/1852)*(3600/1),
            dir = mean(wind_dir),
            m = mean(as.numeric(month)))

daily_wind_nyh$season = 'Fall'
daily_wind_nyh[daily_wind_nyh$m == 1,]$season = 'Winter'
daily_wind_nyh[daily_wind_nyh$m == 2,]$season = 'Winter'
daily_wind_nyh[daily_wind_nyh$m == 3,]$season = 'Winter'

daily_wind_nyh[daily_wind_nyh$m == 4,]$season = 'Spring'
daily_wind_nyh[daily_wind_nyh$m == 5,]$season = 'Spring'
daily_wind_nyh[daily_wind_nyh$m == 6,]$season = 'Spring'

daily_wind_nyh[daily_wind_nyh$m == 7,]$season = 'Summer'
daily_wind_nyh[daily_wind_nyh$m == 8,]$season = 'Summer'
daily_wind_nyh[daily_wind_nyh$m == 9,]$season = 'Summer'

# Create advisory categories and get the counts
daily_wind_nyh$category = 'No advisory'
daily_wind_nyh[!is.na(daily_wind_nyh$wind) & daily_wind_nyh$wind >= 18 & daily_wind_nyh$wind <=33, ]$category ='small craft warning'
daily_wind_nyh[!is.na(daily_wind_nyh$wind) & daily_wind_nyh$wind > 33 & daily_wind_nyh$wind<=64, ]$category = 'tropical storm warning'
# daily_wind_nyh[!is.na(daily_wind_nyh$wind) & daily_wind_nyh$wind > 64, ]$category = 'Exceeded categories'
daily_wind_nyh$buoy <- 'New York Harbor'

warnings_nyh<- daily_wind_nyh %>% group_by(year, season, buoy) %>% 
  summarise(n_scw = sum(category == 'small craft warning'),
            n_tsw = sum(category == 'tropical storm warning'),
            n_ec = sum(category == 'Exceeded categories'),
            n_yday = n_distinct(yday))

################################################################################
# South of Islip
daily_wind_isl <- b_isl %>% group_by(year, yday) %>%
  summarize(wind = mean(wind_spd)*(1/1852)*(3600/1),
            dir = mean(wind_dir),
            m = mean(as.numeric(month)))

daily_wind_isl$season = 'Fall'
daily_wind_isl[daily_wind_isl$m == 1,]$season = 'Winter'
daily_wind_isl[daily_wind_isl$m == 2,]$season = 'Winter'
daily_wind_isl[daily_wind_isl$m == 3,]$season = 'Winter'

daily_wind_isl[daily_wind_isl$m == 4,]$season = 'Spring'
daily_wind_isl[daily_wind_isl$m == 5,]$season = 'Spring'
daily_wind_isl[daily_wind_isl$m == 6,]$season = 'Spring'

daily_wind_isl[daily_wind_isl$m == 7,]$season = 'Summer'
daily_wind_isl[daily_wind_isl$m == 8,]$season = 'Summer'
daily_wind_isl[daily_wind_isl$m == 9,]$season = 'Summer'

# Create advisory categories and get the counts
daily_wind_isl$category = 'No advisory'
daily_wind_isl[!is.na(daily_wind_isl$wind) & daily_wind_isl$wind >= 18 & daily_wind_isl$wind <=33, ]$category ='small craft warning'
daily_wind_isl[!is.na(daily_wind_isl$wind) & daily_wind_isl$wind > 33 & daily_wind_isl$wind<=64, ]$category = 'tropical storm warning'
# daily_wind_isl[!is.na(daily_wind_isl$wind) & daily_wind_isl$wind > 64, ]$category = 'Exceeded categories'
daily_wind_isl$buoy <- 'South of Islip'

warnings_isl<- daily_wind_isl %>% group_by(year, season, buoy) %>% 
  summarise(n_scw = sum(category == 'small craft warning'),
            n_tsw = sum(category == 'tropical storm warning'),
            n_ec = sum(category == 'Exceeded categories'),
            n_yday = n_distinct(yday))

################################################################################
# Texas Tower
daily_wind_tt <- b_tt %>% group_by(year, yday) %>%
  summarize(wind = mean(wind_spd)*(1/1852)*(3600/1),
            dir = mean(wind_dir),
            m = mean(as.numeric(month)))

daily_wind_tt$season = 'Fall'
daily_wind_tt[daily_wind_tt$m == 1,]$season = 'Winter'
daily_wind_tt[daily_wind_tt$m == 2,]$season = 'Winter'
daily_wind_tt[daily_wind_tt$m == 3,]$season = 'Winter'

daily_wind_tt[daily_wind_tt$m == 4,]$season = 'Spring'
daily_wind_tt[daily_wind_tt$m == 5,]$season = 'Spring'
daily_wind_tt[daily_wind_tt$m == 6,]$season = 'Spring'

daily_wind_tt[daily_wind_tt$m == 7,]$season = 'Summer'
daily_wind_tt[daily_wind_tt$m == 8,]$season = 'Summer'
daily_wind_tt[daily_wind_tt$m == 9,]$season = 'Summer'

# Create advisory categories and get the counts
daily_wind_tt$category = 'No advisory'
daily_wind_tt[!is.na(daily_wind_tt$wind) & daily_wind_tt$wind >= 18 & daily_wind_tt$wind <=33, ]$category ='small craft warning'
daily_wind_tt[!is.na(daily_wind_tt$wind) & daily_wind_tt$wind > 33 & daily_wind_tt$wind<=64, ]$category = 'tropical storm warning'
# daily_wind_tt[!is.na(daily_wind_tt$wind) & daily_wind_tt$wind > 64, ]$category = 'Exceeded categories'
daily_wind_tt$buoy <- 'Texas Tower'

warnings_tt<- daily_wind_tt %>% group_by(year, season, buoy) %>% 
  summarise(n_scw = sum(category == 'small craft warning'),
            n_tsw = sum(category == 'tropical storm warning'),
            # n_ec = sum(category == 'Exceeded categories'),
            n_yday = n_distinct(yday))

################################################################################
# Compile daily wind and all warnings
all_warnings <- rbind(warnings_nyh, warnings_tt, warnings_isl)

################################################################################
# Plot to look for missing data (ydays)

ggplot(all_warnings, aes(x=year, y=n_yday, color=buoy))+
  geom_point(stat='identity')+
  facet_grid(season~buoy, scales="free", space="free_x") + 
  theme_bw() +
  labs (y = bquote("Number of Days"), x = '', title = 'Number of days covered by data in each season') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5), 
        plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), 
        axis.title=element_text(size = 14, face   = 'bold'), 
        axis.text= element_text(color = 'black', size = 12))

################################################################################
# Plot number of warnings over time

# All warnings with y_day == 0 removed.
ggplot(all_warnings %>% filter(n_yday!= 0), aes(x=year, y=n_scw+n_tsw, color=buoy))+
  geom_line(stat='identity')+
  facet_grid(season~buoy, scales="free", space="free_x") + 
  theme_bw() +
  labs (y = bquote("Number of Days"), x = '', title = 'Total warning Days') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5), 
        plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), 
        axis.title=element_text(size = 14, face   = 'bold'), 
        axis.text= element_text(color = 'black', size = 12))

# Small Craft Advisories
ggplot(all_warnings, aes(x=year, y=n_scw, color=buoy))+
  geom_line(stat='identity')+
  facet_grid(season~buoy, scales="free", space="free_x") + 
  theme_bw() +
  labs (y = bquote("Number of Days"), x = '', title = 'Small Craft Warning Days') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5), 
        plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), 
        axis.title=element_text(size = 14, face   = 'bold'), 
        axis.text= element_text(color = 'black', size = 12))

# Tropical Storm Warnings
ggplot(all_warnings, aes(x=year, y=n_tsw, color=buoy))+
  geom_line(stat='identity')+
  facet_grid(season~buoy, scales="free", space="free_x")

# Exceeded Categories
ggplot(all_warnings, aes(x=year, y=n_ec, color=buoy))+
  geom_line(stat='identity')+
  facet_grid(season~buoy, scales="free", space="free_x")

################################################################################
# Plot number of warnings over time divided by the number of days w data in a year

# All warnings
report_fig <- ggplot(all_warnings, aes(x=year, y=(n_scw+n_tsw)/n_yday, color=buoy))+
  geom_line(stat='identity')+
  facet_grid(season~buoy, scales="free", space="free_x") +
  # facet_grid(season~buoy, scales = space="free_x") + 
  theme_bw() +
  labs (y = bquote("Proportion"), x = '', title = 'Proportion of days with data that are warning days') +
  ylim(0,1.00) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5), 
        plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), 
        axis.title=element_text(size = 14, face   = 'bold'), 
        axis.text= element_text(color = 'black', size = 12))
report_fig
# fall isl 2007? why 1.00? should be 0.00? 

################################################################################
# Save plot
library(ggpubr)

# Save the plot as a PNG
png(file = paste0("/Users/ian/Documents/GitHub/NYB_Indicators_Calculations-main/IJM_code/Figures_2024/",
                  "Wind_warnings.png"), 
    width = 2000, height = 1200, res = 200)

# Create the plot
report_fig

# Close the graphics device to save the file
dev.off()

################################################################################

# Small Craft Advisories
ggplot(all_warnings, aes(x=year, y=n_scw/n_yday, color=buoy))+
  geom_line(stat='identity')+
  facet_grid(season~buoy, scales="free", space="free_x") +
  # facet_grid(season~buoy, scales = space="free_x") + 
  theme_bw() +
  labs (y = bquote("Proportion"), x = '', title = 'Proportion of days with data that are small craft warning days') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5), 
        plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), 
        axis.title=element_text(size = 14, face   = 'bold'), 
        axis.text= element_text(color = 'black', size = 12))

# All warnings
ggplot(all_warnings, aes(x=year, y=(n_scw+n_tsw+n_ec)/n_yday, color=buoy))+
  geom_line(stat='identity')+
  facet_grid(season~buoy, scales="free_x", space="free_x") +
  # facet_grid(season~buoy, scales = space="free_x") + 
  theme_bw() +
  labs (y = bquote("Proportion"), x = '', title = 'Proportion of days with data that are warning days') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5), 
        plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), 
        axis.title=element_text(size = 14, face   = 'bold'), 
        axis.text= element_text(color = 'black', size = 12))
