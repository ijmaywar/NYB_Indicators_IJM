################################################################################
#
# IJM
# Figures for sea level trend from Montauk and the battery 
#
# Last edited: Dec 11 2024
#
################################################################################

rm(list = ls())

setwd("~/Desktop/*NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(ggplot2)
library(readr)

#######Load the datasets
# Download from https://tidesandcurrents.noaa.gov/sltrends/
setwd("/Users/ian/Desktop/*NYB Indicators/Final_Timeseries_Figures/Timeseries_Files_2024/Sea_Level/")
montauk <- read_csv("8510560_meantrend montauk tide guage 8510560.csv", skip=4)
battery <- read_csv("8518750_meantrend battery park 8518750.csv",skip=4)

# Find yearly and seasonal averages ############################################

# # Calculate yearly average
# montauk <- montauk %>%
#   group_by(Time) %>%
#   summarise(Monthly_MSL = mean(Monthly_Monthly_MSL, na.rm = TRUE))
# 
# battery <- battery %>%
#   group_by(Time) %>%
#   summarise(Monthly_MSL = mean(Monthly_Monthly_MSL, na.rm = TRUE))

# Calculate fractional time to represent months during years
montauk$Time <- montauk$Year + (montauk$Month - 1) / 12
battery$Time <- battery$Year + (battery$Month - 1) / 12

# Montauk
qmontauk = quantile(montauk$Monthly_MSL, probs = c(.30,.70))

# find the last 5 years mean - this is going TO BE DIFFERENT BECAUSE THE DATA ISN'T REALLY YEARLY ANYMORE
mn_montauk5 = mean(montauk$Monthly_MSL[montauk$Year %in% tail(unique(montauk$Year))])
# what quintile the data is in
mn_montauk5 < qmontauk


# Create a GAM - adjust k and remember to check model
mod_montauk<- gam(Monthly_MSL ~ s(Time, k=5), data = montauk)
summary(mod_montauk) #check out model
gam.check(mod_montauk)

pdata_montauk <- with(montauk, data.frame(Time = Time))
p2_mod_montauk <- predict(mod_montauk, newdata = pdata_montauk,  type = "terms", se.fit = TRUE)
intercept_montauk = mod_montauk$coefficients[1]   # look at p2_mod and extract the intercept
pdata_montauk <- transform(pdata_montauk, p2_mod_montauk = p2_mod_montauk$fit[,1], se2_w = p2_mod_montauk$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Time"
mod_montauk.d <- Deriv(mod_montauk, n=length(unique(mod_montauk$model$Time))) # n is the number of Xs
mod_montauk.dci <- confint(mod_montauk.d, term = Term)
mod_montauk.dsig <- signifD(pdata_montauk$p2_mod_montauk, d = mod_montauk.d[[Term]]$deriv,
                         +                    mod_montauk.dci[[Term]]$upper, mod_montauk.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Monthly_MSL ~ Time, data = montauk)
lines(Monthly_MSL ~ Time, data = montauk)
lines(p2_mod_montauk+intercept_montauk ~ Time, data = pdata_montauk, type = "n")
lines(p2_mod_montauk+intercept_montauk ~ Time, data = pdata_montauk)
lines(unlist(mod_montauk.dsig$incr)+intercept_montauk ~ Time, data = pdata_montauk, col = "blue", lwd = 3)
lines(unlist(mod_montauk.dsig$decr)+intercept_montauk ~ Time, data = pdata_montauk, col = "red", lwd = 3)

linearmod_montauk <- lm(Monthly_MSL ~ Time, data=montauk)
summary(linearmod_montauk)

latest_montauk <- tail(mod_montauk$model,1)

montauk_plot <- ggplot() + 
  # geom_line(data = montauk, aes(x = Time, y = Monthly_Monthly_MSL), color = 'lightgrey') +
  geom_point(data = montauk, aes(x = Time, y = Monthly_MSL), color = 'lightgrey') + 
  # geom_point(data = montauk[32,], aes(x= Time, y = Monthly_Monthly_MSL), shape = 17, size =3) +
  geom_smooth(data = montauk, aes(x = Time, y = Monthly_MSL), method = lm, se = FALSE, color = 'black') +
  # geom_line(data=pdata_montauk, aes(x = Time, y = p2_mod_montauk+intercept_montauk), color = 'black', linetype = 'twodash', size = 1) +
  # geom_line(data = pdata_montauk, aes(y = unlist(mod_montauk.dsig$incr)+intercept_montauk, x = Time), color = "blue", size = 1) +
  # geom_line(data = pdata_montauk, aes(y = unlist(mod_montauk.dsig$decr)+intercept_montauk, x = Time), color = 'red', size = 1) +
  theme_bw() +
  # xlim(1947,2025) +
  xlim(1856,2024) +
  ylim(-.2,.25) +
  # geom_text(
  #   x = latest_montauk$Time, y = latest_montauk$Monthly_MSL,
  #   aes(label = "2024"),  # Custom label
  #   hjust = .75, vjust = -1,             # Adjust position
  #   color = "black", size = 4              # Styling
  # ) +
  labs (y = bquote("Mean sea level "~(m)~" "), x =' ',title = 'Monthly_MSL at Montauk') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
montauk_plot


# battery ######################################################################
qbattery = quantile(battery$Monthly_MSL, probs = c(.30,.70))

# find the last 5 years mean
mn_battery5 = mean(battery$Monthly_MSL[battery$Year %in% tail(unique(battery$Year))])
# what quintile the data is in
mn_battery5 < qbattery


# Create a GAM - adjust k and remember to check model
mod_battery<- gam(Monthly_MSL ~ s(Time, k=10), data = battery)
summary(mod_battery) #check out model
gam.check(mod_battery)

pdata_battery <- with(battery, data.frame(Time = Time))
p2_mod_battery <- predict(mod_battery, newdata = pdata_battery,  type = "terms", se.fit = TRUE)
intercept_battery = mod_battery$coefficients[1]   # look at p2_mod and extract the intercept
pdata_battery <- transform(pdata_battery, p2_mod_battery = p2_mod_battery$fit[,1], se2_w = p2_mod_battery$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Time"
mod_battery.d <- Deriv(mod_battery, n=length(unique(mod_battery$model$Time))) # n is the number of Xs
mod_battery.dci <- confint(mod_battery.d, term = Term)
mod_battery.dsig <- signifD(pdata_battery$p2_mod_battery, d = mod_battery.d[[Term]]$deriv,
                            +                    mod_battery.dci[[Term]]$upper, mod_battery.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Monthly_MSL ~ Time, data = battery)
lines(Monthly_MSL ~ Time, data = battery)
lines(p2_mod_battery+intercept_battery ~ Time, data = pdata_battery, type = "n")
lines(p2_mod_battery+intercept_battery ~ Time, data = pdata_battery)
lines(unlist(mod_battery.dsig$incr)+intercept_battery ~ Time, data = pdata_battery, col = "blue", lwd = 3)
lines(unlist(mod_battery.dsig$decr)+intercept_battery ~ Time, data = pdata_battery, col = "red", lwd = 3)

linearmod_battery <- lm(Monthly_MSL ~ Time, data=battery)
summary(linearmod_battery)

latest_battery <- tail(mod_battery$model,1)

battery_plot <- ggplot() + 
  # geom_line(data = battery, aes(x = Time, y = Monthly_Monthly_MSL), color = 'lightgrey') +
  geom_point(data = battery, aes(x = Time, y = Monthly_MSL), color = 'lightgrey') + 
  # geom_point(data = battery[32,], aes(x= Time, y = Monthly_Monthly_MSL), shape = 17, size =3) +
  geom_smooth(data = battery, aes(x = Time, y = Monthly_MSL), method = lm, se = FALSE, color = 'black') +
  # geom_line(data=pdata_battery, aes(x = Time, y = p2_mod_battery+intercept_battery), color = 'black', linetype = 'twodash', size = 1) +
  # geom_line(data = pdata_battery, aes(y = unlist(mod_battery.dsig$incr)+intercept_battery, x = Time), color = "blue", size = 1) +
  # geom_line(data = pdata_battery, aes(y = unlist(mod_battery.dsig$decr)+intercept_battery, x = Time), color = 'red', size = 1) +
  theme_bw() +
  xlim(1856,2025) +
  ylim(-0.4,.25) +
  # geom_text(
  #   x = latest_battery$Time, y = latest_battery$Monthly_MSL,
  #   aes(label = latest_battery$Time),  # Custom label
  #   hjust = .5, vjust = -1,             # Adjust position
  #   color = "black", size = 4              # Styling
  # ) +
  labs (y = bquote("Mean sea level "~(m)~" "), x =' ',title = 'Monthly_MSL at Battery Park') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
battery_plot

library(ggpubr)
ggarrange(battery_plot,montauk_plot,nrow=2,ncol=1)

################################################################################
# Now plot both together

# Save the plot as a PNG
png(file = paste0("/Users/ian/Documents/GitHub/NYB_Indicators_Calculations-main/IJM_code/Figures_2024/",
                  "Sea_Level.png"), 
    width = 4000, height = 2500, res = 300)

# Create the plot
ggarrange(battery_plot,montauk_plot,nrow=2,ncol=1)

# Close the graphics device to save the file
dev.off()

################################################################################

