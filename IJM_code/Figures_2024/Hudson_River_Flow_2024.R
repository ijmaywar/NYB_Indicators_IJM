################################################################################
# Figures for River Flow - taken from Laura Gruenburg's code (last updated 
# December 14, 2023)
#
# Edited by IJM November 17, 2024
################################################################################

rm(list = ls())

#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("/Users/ian/Desktop/*NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(ggplot2)

####### Load the datasets
setwd("/Users/ian/Desktop/*NYB Indicators/Final_timeseries")
hudson<-read.csv("RiverFlow_NOV_17_2024.csv", header = TRUE)

meanflow <- hudson[hudson$Variable == 'Hudson_meanflow',]
cfs_to_cms<-0.0283168
meanflow$Val <- meanflow$Val*cfs_to_cms

qmeanflow = quantile(meanflow$Val, probs = c(.30, .70))

# find the last 5 years mean
mn_meanflow5 = mean(meanflow$Val[meanflow$Year %in% tail(meanflow$Year,5)])
# what quantile the data is in
mn_meanflow5 >qmeanflow

# Create a GAM - adjust k and remember to check model
mod<- gam(Val ~ s(Year, k=15), data = meanflow)
summary(mod) #check out model
gam.check(mod)

pdata <- with(meanflow, data.frame(Year = Year))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = mod$coefficients[[1]]  # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod.d <- Deriv(mod, n=length(mod$model$Year)) # n is the number of Years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                    +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Val ~ Year, data = meanflow)
lines(Val ~ Year, data = meanflow)
lines(p2_mod+intercept ~ Year, data = pdata, type = "n")
lines(p2_mod+intercept ~ Year, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ Year, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ Year, data = pdata, col = "red", lwd = 3)

linearMod<- lm(Val ~ Year, data=meanflow)
summary(linearMod)

latest <- tail(mod$model,1)

mean_flow <- ggplot() + 
  geom_line(data = meanflow, aes(x = Year, y = Val), color = 'grey53') +
  geom_point(data = meanflow, aes(x = Year, y = Val), color = 'gray53') + 
  geom_point(data = meanflow[length(mod$model$Year) ,], aes(x = Year, y = Val), shape = 17, size = 3) + 
  geom_smooth(data = meanflow, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata, aes(x = Year, y = p2_mod+intercept), color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  xlim(1947,2025) + 
  geom_text(
    x = latest$Year, y = latest$Val,
    aes(label = latest$Year),  # Custom label
    hjust = 0.3, vjust = -1,             # Adjust position
    color = "black", size = 4              # Styling
  ) +
  labs (y = bquote("Mean Flow "~m^3~"/s"), x = 'Year', title = 'Hudson Mean Flow at Green Island') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
mean_flow

################################################################################
# Now plot all 4 together
library(ggpubr)

# Save the plot as a PNG
png(file = paste0("/Users/ian/Documents/GitHub/NYB_Indicators_Calculations-main/IJM_code/Figures_2024/",
                  "Hudson_mean_flow.png"), 
    width = 2000, height = 1200, res = 200)

# Create the plot
mean_flow

# Close the graphics device to save the file
dev.off()

################################################################################

