################################################################################
#
# Laura Gruenburg's code modified by IJM
# Figures for Lobster thermal habitat
#
# Last edited: Dec 04 2024
#
################################################################################

rm(list = ls())

#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/*NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(ggplot2)
library(dplyr)

# Load the datasets
setwd("/Users/ian/Desktop/*NYB Indicators/Final_Timeseries_Figures/Timeseries_Files_2024")
df<-read.csv("Lobster_Thermal_Hab_Yearly_Nov_18_2024.csv", header = TRUE)

# read.csv("/Users/ian/Desktop/*NYB Indicators/Final_Timeseries_Figures/Timeseries_Files_2024/Lobster_Thermal_Hab_Jan_6_2022.csv", header=TRUE)

qdf = quantile(df$Percent_Inhospitable, probs = c(.3, 0.70))

# find the last 5 years mean
mn_df5 = mean(df$Percent_Inhospitable[df$X %in% tail(sort(df$X))])
# what quintile the data is in
mn_df5 >qdf

mod<- gam(Percent_Inhospitable ~ s(X, k=5), data = df)
summary(mod) #check out model
gam.check(mod)

pdata <- with(df, data.frame(X = X))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept =   mod$coefficients[[1]] # look at p2_mod and eXtract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

Term = "X"
mod.d <- Deriv(mod, n=length(unique(mod$model$X))) # n is the number of years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                    +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Percent_Inhospitable ~ X, data = df)
lines(Percent_Inhospitable ~ X, data = df)
lines(p2_mod+intercept ~ X, data = pdata, type = "n")
lines(p2_mod+intercept ~ X, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ X, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ X, data = pdata, col = "red", lwd = 3)

linearMod<- lm(Percent_Inhospitable ~ X, data=df)
summary(linearMod)

latest <- tail(mod$model,1)

Percent_Inhospitable_plot <- ggplot() + 
  geom_line(data = df, aes(x = X, y = Percent_Inhospitable), color = 'grey53') +
  geom_point(data = df, aes(x = X, y = Percent_Inhospitable), color = 'gray53') + 
  geom_point(data = df[32,], aes(x = X, y = Percent_Inhospitable), shape = 17, size =3) + 
  #geom_line(data = hypoxia_risk, aes(x = first_column, y = second_column), color = 'orange') +
  #geom_line(data = hypoxia, aes(x = first_column, y = second_column2), color = 'red') +
  #geom_smooth(data = df, aes(X = X, y = Percent_Inhospitable), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata, aes(x = X, y = p2_mod+intercept), color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = X), color = "blue", size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = X), color = 'red', size = 1) + 
  theme_bw() +
  xlim(1993,2026) +
  geom_text(
    x = latest$X, y = latest$Percent_Inhospitable,
    aes(label = latest$X),  # Custom label
    hjust = 0, vjust = 2,             # Adjust position
    color = "black", size = 4              # Styling
  ) +
  labs (y = 'Percent Area Lethal', x = 'Year', title = 'Lobster Thermal Habitat') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
Percent_Inhospitable_plot

################################################################################
# Save figure
library(ggpubr)

# Save the plot as a PNG
png(file = paste0("/Users/ian/Documents/GitHub/NYB_Indicators_Calculations-main/IJM_code/Figures_2024/",
                  "Lobster_Thermal_Habitat.png"), 
    width = 4000, height = 2200, res = 300)

# Create the plot
Percent_Inhospitable_plot

# Close the graphics device to save the file
dev.off()

################################################################################
