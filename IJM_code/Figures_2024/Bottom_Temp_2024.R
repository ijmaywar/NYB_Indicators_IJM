################################################################################
#
# Laura Gruenburg's code modified by IJM
#
# LAST UPDATED: Nov 20, 2024
#
################################################################################

rm(list = ls())

setwd("~/Desktop/*NYB Indicators/Deriv")
source("Deriv.R")
library(dplyr)
library(mgcv)
library(ggplot2)

#######Load the datasets
setwd("/Users/ian/Desktop/*NYB Indicators/Final_timeseries")
bt<-read.csv("BT_insitu_Nov_17_2024.csv", header = TRUE)
nyb = bt[bt$Loc == 'NYB',]
nyb$Val_I <- nyb$Intercept + nyb$Val

winter = nyb[nyb$season == 'winter',]
spring = nyb[nyb$season == 'spring',]
summer = nyb[nyb$season == 'summer',]
fall = nyb[nyb$season == 'fall',]



# WINTER

winter1960 = winter[winter$Year >=1960, ]

qwinter1960 = quantile(winter1960$Val_I, probs = c(.30,.70))

# find the last 5 years mean
mn_winter19605 = mean(winter1960$Val_I[winter1960$Year %in% tail(sort(unique(winter1960$Year)),5)])
# what quintile the data is in
mn_winter19605 >qwinter1960

# Creat a GAM - adjust k and remember to check model
mod_w<- gam(Val_I ~ s(Year, k=5), data = winter)
summary(mod_w) #check out model
gam.check(mod_w)

pdata_w <- with(winter, data.frame(Year = Year))
p2_mod_w <- predict(mod_w, newdata = pdata_w,  type = "terms", se.fit = TRUE)
intercept_w = mod_w$coefficients[1]
pdata_w <- transform(pdata_w, p2_mod_w = p2_mod_w$fit[,1], se2_w = p2_mod_w$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod_w.d <- Deriv(mod_w, n=length(unique(mod_w$model$Year)))
mod_w.dci <- confint(mod_w.d, term = Term)
mod_w.dsig <- signifD(pdata_w$p2_mod_w, d = mod_w.d[[Term]]$deriv,
                      +                    mod_w.dci[[Term]]$upper, mod_w.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Val_I ~ Year, data = winter)
lines(Val_I ~ Year, data = winter)
lines(p2_mod_w+intercept_w ~ Year, data = pdata_w, type = "n")
lines(p2_mod_w+intercept_w ~ Year, data = pdata_w)
lines(unlist(mod_w.dsig$incr)+intercept_w ~ Year, data = pdata_w, col = "blue", lwd = 3)
lines(unlist(mod_w.dsig$decr)+intercept_w ~ Year, data = pdata_w, col = "red", lwd = 3)

# Build linear regression based on data starting at 1960
linearMod_w<- lm(Val_I ~ Year, data=winter %>% dplyr::filter(Year>=1960))
summary(linearMod_w)

latest_w <- tail(mod_w$model,1)

wint_plot <- ggplot() + 
  geom_line(data = winter, aes(x = Year, y = Val_I), color = 'grey52') +
  geom_point(data = winter, aes(x = Year, y = Val_I), color = 'grey52') + 
  #geom_point(data = winter[42,], aes(x= Year, y = Val_I), shape = 17, size =3) +
  # geom_smooth(data = winter %>% dplyr::filter(Year>=1960), aes(x = Year, y = Val_I), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata_w, aes(x = Year, y = p2_mod_w+intercept_w), color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_w, aes(y = unlist(mod_w.dsig$incr)+intercept_w, x = Year), color = "blue", size = 1) + 
  #geom_line(data = pdata_w, aes(y = unlist(mod_w.dsig$decr)+intercept_w, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  xlim(1940,2024) +
  geom_text(
    x = latest_w$Year, y = latest_w$Val,
    aes(label = latest_w$Year),  # Custom label
    hjust = 0, vjust = -1,             # Adjust position
    color = "black", size = 4              # Styling
  ) +
  labs (y = bquote("Temp (\u00B0C)"), x =' ',title = 'Winter') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

wint_plot

# SPRING


spring1960 = spring[spring$Year >=1960, ]

qspring1960 = quantile(spring1960$Val_I, probs = c(.30,.70))

# find the last 5 years mean
mn_spring19605 = mean(spring1960$Val_I[spring1960$Year %in% tail(sort(unique(spring1960$Year)),5)])
# what quintile the data is in
mn_spring19605 >qspring1960

#the last spring Val_Iue is not averaging all the spring months so it looks anomalously low
#spring = spring[spring$X<77,]
# Creat a GAM - adjust k and remember to check model
mod_sp<- gam(Val_I ~ s(Year, k=5), data = spring)
summary(mod_sp) #check out model
gam.check(mod_sp)

pdata_sp <- with(spring, data.frame(Year = Year))
p2_mod_sp <- predict(mod_sp, newdata = pdata_sp,  type = "terms", se.fit = TRUE)
intercept_sp = mod_sp$coefficients[[1]]   # look at p2_mod and extract the intercept
pdata_sp <- transform(pdata_sp, p2_mod_sp = p2_mod_sp$fit[,1], se2_sp = p2_mod_sp$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod_sp.d <- Deriv(mod_sp, n=length(mod_sp$model$Year)) # n is the number of years
mod_sp.dci <- confint(mod_sp.d, term = Term)
mod_sp.dsig <- signifD(pdata_sp$p2_mod_sp, d = mod_sp.d[[Term]]$deriv,
                       +                    mod_sp.dci[[Term]]$upper, mod_sp.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Val_I ~ Year, data = spring)
lines(Val_I ~ Year, data = spring)
lines(p2_mod_sp+intercept_sp ~ Year, data = pdata_sp, type = "n")
lines(p2_mod_sp+intercept_sp ~ Year, data = pdata_sp)
lines(unlist(mod_sp.dsig$incr)+intercept_sp ~ Year, data = pdata_sp, col = "blue", lwd = 3)
lines(unlist(mod_sp.dsig$decr)+intercept_sp ~ Year, data = pdata_sp, col = "red", lwd = 3)

linearMod_sp<- lm(Val_I ~ Year, data=spring[spring$Year > 1959, ])
summary(linearMod_sp)

latest_sp <- tail(mod_sp$model,1)

spr_plot <- ggplot() + 
  geom_line(data = spring, aes(x = Year, y = Val_I), color = 'grey52') +
  geom_point(data = spring, aes(x = Year, y = Val_I), color = 'grey52') + 
  geom_point(data = spring[c(length(mod_sp$model$Year),length(mod_sp$model$Year)-1),], aes(x= Year, y = Val_I), shape = 17, size =3) +
  geom_smooth(data = spring[spring$Year > 1959, ], aes(x = Year, y = Val_I), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata_sp, aes(x = Year, y = p2_mod_sp+intercept_sp), color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_sp, aes(y = unlist(mod_sp.dsig$incr)+intercept_sp, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata_sp, aes(y = unlist(mod_sp.dsig$decr)+intercept_sp, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  xlim(1940,2024) +
  geom_text(
    x = latest_sp$Year, y = latest_sp$Val,
    aes(label = latest_sp$Year),  # Custom label
    hjust = .5, vjust = 2,              # Adjust position
    color = "black", size = 4              # Styling
  ) +
  labs (y = bquote(" "), x = ' ', title = 'Spring') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

spr_plot

# SUMMER

summer1960 = summer[summer$Year >= 1960, ]

qsummer1960 = quantile(summer1960$Val_I, probs = c(.30,.70))

# find the last 5 years mean
mn_summer19605 = mean(summer1960$Val_I[summer1960$Year %in% tail(sort(unique(summer1960$Year)),5)])
# what quintile the data is in
mn_summer19605 >qsummer1960

# Creat a GAM - adjust k and remember to check model
mod_su<- gam(Val_I ~ s(Year, k=6), data = summer)
summary(mod_su) #check out model
gam.check(mod_su)

pdata_su <- with(summer, data.frame(Year = Year))
p2_mod_su <- predict(mod_su, newdata = pdata_su,  type = "terms", se.fit = TRUE)
intercept_su = mod_su$coefficients[[1]]  # look at p2_mod and extract the intercept
pdata_su <- transform(pdata_su, p2_mod_su = p2_mod_su$fit[,1], se2_su = p2_mod_su$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod_su.d <- Deriv(mod_su, n=length(unique(mod_su$model$Year))) # n is the number of years
mod_su.dci <- confint(mod_su.d, term = Term)
mod_su.dsig <- signifD(pdata_su$p2_mod_su, d = mod_su.d[[Term]]$deriv,
                       +                    mod_su.dci[[Term]]$upper, mod_su.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Val_I ~ Year, data = summer)
lines(Val_I ~ Year, data = summer)
lines(p2_mod_su+intercept_su ~ Year, data = pdata_su, type = "n")
lines(p2_mod_su+intercept_su ~ Year, data = pdata_su)
lines(unlist(mod_su.dsig$incr)+intercept_su ~ Year, data = pdata_su, col = "blue", lwd = 3)
lines(unlist(mod_su.dsig$decr)+intercept_su ~ Year, data = pdata_su, col = "red", lwd = 3)

linearMod_su<- lm(Val_I ~ Year, data=summer[summer$Year > 1959, ])
summary(linearMod_su)

latest_su <- tail(mod_su$model,1)

sum_plot <- ggplot() + 
  geom_line(data = summer, aes(x = Year, y = Val_I), color = 'grey52') +
  geom_point(data = summer, aes(x = Year, y = Val_I), color = 'grey52') + 
  geom_point(data = summer[length(mod_su$model$Year),], aes(x= Year, y = Val_I), shape = 17, size =3) +
  geom_smooth(data = summer[summer$Year > 1959, ], aes(x = Year, y = Val_I), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata_su, aes(x = Year, y = p2_mod_su+intercept_su), color = 'black', linetype = 'twodash', size = 1) +
  geom_line(data = pdata_su, aes(y = unlist(mod_su.dsig$incr)+intercept_su, x = Year), color = "blue", size = 1) +
  geom_line(data = pdata_su, aes(y = unlist(mod_su.dsig$decr)+intercept_su, x = Year), color = 'red', size = 1) +
  theme_bw() +
  xlim(1940,2024) +
  geom_text(
    x = latest_su$Year, y = latest_su$Val,
    aes(label = latest_su$Year),  # Custom label
    hjust = .5, vjust = 2,              # Adjust position
    color = "black", size = 4              # Styling
  ) +
  labs (y = bquote("Temp (\u00B0C)"), x = 'Year', title = 'Summer') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

sum_plot

# FALL

fall1960 = fall[fall$Year >=1960, ]

qfall1960 = quantile(fall1960$Val_I, probs = c(.30,.70))

# find the last 5 years mean
mn_fall19605 = mean(fall1960$Val_I[fall1960$Year %in% tail(sort(unique(fall1960$Year)),5)])
# what quintile the data is in
mn_fall19605 >qfall1960

# Creat a GAM - adjust k and remember to check model
#fall = fall[fall$Year < 2023, ]
#fall 2023 is incomplete, we only have data through early Nov.  Fall 2023 will be included next year
mod_fa<- gam(Val_I ~ s(Year, k=5), data = fall)
summary(mod_fa) #check out model
gam.check(mod_fa)

pdata_fa <- with(fall, data.frame(Year = Year))
p2_mod_fa <- predict(mod_fa, newdata = pdata_fa,  type = "terms", se.fit = TRUE)
intercept_fa = mod_fa$coefficients[[1]]  # look at p2_mod and extract the intercept
pdata_fa <- transform(pdata_fa, p2_mod_fa = p2_mod_fa$fit[,1], se2_fa = p2_mod_fa$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod_fa.d <- Deriv(mod_fa, n=length(unique(mod_fa$model$Year))) # n is the number of years
mod_fa.dci <- confint(mod_fa.d, term = Term)
mod_fa.dsig <- signifD(pdata_fa$p2_mod_fa, d = mod_fa.d[[Term]]$deriv,
                       +                    mod_fa.dci[[Term]]$upper, mod_fa.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Val_I ~ Year, data = fall)
lines(Val_I ~ Year, data = fall)
lines(p2_mod_fa+intercept_fa ~ Year, data = pdata_fa, type = "n")
lines(p2_mod_fa+intercept_fa ~ Year, data = pdata_fa)
lines(unlist(mod_fa.dsig$incr)+intercept_fa ~ Year, data = pdata_fa, col = "blue", lwd = 3)
lines(unlist(mod_fa.dsig$decr)+intercept_fa ~ Year, data = pdata_fa, col = "red", lwd = 3)

linearMod_fa<- lm(Val_I ~ Year, data=fall[fall$Year > 1959, ])
summary(linearMod_fa)

latest_fa <- tail(mod_fa$model,1)

fall_plot <- ggplot() + 
  geom_line(data = fall, aes(x = Year, y = Val_I), color = 'grey52') +
  geom_point(data = fall, aes(x = Year, y = Val_I), color = 'grey52') + 
  geom_point(data = fall[length(unique(mod_fa$model$Year)),], aes(x= Year, y = Val_I), shape = 17, size =3) +
  geom_smooth(data = fall[fall$Year > 1959, ], aes(x = Year, y = Val_I), method = lm, se = FALSE, color = 'black') + 
  geom_line(data = pdata_fa, aes(x = Year, y = p2_mod_fa+intercept_fa), color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_fa, aes(y = unlist(mod_fa.dsig$incr)+intercept_fa, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata_fa, aes(y = unlist(mod_fa.dsig$decr)+intercept_fa, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  xlim(1940,2024) +
  geom_text(
    x = latest_fa$Year, y = latest_fa$Val,
    aes(label = latest_fa$Year),  # Custom label
    hjust = .4, vjust = 2.5,              # Adjust position
    color = "black", size = 4              # Styling
  ) +
  labs (y = bquote(" "), x = 'Year', title = 'Autumn') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

fall_plot

################################################################################
# Now plot all 4 together
library(ggpubr)

# Save the plot as a PNG
png(file = paste0("/Users/ian/Documents/GitHub/NYB_Indicators_Calculations-main/IJM_code/Figures_2024/",
                  "Bottom_temp.png"), 
    width = 2000, height = 1200, res = 200)

# Create the plot
ggarrange(wint_plot,spr_plot,sum_plot,fall_plot,nrow=2,ncol=2)

# Close the graphics device to save the file
dev.off()

################################################################################

