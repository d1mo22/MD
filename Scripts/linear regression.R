setwd("C:/Users/max.estrade/Downloads")
dd <- read.table("hotel_sample_5000.csv",header=T, sep=",")

attach(dd)
names(dd)
columns(dd)
#plot(x,y)
plot(adults, adr) 
install.packages("car")   # only once
library(car)              # every time you start R
install.packages("MASS")   # only needed once
library(MASS)              # load it every R session


#rs_date increases r^2 but makes the charts dificult to understand

reg3 <- lm(adr ~ adults+children+meal+year+month+hotel+pre_bcn
           +pre_cancel+repeated
           +rroom_type+lead_time, data=dd) 

reg_full <- lm(adr ~ hotel + lead_time + year + month + week + day +
                 weekend_nights + week_nights + adults + children + babies +
                 meal  + market_seg + channel + repeated +
                 pre_cancel + pre_bcn + rroom_type + assroom_type + changes +
                 deposit_type + agent + days_wait + customer_type +
                 rcar_parking_spaces + ts_requests + r_status + country,
               data = dd)

# Summary of the full model
summary(reg_full)$coefficients

summary(reg_full)
alias(reg_full)

vif(reg_full)
model_step <- stepAIC(reg3, direction = "both")

# View final model summary
summary(model_step)
print (reg3) 
summary(reg3)

plot(reg3)

attributes(reg3)
reg3$xlevels
reg3$terms

# Check the fit visually 
plot(Sepal.Width, Petal.Width, col=Species)
# and without closing the plot window 
lines(Sepal.Width, reg3$fitted.values, col="blue")

