# tutorial time series...
install.packages("astsa")  # install it ... I'm using version 1.5 below
library(astsa)             # then load it (has to be done at the start of each session)
data()       
jj   # time series...
jjm <- as.matrix(jj)

# make monthly time series... seasonality is year...
(zardoz = ts(rnorm(48), start=c(2293,6), frequency=12)) 

# use window() if you want a part of a ts object
(oz = window(zardoz, start=2294, end=c(2295,12)))

time(jj)
cycle(jj)
plot(jj, ylab="Earnings per Share", main="J & J")
plot(jj, type="o", col="blue", lty="dashed")
plot(diff(log(jj)), main="logged and diffed")


x = -5:5                  # sequence of integers from -5 to 5
y = 5*cos(x)              # guess
par(mfrow=c(3,2))         # multifigure setup: 3 rows, 2 cols
#---  plot:
plot(x, main="plot(x)")
plot(x, y, main="plot(x,y)")
#---  plot.ts:
plot.ts(x, main="plot.ts(x)")
plot.ts(x, y, main="plot.ts(x,y)")
#---  ts.plot:
ts.plot(x, main="ts.plot(x)")
ts.plot(ts(x), ts(y), col=1:2, main="ts.plot(x,y)")  # note- x and y are ts objects 
#---  the help files [? and help() are the same]:
?plot.ts
help(ts.plot)
?par        # might as well skim the graphical parameters help file while you're here

k = c(.5,1,1,1,.5)            # k is the vector of weights
(k = k/sum(k))       
#[1] 0.125 0.250 0.250 0.250 0.125
fjj = stats::filter(jj, sides=2, k)  # ?filter for help [but you knew that already]
plot(jj)
lines(fjj, col="red")         # adds a line to the existing plot
lines(lowess(jj), col="blue", lty="dashed")



dljj = diff(log(jj))        # difference the logged data
plot(dljj)                  # plot it (not shown)

shapiro.test(dljj)          # test for normality 
# Shapiro-Wilk normality test
# data:  dljj 
# W = 0.9725, p-value = 0.07211
# 


par(mfrow=c(2,1))        # set up the graphics 
hist(dljj, prob=TRUE, 12)   # histogram    
lines(density(dljj))     # smooth it - ?density for details 
qqnorm(dljj)             # normal Q-Q plot  
qqline(dljj)             # add a line    


lag1.plot(dljj, 4)  # this is the astsa version of lag.plot in the stats package 



acf2(dljj)   # astsa gives both in one swell foop ... or use acf() and pacf() individually 
