install.packages("devtools") # only need to do this once 
devtools::install_github("nickpoison/astsa") 
library(astsa) ar(mfrow=2:1) 
tsplot(jj, ylab="QEPS", type="o", col=4, main="Johnson & Johnson Quarterly Earnings") 
tsplot(log(jj), ylab="log(QEPS)", type="o", col=4) 
library(astsa) # SEE THE FOOTNOTE 
plot(jj, type="o", ylab="Quarterly Earnings per Share") 
plot(globtemp, type="o", ylab="Global Temperature Deviations") 
plot(speech)
library(TTR)
djia = getYahooData("^DJI", start=20060420, end=20160420, freq="daily")
lines(djiar)
library(xts)

w = rnorm(500,0,1) # 500 N(0,1) variates 
v = filter(w, sides=2, filter=rep(1/3,3)) # moving average 
v1 = filter(w, sides=1, filter=rep(1/3,3)) # moving average 
par(mfrow=c(1,1)) 
plot.ts(w, main="white noise") 
plot.ts(v, ylim=c(-3,3), main="moving average")


set.seed(154) # so you can reproduce the results
w = rnorm(200); x = cumsum(w) # two commands in one line
wd = w +.2; xd = cumsum(wd)
plot.ts(xd, ylim=c(-5,55), main="random walk", ylab='')
lines(x, col=4); abline(h=0, col=4, lty=2); abline(a=0, b=.2, lty=2)

cumsum(1:10)

djiar = diff(log(djia$Close))[-1] # approximate returns 
plot(djiar, main="DJIA Returns", type="n") 

djclose = acf(djiar$Close)
?acf
lh
acf(lh)

obj=cbind(lag(lh), lh)
cor(obj[,-1], obj[,-2])
obj[-1,]
obj[-48]
obj=cbind(lag(lh),lh,lag(lh,k=-1))

ccf(mdeaths, fdeaths, ylab = "cross-correlation")
ts1=ar(lh, demean = FALSE)
ts1$aic
ts1$ar
class(ts1)
newdata = lh[-5,]
predict(object, newdata, n.ahead = 1)

#use the last 5 lines of lh as new data

x <- rnorm (100) 
Box.test (x, lag = 1)

Box.test(lh,lag=1)
lhdiff =diff(lh)
Box.test(lhdiff,lag=1)
