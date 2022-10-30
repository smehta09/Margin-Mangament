#Time series 
#1. simulating a stationary time series

#if only MA dont put AR and vica versa
set.seed(100)
Wt<-10 + arima.sim(n=1000,list(ar=0.8,ma=c(0.4,0.1)),sd=5)
Wt
#2. simulating non stat time series

#Deterministic trend
# Xt = 3 + 8*t + Wt
t<-100
set.seed(50)
Xt<-3 + 8*(1:t) + Wt
Xt

#stochastic trend
# x is time series and y is non time series
# y2-y1 = x2
#y3-y2 = x3
#we calculate Yt using (1-B)*Yt=Xt using the cumsum function.
#Basically, Y2=X1+X2, Y3=X1+X2+X3, so Y2-Y1 = nabla[y2] = x2

yt<- cumsum(Wt) # cummulative sum of time series data Wt
#nabla[yt] = Wt
Wt
plot(Wt,main = "time series ARIMA(2,1,0)",col="blue")
yt
plot(yt,main = "differenced time series",col="red")
# converted to time series
Bt<-ts(yt)
Bt
plot(Bt,main = "conveRted time series ARIMA(2,1,1)", col="green")

# to view both plots together
#par(mfrow=c(1,2)) after this command run 2 plots



# time series class 2

setwd("D:/IFoA exams/CS2/R_working_files")
Xt<-read.table("Xt.csv",sep=",")
head(Xt)
str(Xt)
xt<-ts(Xt)
plot(xt)

xt<-ts(Xt,start=c(2000,1),frequency = 12)
plot(xt)
#for quaterly data frequency = 4

#march 1950

xtquatr<-ts(Xt,start=c(1950,3),frequency=4)
plot(xtquatr)

# graph of time series

plot(xt,main = "Time series xt",ylab="sale",col="red")

# if data not converted to time series 
ts.plot(Xt,main = "Time series Xt",ylab="sale",col="blue")

#locate points on graph

points(xt,col="black",cex=1)


#plotting sample ACF and sample PACF
xt<-ts(Xt,start=c(2000,1))
       
#lag defined by ourself in the graph of ACF shown
acf(xt,lag.max=18,main="graph of sample ACF of Xt",ylab="sample acf")
pacf(xt,lag.max=18,main="graph of sample PACF of Xt",ylab="sample pacf")

#blue dotted lines indicate 95% CI for ACF and PACF
#values falling outside dotted lines are significant
# in our graph values(PACF) after lag 3 dont fall out of dotted area, 
#so could be AR(3)



# Plotting theoritical ACF and PACF

#Yt = 0.5Yt-1 + 0.1Yt-2 + et +0.2et-1

modelacf<-ARMAacf(ar=c(0.5,-0.1),ma=0.2,lag.max=15)

modelacf
#PACF value theoritical

modelpacf<-ARMAacf(ar=c(0.5,-0.1),ma=0.2,lag.max=15,pacf=TRUE)
modelpacf

# barplot
barplot(modelacf,main="ACF of ARMA(2,1)",col="red",xlab="lag",ylab="ACF")

barplot(modelpacf,main="PACF of ARMA(2,1)",col="BLUE",xlab="lag",ylab="PACF",names=1:15)

par(mfrow=c(1,2))# to show 2 graphs at same time, run this first

#remove 1st value in barplot(modelacf[-1],... in above barplot code


#extracting nos

frequency(xt)
start(xt)


#calculation of sample ACF and PACF

a<-acf(xt,lag.max=18,main="graph of sample ACF of Xt",ylab="sample acf",plot=FALSE)
a
a[5]
a$acf[6]




#time series class 3


#analysing stationary
#diferencing
#least square trend removal

getwd()

test.stationary<-read.table("testing.stationarity.txt")
head(test.stationary)
str(test.stationary)
test.st<-ts(test.stationary)
str(test.st)
acf(test.st,main="sample ACF",ylab="sample ACF")


#PP test philips - perron test
#H0: time series has unit root hence can be differenced.
#H1: Time series doesnt need to be differenced(already stationary)

# p value<5%, suff evidence to reject H0, vica versa, diff not needed
# pvalue> 5%, differencing needed


PP.test(test.st)
#differencing needed

set.seed(1901)
n=365
data<-arima.sim(list(ma=c(-1.4,0.8)),n)
acf(data,main="Sample ACF of TS data",ylab="Sample ACF")

PP.test(data)
# p value less than 5 % , diff not needed, is stationary



#differencing
?diff

Xt<-diff(test.st,lag=1,differences=1)
Xt

#Xt = (1-B)test.st
#nabla Xt = (1-B)*Xt, difference =1
par(mfrow=c(2,2))
plot(test.st,main="original time series")
plot(Xt,main="diff ts")
acf(test.st,main="sample ACF of original series")
acf(Xt,main="sample ACF of differenced series")

par(mfrow=c(1,1))
PP.test(Xt)

# choosing d using variance

var(test.st)
var(Xt)

d2t<-diff(test.st,lag=1,differences=2)
var(d2t)
# d2t = (1-B)^2 test.st

#variance increased after 2nd time diff, so stationary achieved
# after 1 time differencing only
#note:The sample ACF is decreasing slowly and steadily. Therefore, the data should be differenced before 
#fitting a model

#least square trend removal

set.seed(123)
n=1000
sim<-arima.sim(list(ar=0.9),n)
xt<-2000+cumsum(sim)

ts.plot(xt)
#fitting linear model
time<-seq(1,1000)
fit<-lm(xt~time)
#regression line fitted
#xt=a+bt
names(fit)

fit$fitted.values
fit$residuals

par(mfrow=c(1,1))
ts.plot(xt,main="ts with linear trend")
lines(time,fit$fitted.values,type="l",col="blue")
plot(fit$residuals,ylab="residuals",main="plot of residuals",lwd=0.1)


#time series class 4


#Identify seasonality
#Removing seasonality

ldeaths
str(ldeaths)
# 1st plot
plot(ldeaths)
#ACF plot
acf(ldeaths)

#spectrum to se seasonality
spectrum(ldeaths,main="Periodogram",xlab="Frequency")

#combining plots

m<-matrix(c(1,2,1,3),2,2)
m
layout(m)
#then run plot graphs to show 1 saath


#removing seasonality

# we could see from graph seasonality of 12

sdiff<-diff(ldeaths,lag=12,differences=1)
sdiff
par(mfrow=c(2,2))
plot(ldeaths,main="original TS")
acf(ldeaths,main="sample ACF of original TS")
plot(sdiff,main="differenced TS")
acf(sdiff,main="sample ACF of diff time series")






# Time series class 5

# using inbuilt functions
#seasonality and white noise
#core reading has only decompose, alternative is stil

#used to seperate out seasonal variation, trend,white noise structure

ldeaths
#graph
plot(decompose(ldeaths,type="additive"))
# data
d<-(decompose(ldeaths,type="additive"))
d$seasonal
d$trend
d$random

plot(ldeaths)
points(ldeaths,cex=0.7,col="red")
lines(d$trend,col="blue")
lines(d$seasonal+d$trend,col="Green")

#Additive Model:
#X(t) = Trend + Seasonal + Random
#Multiplicative Model:
 # X(t) = Trend*Seasonal*Random
#Random = Data - Trend - Seasonal (Additive)
#Random = Data / (Trend*Seasonal) (Multiplicative












