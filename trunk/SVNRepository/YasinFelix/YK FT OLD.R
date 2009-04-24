library(Rmetrics)
library(fArma)
library(fracdiff)
library(fBasics)
library(tseries)
library(MASS)
library(fGarch)

setwd("Z:/MEng Project/MEng SVN/Data/States")

source("ATS_2008.R")

x = read.table("la.txt")
y = read.table("nile.txt")

## Original Data, Plot, ACF
current.data = as.ts(x[,4])
current.data = as.ts(y)

## Return Series Data
current.returns = (current.data[2:length(current.data)]-current.data[1:(length(current.data)-1)])/current.data[1:(length(current.data)-1)]
current.data = current.returns

## Basic Info
basicStats(current.data)
length(current.data)
plot(current.data)
acf(current.data, lag.max = 30)

## Differenced Data (Plot, ACF, PACF)
current.data.diff = diff(current.data)
plot(current.data.diff)
acf(current.data.diff,lag.max=30)
pacf(current.data.diff,lag.max=30)

## ---Original ADF-Test -> lag based on PACF plot
adfLag = 4
adfTest(current.data,lags=adfLag,type=c("c"))
adfTest(current.data,lags=adfLag,type=c("nc"))
adfTest(current.data,lags=adfLag,type=c("ct"))

adf.test(current.data, k=adfLag)
adf.test(current.data, k=adfLag, alternative = c("explosive"))

## ---Differenced ADF-Test -> lag based on PACF plot
pacf(diff(current.data.diff))
adfLag.diff = adfLag - 1
adfTest(current.data.diff,lags=adfLag.diff,type=c("c"))
adfTest(current.data.diff,lags=adfLag.diff,type=c("nc"))
adfTest(current.data.diff,lags=adfLag.diff,type=c("ct"))

adf.test(current.data.diff, k=adfLag.diff)

## Normality Test
ksnormTest(current.data.diff)
shapiroTest(current.data.diff)
jarqueberaTest(current.data.diff)
ad.Test = adTest(current.data.diff)
dagoTest(current.data.diff)

## Ljung Box test on Differenced Series
Box.Ljung.test(current.data.diff, lag = 12, adj.DF =  12)
Box.Ljung.test(current.data.diff^2, lag = 12, adj.DF =  12)

## Fractionally Differenced
d=fdGPH(current.data.diff)$d
d
fdGPH(current.data.diff)$sd.reg
current.data.diff = diffseries(current.data.diff, d)
Box.Ljung.test(current.data.diff^2,lag = 12, adj.DF = 12-params)

## Plot EACF
eacf(current.data.diff)

## Build ARIMA Model
current.data.fit1 = arima(current.data.diff,order = c(3,0,0))
current.data.fit1

#Ljung Box test on Model Resids and Sq. Resids 
params = 3
Box.Ljung.test(current.data.fit1$resid,lag = 12, adj.DF =  12-params)
Box.Ljung.test(current.data.fit1$resid^2,lag = 12, adj.DF =  12)

#--BUILD GARCH--#
box.test.pval = Box.Ljung.test(current.data.fit1$resid,lag = 12,adj.DF = 12-params)$p.value
arch.test.pval = Box.Ljung.test(current.data.fit1$resid^2,lag = 12, adj.DF = 12-params)$p.value

garch.fit=garchOxFit(formula.mean=~arma(1,1),formula.var=~garch(1,1),series=current.data.diff)
garch.fit = garchFit(formula = ~arma(3,0)+garch(1,1), data=current.data.diff,cond.dist = c("QMLE"), include.mean=TRUE)

meanParams = 3
volParams = 2
sresi=garch.fit$residuals/garch.fit$condvars^.5
Box.Ljung.test(sresi, lag = 12, adj.DF =  12-meanParams)  
Box.Ljung.test(sresi^2, lag = 12, adj.DF = 12-volParams ) 

#--Plots
plot(garch.fit$residuals, type = "l")
qqnorm(sresi) ; qqline(sresi)
acf(sresi)
acf(sresi^2)

#--Simulate Normal Data--#
sim.data = rnorm(662, mean = 0, sd = 1)
qqnorm(sim.data, main = "Normal Q-Q Plot of 662 Simulated Values from N(0,1)"); qqline(sim.data)
?plot
plot(density(sim.data), xlab = "Simulated Value", main = "Density Plot for Simulated Data, 662 Observations")
x <- seq(min(sim.data),max(sim.data),length=100)
hx <- dnorm(x,mean(sim.data),sd(sim.data))
lines(x,hx, lty = 2, col = 2)

#--
?boxcox

