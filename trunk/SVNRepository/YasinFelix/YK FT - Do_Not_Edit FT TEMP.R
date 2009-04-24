library(Rmetrics)
library(fArma)
library(fracdiff)
library(fBasics)
library(tseries)
library(MASS)
library(fGarch)
library(stats)
library(base)

?garchFit

setwd("C:/Documents and Settings/wgt4/Desktop/All R Files/R Temp")

source("ATS_2008.R")

x = read.table("al.txt")
y = read.table("nile.txt")

## --Original Data, Plot, ACF
current.data = as.ts(x[,4])
current.data =current.data[40:length(current.data)]
interpolated.data = interpolate(current.data,method="linear")
current.data = interpolated.data


current.data = as.ts(y)

## --Return Series Data
current.returns = (current.data[2:length(current.data)]-current.data[1:(length(current.data)-1)])/current.data[1:(length(current.data)-1)]
current.data = current.returns

## --Basic Info
basicStats(current.data)
length(current.data)
plot(current.data)
acf(current.data, lag.max = 30)

## --Differenced Data (Plot, ACF, PACF)
current.data.diff = diff(current.data)
current.data.diff = current.data

plot(current.data.diff)
acf(current.data.diff,lag.max=30)
pacf(current.data.diff,lag.max=30)

## --Original ADF-Test -> lag based on PACF plot
adfLag = 7
adfTest(current.data,lags=adfLag,type=c("c"))
adfTest(current.data,lags=adfLag,type=c("nc"))
adfTest(current.data,lags=adfLag,type=c("ct"))

adf.test(current.data, k=adfLag)
adf.test(current.data, k=adfLag, alternative = c("explosive"))

## --Differenced ADF-Test -> lag based on PACF plot
pacf(diff(current.data.diff))
adfLag.diff = adfLag - 1
adfTest(current.data.diff,lags=adfLag.diff,type=c("c"))
adfTest(current.data.diff,lags=adfLag.diff,type=c("nc"))
adfTest(current.data.diff,lags=adfLag.diff,type=c("ct"))

adf.test(current.data.diff, k=adfLag.diff)

## --Normality Test
ksnormTest(current.data.diff)
shapiroTest(current.data.diff)
jarqueberaTest(current.data.diff)
ad.Test = adTest(current.data.diff)
dagoTest(current.data.diff)



## --LjungBox test on Differenced Series
Box.Ljung.test(current.data.diff, lag = 12, adj.DF =  12)
Box.Ljung.test(current.data.diff^2, lag = 12, adj.DF =  12)

## --Fractionally Differenced
d=fdGPH(current.data.diff)$d
d
fdGPH(current.data.diff)$sd.reg
current.data.diff = diffseries(current.data.diff, d)
Box.Ljung.test(current.data.diff^2,lag = 12, adj.DF = 12-params)

## --Plot EACF
eacf(current.data.diff)
 
## --Build ARIMA Model
current.data.fit1 = arima(current.data.diff,order = c(2,0,3))
current.data.fit1
#arima.fit

## --LjungBox test on Model Resids and Sq. Resids 
params = 5
Box.Ljung.test(current.data.fit1$resid,lag = 12, adj.DF =  12-params)
Box.Ljung.test(current.data.fit1$resid^2,lag = 12, adj.DF =  12)

## --BUILD GARCH--
box.test.pval = Box.Ljung.test(arima.fit$resid,lag = 12,adj.DF = 12-params)$p.value
arch.test.pval = Box.Ljung.test(arima.fit$resid^2,lag = 12, adj.DF = 12-params)$p.value

garch.fit = garchOxFit(formula.mean=~arma(1,3),formula.var=~garch(1,1),series=current.data.diff)
garch.fit = garchFit(formula = ~arma(2,3)+garch(1,2), data=current.data.diff,cond.dist = c("QMLE"), include.mean=TRUE)

## --BUILD GARCH ON RESIDUALS--
garch.fit = garchFit(formula = ~arma(0,0)+garch(1,1), data=arima.fit$residuals,cond.dist = c("QMLE"), include.mean=TRUE)

## --CHECK OF GARCH ADEQUACY--
meanParams = 9
volParams =3

sresi=garch.fit$residuals/garch.fit$condvars^.5
sresi=garch.fit@residuals/garch.fit@sigma.t

Box.Ljung.test(sresi, lag = 12, adj.DF =  12-meanParams)  
Box.Ljung.test(sresi^2, lag = 12, adj.DF = 12-volParams ) 

shapiro.test(sresi)

## --Plots
plot(garch.fit$residuals, type = "l")
qqnorm(sresi) ; qqline(sresi)
acf(sresi)
acf(sresi^2)

## --Simulate & Plot Normal Data--#
sim.data = rnorm(662, mean = 0, sd = 1)

qqnorm(sim.data, main = "Normal Q-Q Plot of 662 Simulated Values from N(0,1)"); qqline(sim.data)

plot(density(sim.data), xlab = "Simulated Value", main = "Density Plot for Simulated Data, 662 Observations")
x <- seq(min(sim.data),max(sim.data),length=100)
hx <- dnorm(x,mean(sim.data),sd(sim.data))
lines(x,hx, lty = 2, col = 2)

## --Simulate & Test Normal Data--#
sim.data = rnorm(134, mean = 0, sd = 1)

ksnormTest(sim.data)
shapiroTest(sim.data)
jarqueberaTest(sim.data)
ad.Test = adTest(sim.data)

sim.data2 = rnorm(662, mean = 0, sd = 1)

ksnormTest(sim.data2)
shapiroTest(sim.data2)
jarqueberaTest(sim.data2)
ad.Test = adTest(sim.data2)

## --Notes--
?boxcox

