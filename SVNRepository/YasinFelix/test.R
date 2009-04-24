library(Rmetrics)
library(fArma)
library(fracdiff)

setwd("Z:/MEng Project/R Working Directory/")

source("ATS_2008.R")

x = read.table("ky.txt")

#Original Data, Plot, ACF
current.data = as.ts(x[,4])
plot(current.data)
acf(current.data, lag.max = 30)

#Differenced Data, Plot, ACF, PACF
current.data.diff = diff(current.data)
plot(current.data.diff)
acf(current.data.diff,lag.max=30)
pacf(current.data.diff,lag.max=30)

#Fractionally Differenced
d=fdGPH(current.data.diff)$d
d
current.data.diff = diffseries(current.data.diff, d)
Box.Ljung.test(current.data.diff^2,lag = 12, adj.DF = 12-params)



#Plot EACF
eacf(current.data.diff)

#Build ARIMA Model
current.data.fit1 = arima(current.data.diff,order = c(2,0,0))
current.data.fit1

#!!!!!!!!!!!!ADF-Test -> lag based on PACF plot
adfTest(current.data,lags=3,type=c("c"))

#Ljung Box test on Model Resids and Sq. Resids 
params = 2
Box.Ljung.test(current.data.fit1$resid,lag = 12, adj.DF =  12-params)
Box.Ljung.test(current.data.fit1$resid^2,lag = 12, adj.DF =  12-params)


#--BUILD GARCH--#
box.test.pval = Box.Ljung.test(current.data.fit1$resid,lag = 12,adj.DF = 12-params)$p.value
arch.test.pval = Box.Ljung.test(current.data.fit1$resid^2,lag = 12, adj.DF = 12-params)$p.value

garch.fit=garchOxFit(cond.dist = "t", formula.mean=~arma(1,4),formula.var=~garch(2,1),series=current.data.diff)
?garchOxFit
sresi=garch.fit$residuals/garch.fit$condvars^.5
 
Box.Ljung.test(sresi, lag = 12, adj.DF =  12-params)  
Box.Ljung.test(sresi^2, lag = 12, adj.DF = 12-params) 

#--Plots
plot(garch.fit$residuals, type = "l")
qqnorm(sresi) ; qqline(sresi)
acf(sresi)
acf(sresi^2)

