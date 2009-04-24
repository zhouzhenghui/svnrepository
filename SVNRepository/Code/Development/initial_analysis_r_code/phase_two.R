library(fGarch)
library(forecast)
library(fracdiff)
## --PHASE 2

resid.data = resids

## --Plot EACF
eacf(resid.data)

## --Build ARIMA Model

auto.arima(resid.data, stationary = TRUE)

arima.fit = arima(resid.data,order = c(1,0,0), include.mean = TRUE)
names(arima.fit)

## --LjungBox test on Model Resids and Sq. Resids 
params = 1
acf(arima.fit$resid,lag.max = 40,main="ACF of Phase 2 Residuals")
acf(arima.fit$resid^2,lag.max = 40,main="ACF of Phase 2 Sq-Residuals")
Box.Ljung.test(arima.fit$resid,lag = 12, adj.DF =  12-params)
Box.Ljung.test(arima.fit$resid^2,lag = 12, adj.DF =  12)

##get GPH estimate
fdGPH(resid.data)
## --BUILD GARCH ON RESIDUALS IF NECESSARY



resid.garch = garchFit(formula = ~arma(1,0)+garch(1,1), data=resid.data, include.mean=FALSE)
summary(resid.garch)

## --CHECK OF GARCH ADEQUACY--
meanParams = 1
volParams =2

?garchFit

sresi=resid.garch@residuals/resid.garch@sigma.t
acf(sresi, main = "ACF of Standardized Residuals")
acf(sresi^2, main = "ACF of Sq-Standardized Residuals")
Box.Ljung.test(sresi, lag = 12, adj.DF =  12-meanParams)  
Box.Ljung.test(sresi^2, lag = 12, adj.DF = 12-volParams ) 

pacf(diff(sresi), lag.max=40)
adf.test(sresi, k = 3)

## --LM MODEL
lm.reg = lm(formula = form, data = shifted.multivar.test.data.vector)

acf(lm.reg$residuals, lag = 40)

Box.test(lm.reg$residuals, lag = 12, type = c("Ljung-Box"))
adf.test(lm.reg$residuals, k = 7)
?adf.test
pacf(diff(lm.reg$residuals))

summary(lm.reg)
names(summary(lm.reg))

plot(lm.reg$fitted.values, type="l")
n = ncol(shifted.multivar.test.data.vector)
lines(shifted.multivar.test.data.vector[,n],col="red")