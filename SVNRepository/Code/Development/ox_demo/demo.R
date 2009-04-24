library(Rmetrics)
source("../ATS_2008.R")
############################################################################
sp5=scan(file="sp500.txt")
plot(sp5,type='l')
pacf(sp5)
m3=arima(sp5,order=c(3,0,0))
m3

pacf(m3$resid^2)

#
x=ts(sp5)
m4=garchOxFit(formula.mean=~arma(3,0),formula.var=~garch(1,1),series=x)
#
m4=garchOxFit(formula.mean=~arma(0,0),formula.var=~garch(1,1),series=x)
#
sresi=m4$residuals/m4$condvars^.5
qqnorm(sresi) ; qqline(sresi)
#
acf(sresi)
Box.Ljung.test(sresi, lag = 12, adj.DF = NULL)  
Box.Ljung.test(sresi^2, lag = 12, adj.DF = 12-2) 
#
Box.Ljung.test(sresi, lag = 24, adj.DF = NULL)  
Box.Ljung.test(sresi^2, lag = 24, adj.DF = 24-2) 

ts.plot(m4$condvars^.5)
 
