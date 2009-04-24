setwd("Z:/SVNRepository/Code/Development/fractional integration test code/")  # Set working directory for R

library(Rmetrics)               # load the fSeries package
library(sandwich)
library(fracdiff)
library(GeneCycle)
library(fArma)
source("../ATS_2008.R")


#read in state data
all.data = read.table("../../../Data/States/all.txt")
all.data.state = all.data[,1]
states = read.csv("../../../Data/States/states.csv")

#select an individual state here
i=1
state.names = "KY"

#make appropriate transformations and select data
current.state.name = as.matrix(state.names)[i]
current.state.data = subset(all.data,all.data[,1]==current.state.name)[,4]
current.state.data.diff = diff(current.state.data)
current.state.acf = acf(current.state.data.diff,lag.max = 30)

n=length(current.state.data.diff)
sample.size=1:n

#try second difference
pacf(diff(current.state.data.diff), lag.max = 40)
#get acf of first difference series
acf.lags = 40

current.state.data.diff.acf = acf(current.state.data.diff,lag.max = acf.lags)

#log-log acf
lags.count=2:(acf.lags+1)
reg.line=lm(current.state.data.diff.acf$acf[2:length(current.state.data.diff.acf$acf)]~lags.count)
reg.line.log=lm(log(abs(current.state.data.diff.acf$acf[2:length(current.state.data.diff.acf$acf)]))~log(lags.count))

plot(lags.count,abs(current.state.data.diff.acf$acf[2:length(current.state.data.diff.acf$acf)]),log="xy", main=paste("log-log acf for",current.state.name,"slope=",round(reg.line.log$coefficients[2],2)),ylab="log acf", xlab = "log lag")
abline(reg.line.log)
#periodogram

#acf(log((current.state.data.diff[2:length(current.state.data.diff)]-current.state.data.diff[1:length(current.state.data.diff)-1])/current.state.data.diff[1:length(current.state.data.diff)]+1), lag.max = 400)
#acf(diff(current.state.data.diff),lag.max = 400)
spec=periodogram(current.state.data.diff,method="builtin")$spec*length(periodogram(current.state.data.diff,method="builtin")$spec)/4
freq=periodogram(current.state.data.diff,method="builtin")$freq*2*pi
plot(freq,spec,log="xy",xlab="log frequency",ylab="log periodogram")
periodogram.reg.log.line=lm(log(spec)~log(freq))
periodogram.reg.line=lm(spec~freq)
abline(periodogram.reg.line)
#periodogram

#acf(log((current.state.data.diff[2:length(current.state.data.diff)]-current.state.data.diff[1:length(current.state.data.diff)-1])/current.state.data.diff[1:length(current.state.data.diff)]+1), lag.max = 400)
#acf(diff(current.state.data.diff),lag.max = 400)
spec=periodogram(current.state.data.diff,method="builtin")$spec*length(periodogram(current.state.data.diff,method="builtin")$spec)/4
freq=periodogram(current.state.data.diff,method="builtin")$freq*2*pi
plot(freq,spec,log="xy",xlab="log frequency",ylab="log periodogram")

#plots
plot(lags.count,abs(current.state.data.diff.acf$acf[2:length(current.state.data.diff.acf$acf)]),log="xy")
reg.line=lm(current.state.data.diff.acf$acf[2:length(current.state.data.diff.acf$acf)]~lags.count)
reg.line.log=lm(log(abs(current.state.data.diff.acf$acf[2:length(current.state.data.diff.acf$acf)]))~log(lags.count))
abline(reg.line)
reg.line.log$coefficients
reg.line$coefficients

dev.new()
rs.fit=rsFit(current.state.data.diff, levels = 20, minnpts = 1, doplot = TRUE, trace = TRUE, title = NULL, description = NULL)

whittle.fit = whittleFit(current.state.data.diff, order = c(0, 0), subseries = 1, method = c("farma"),trace = TRUE, spec = FALSE, title = NULL, description = NULL)

whittle.fit2 = whittleFit(current.state.data.diff, order = c(0, 0), subseries = 1, method = c("fgn"),trace = TRUE, spec = FALSE, title = NULL, description = NULL)
whittle.fit@hurst$H+sqrt(whittle.fit@hurst$sigma2)
whittle.fit
hurstSlider(current.state.data.diff)
length(current.state.data.diff)

#d = fdGPH(current.state.data.diff)$d
d = fdSperio(current.state.data.diff)$d

try = diffseries(current.state.data.diff,d)
Box.Ljung.test(try ,lag= 12, adj.DF = 12)
Box.Ljung.test(try^2 ,lag= 12, adj.DF = 12)

pacf(try)
acf(try)

try.arma = arima(current.state.data.diff,order=c(0,0,3))
Box.Ljung.test(try.arma$resid,lag= 12, adj.DF = 12-3)
Box.Ljung.test(try.arma$resid^2,lag= 24, adj.DF = 24)


current.state.garch.fit=garchOxFit(formula.mean=~arma(3,1),formula.var=~garch(1,1),series=try)
sresi=current.state.garch.fit$residuals/current.state.garch.fit$condvars^.5


pacf(try)
acf(try)
Box.Ljung.test(sresi,lag= 12, adj.DF = 12-4)
Box.Ljung.test(sresi^2,lag= 12, adj.DF = 12-2)
plot(try.arma$resid)
