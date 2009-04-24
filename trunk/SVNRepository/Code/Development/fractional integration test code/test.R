setwd("C:/Users/tim/Documents/ClassesSpring09/MastersProject/RCode/FractionalDifferencingTest/")  # Set working directory for R

library(Rmetrics)               # load the fSeries package
library(sandwich)
library(fracdiff)
library(quantreg)
library(GeneCycle)
library(ggplot2)
library(fBasics)
library(fArma)
library(fNonlinear)


source("../ATS_2008.R")




nile=as.ts(read.table("nilet.dat"))
nile.acf = acf(nile,lag.max = 80)
n=length(nile)
sample.size=1:n
lags.count=2:81
#periodogram
periodogram(nile,method="builtin",plot=TRUE)
plot(freq,spec,log="xy",xlab="log frequency",ylab="log periodogram")
spec=periodogram(nile,method="builtin")$spec*length(periodogram(nile,method="builtin")$spec)/4
freq=periodogram(nile,method="builtin")$freq*2*pi

#plots
plot(lags.count,abs(nile.acf$acf[2:length(nile.acf$acf)]),log="xy")
reg.line=lm(nile.acf$acf[2:length(nile.acf$acf)]~lags.count)
reg.line.log=lm(log(abs(nile.acf$acf[2:length(nile.acf$acf)]))~log(lags.count))
abline(reg.line.log)
reg.line.log$coefficients
reg.line$coefficients
names(lm(log(nile.acf$acf[2:length(nile.acf$acf)])~log(lags.count)))
nile.qplot=qplot(lags.count, abs(nile.acf$acf[2:length(nile.acf$acf)]), log="xy") + geom_smooth(method = lm)
names(nile.qplot$facet)

nile.var = NULL
scaled.n=seq(1,n,by=10)
for(i in 1:length(scaled.n)){
  	nile.var = c(nile.var, var(nile[1:scaled.n[i]])/scaled.n[i]*100)
  
	}

plot(scaled.n[1:60],nile.var[1:60], log="xy")
reg.line.log.nile.mean=lm(log(nile.var[1:100])~log(sample.size[1:100]))

rs.fit=rsFit(nile, levels = 20, minnpts = 1, doplot = TRUE, trace = TRUE, title = NULL, description = NULL)

whittle.fit = whittleFit(nile, order = c(0, 0), subseries = 1, method = c("farma"),trace = TRUE, spec = FALSE, title = NULL, description = NULL)
whittle.fit2 = whittleFit(nile, order = c(0, 0), subseries = 1, method = c("fgn"),trace = TRUE, spec = FALSE, title = NULL, description = NULL)
hurstSlider(nile)
length(nile)
##demo stuff
t= 1:500;
n=length(t);
x = 2*cos(2*pi*t/50 + 0.6*pi) + rnorm(500,0,5);
I = abs(fft(x)/sqrt(n))^2;
P = (4/n)*I;
f = 0:250/n;
plot(f,I[1:251],type = "l",xlab="frequency",ylab= " ");
abline(v=seq(0,0.5,0.02), lty="dotted");
