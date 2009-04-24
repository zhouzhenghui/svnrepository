source("../source_functions/UnitRootNormality.R")
source("../source_functions/automodel.R")
source("../source_functions/temporal_holdout.R")
source("../source_functions/interpolate.R")
source("../source_functions/cutoff.R")
source("../source_functions/fadf_test2.R")

current.state.name="AK"
current.state.data = subset(all.data,all.data[,1]==current.state.name)[,4]
cutoff.data = cutoff(current.state.data, method="constant", constant=40)
interpolated.data = interpolate(cutoff.data,method="linear")
current.state.data = interpolated.data
current.state.data.sreturn = (current.state.data[2:length(current.state.data)]-current.state.data[1:(length(current.state.data)-1)])/current.state.data[1:(length(current.state.data)-1)]

##fractional (garch) modeling
unit.root.test(current.state.data.sreturn)
acf(current.state.data.sreturn,lag.max=40,main="ACF of Simple Returns")
acf(current.state.data.sreturn^2,lag.max=40,main="ACF of Squared Simple Returns")
pacf(current.state.data.sreturn,lag.max=40)
pacf(current.state.data.sreturn^2,lag.max=40)
d.est = fdGPH(as.ts(current.state.data.sreturn))$d
d.est = fdSperio(as.ts(current.state.data.sreturn))$d
d.est = whittleFit(as.ts(current.state.data.sreturn), order = c(0, 0), subseries = 1, method = c("farma"),trace = TRUE, spec = FALSE, title = NULL, description = NULL)@hurst$H  - 0.5
state.data.sreturn.fracdiff = diffseries(current.state.data.sreturn,d.est)
acf(state.data.sreturn.fracdiff,lag.max=40,main="ACF of Fractionally Differenced Simple Returns",sub=paste("d=",round(d.est,2),sep=""))
acf(state.data.sreturn.fracdiff^2,lag.max=40,main="ACF of Squared Fractionally Differenced Simple Returns",sub=paste("d=",round(d.est,2),sep=""), cex.main = 0.8)

eacf(state.data.sreturn.fracdiff)
eacf(current.state.data.sreturn)

acf(state.data.sreturn.fracdiff^2,lag.max=40)
pacf(state.data.sreturn.fracdiff,lag.max=40)
pacf(state.data.sreturn.fracdiff^2,lag.max=40)
current.fixed = c(0,NA,NA,0,NA,NA,NA,NA,NA)
trial_arima=arima(current.state.data.sreturn,order=c(6,0,2), fixed = current.fixed)
current.fixed = c(0,NA,NA,0,NA,NA,NA,NA,NA)
trial_arima=arima(state.data.sreturn.fracdiff,order=c(1,0,0))
acf(trial_arima$residuals,lag.max=40,main="ACF of ARMA(1,0) Residuals")

acf(trial_arima$residuals^2,lag.max=40,main="ACF of ARMA(1,0) Squared Residuals")
pacf(trial_arima$residuals,lag.max=40)
pacf(trial_arima$residuals^2,lag.max=40)
eacf(current.state.data.sreturn)
Box.Ljung.test(trial_arima$residuals,12,10)
Box.Ljung.test(trial_arima$residuals^2,12,12)

x=as.ts(current.state.data.sreturn)

x=as.ts(state.data.sreturn.fracdiff)

x=as.ts(trial_arima$residuals)*1000
model.temp = garchFit(formula = ~arma(1,0)+garch(3,1), data=x, cond.dist = c("QMLE"), include.mean=TRUE, trace=FALSE);
sresi=model.temp@residuals/model.temp@sigma.t
acf(sresi,lag.max=40,main="ACF of ARMA(1,0)+GARCH(3,0) Standardized Residuals")

acf(sresi^2,lag.max=40,main="ACF of ARMA(1,0)+GARCH(3,0) Squared Standardized Residuals")

Box.Ljung.test(sresi,12,11)
Box.Ljung.test(sresi^2,12,8)

length(current.state.data.sreturn)
##end fractional garch modeling

m3=NULL
m3=garchOxFit(formula.mean=~arma(0,3),formula.var=~figarch.bbm(1,1),series=as.numeric(x))
sresi=m3$residuals/m3$condvars^.5
Box.Ljung.test(sresi,12,9)
Box.Ljung.test(sresi^2,12,12)
acf(sresi,lag.max=40)
acf(sresi^2,lag.max=40)

par(mfrow=c(3,1))
plot(as.ts(current.state.data.sreturn))
plot(as.ts(state.data.sreturn.fracdiff))
plot(as.ts(trial_arima$residuals))


temp.holdout = temporal.holdout("AK",.9,TRUE,TRUE)

Box.Ljung.test((model.temp@residuals/model.temp@sigma.t)^2,12,10)

cutoff.data = cutoff(current.state.data, method="constant", constant=40)
model.temp = garchFit(formula = ~arma(1,3)+garch(1,1), data=current.state.data.sreturn, cond.dist = c("QMLE"), include.mean=TRUE, trace=FALSE);


#sim.data = as.numeric(fracdiff.sim(135, ar = 0, ma =3 , d = 0.22)$series)
sim.data = as.numeric(as.matrix(armaSim(model = list(ar = 0, ma = 3, d = 0.22), n =135, n.start = 0,rand.gen = rnorm)));			
sim.data[1]=mean(sim.data)
sim.data.cutoff = cutoff(sim.data, method="constant", constant=40)
sim.data.interpolated = interpolate(sim.data.cutoff,method="linear")

sim.data.transformed.simple.returns = (sim.data.interpolated[2:length(sim.data.interpolated)]-sim.data.interpolated[1:(length(sim.data.interpolated)-1)])/sim.data.interpolated[1:(length(sim.data.interpolated)-1)]

sim.data.raw.simple.returns = (sim.data[2:length(sim.data)]-sim.data[1:(length(sim.data)-1)])/sim.data[1:(length(sim.data)-1)]

par(mfrow=c(2,1))
acf(as.ts(sim.data.transformed.simple.returns^2), main = "Cutoff and Interpolated Simulated Returns",lag.max=40)
#acf(diff(current.state.data.sreturn,lag=4))
acf(sim.data.raw.simple.returns^2, main = "Raw Simulated Returns",lag.max=40)

plot(sim.data,type="l")
plot(current.state.data,type="l")
acf(sim.data)
acf(current.state.data)

par(mfrow=c(2,1))
acf(diff(current.state.data.sreturn), main = "Cutoff and Interpolated AL Returns", lag.max = 240)
#acf(diff(current.state.data.sreturn,lag=1))
acf(diff(current.state.data.sreturn2,lag=4), main = "Raw AL Returns", lag.max = 240)


sresi = model.temp@residuals/model.temp@sigma.t
Box.Ljung.test(sresi,12,8)
Box.Ljung.test(sresi,12,10)


plot(as.ts(current.data))
current.data=cutoff.data
interpolated.data = interpolate(current.data,method="total_spline")
par(mfrow=c(1,2))
plot(as.ts(interpolated.data))
plot(as.ts(current.data))
#current.state.data=cutoff.data
#current.state.data=interpolated.data
acf(current.state.data.sreturn)
current.data=interpolated.data
plot(as.ts(current.state.data.sreturn))
current.state.data.sreturn = (current.state.data[2:length(current.state.data)]-current.state.data[1:(length(current.state.data)-1)])/current.state.data[1:(length(current.state.data)-1)]

par(mfrow=c(1,2))

plot(as.ts(current.state.data.sreturn2))
plot(as.ts(current.state.data.sreturn))

 current.state.data.sreturn
 plot(as.ts(current.state.data.sreturn))
 eacf(as.ts(current.state.data.sreturn))
#acf(diff(current.state.data.sreturn,lag=2,differences=1))
eacf(as.ts(current.state.data.sreturn))

unit.root.test(current.state.data.sreturn)

unit.root.test(current.state.data.sreturn)
acf(diff(current.state.data.sreturn,lag=4))


trial_seasonal=diff(current.state.data.sreturn,lag=2,differences=1)
eacf(trial_seasonal)

current.fixed=c(NA,NA,0,NA,NA)
trial_arima=arima(current.state.data.sreturn,order=c(2,0,0),seasonal=list(order=c(2,0,0)), fixed = current.fixed)


acf(trial_arima$residuals)
Box.Ljung.test(trial_arima$residuals,lag=11,11)

Box.Ljung.test(trial_arima$residuals,lag=11,11)
Box.Ljung.test(as.ts(current.state.data.sreturn),lag=12,12)
pacf(trial_seasonal)


d.est = whittleFit(as.ts(current.state.data.sreturn), order = c(0, 2), subseries = 1, method = c("farma"),trace = TRUE, spec = FALSE, title = NULL, description = NULL)@hurst$H  - 0.5
d.est = whittleFit(as.ts(model$model$residuals), order = c(0, 0), subseries = 1, method = c("farma"),trace = TRUE, spec = FALSE, title = NULL, description = NULL)@hurst$H  - 0.5
d.est = fdGPH(as.ts(model$model$residuals))$d
pacf(diffseries(as.ts(model$model$residuals),d.est))
eacf(diffseries(as.ts(current.state.data.sreturn),d.est))

arfima_trial= arima(current.state.data.sreturn,order=c(3,0.1243945,3))

pacf(arfima_trial$residuals)
Box.Ljung.test(arfima_trial$residuals,lag=12,7)

eacf(current.state.data.sreturn)
Box.Ljung.test(model$model$residuals,lag=12,10)
 acf(model$model$residuals)
 pacf(model$model$residuals^2)

 model = auto.model("AK",1,TRUE,TRUE)




Box.Ljung.test((model$model@residuals/model$model@sigma.t)^2,lag=12,10)
Box.Ljung.test((model$model@residuals/model$model@sigma.t),lag=12,9)
Box.Ljung.test(model$model$residuals,lag=12,10)
Box.Ljung.test(current.state.data.sreturn,lag=12,12)

pacf(model$model$residuals)

acf((model$model@data), main = "ACF of Interpolated Data")
dev.new()
acf((model$model@data)^2, main =  "ACF of Interpolated Data^2")
dev.new()
acf((model$model@residuals/model$model@sigma.t)^2,main= "ACF of Squared Standarized Residuals")
dev.new()
acf((model$model@residuals/model$model@sigma.t),main= "ACF of Standarized Residuals")

Box.Ljung.test(tester$residuals,lag=12,9)


unit.root.test(model$model$residuals^2)

unit.root.test(current.data)

fadftest = fadf.test(current.state.data.sreturn, lags = 4, type = c("c"), title = NULL,  description = NULL, dparam = 0) 
critval =	abs(fdfcritical(1,lags=4 ,134,d.est,1)$fivepercent)

m4=NULL
m4=
x=ts(current.state.data.sreturn)
m3=garchOxFit(formula.mean=~arma(1,3),formula.var=~garch(1,1),series=x)
#
m4=garchOxFit(formula.mean=~arma(0,0),formula.var=~garch(1,1),series=x)
#
sresi=m4$residuals/m4$condvars^.5

###temporal holdout
current.state.name = "AR"
nahead = 1
percentage = 0.8
temp.holdout=temporal.holdout(current.state.name,percentage ,TRUE,TRUE,  nahead)
predictions = NULL
se = NULL
cond.sd = NULL
predictions = rep(NA,temp.holdout$n+nahead )
se = rep(NA,temp.holdout$n+nahead )
cond.sd = rep(NA,temp.holdout$n+nahead )
meantests = rep(NA,temp.holdout$n)
voltests = rep(NA,temp.holdout$n )


predictions = c(predictions,temp.holdout$mean.predictions)
se = c(se,temp.holdout$stderrs)
cond.sd = c(cond.sd ,temp.holdout$cond.sd)
meantests  = c(meantests  ,temp.holdout$meantests)
voltests  = c(voltests,temp.holdout$voltests)

xlimits=c((temp.holdout$n-10),length(temp.holdout$x)+nahead)


lowerbd = min(min((predictions-se)[xlimits[1]:xlimits[2]], na.rm=TRUE),min(as.ts(temp.holdout$x)[xlimits[1]:xlimits[2]], na.rm=TRUE))
upperbd = max(max((predictions+se)[xlimits[1]:xlimits[2]], na.rm=TRUE),max(as.ts(temp.holdout$x)[xlimits[1]:xlimits[2]], na.rm=TRUE))
lowerbd = ifelse(lowerbd<0,lowerbd*1.2,lowerbd*0.8)
upperbd = ifelse(upperbd <0,upperbd *0.8,upperbd *1.2)

ylimits = c(lowerbd , upperbd)
par(mfrow=c(3,1))
par(mar=(c(5*0.4, 4, 4*0.3, 2*0.3) + 0.1))
#predictions plot
plot(as.ts(temp.holdout$x),xlim=xlimits, ylim = ylimits,ylab="Simple returns",main=paste(current.state.name,"Temporal Holdout Forecasting, RMSE =",round(temp.holdout$rmse,5)), xlab = paste(nahead, " step ahead predictions starting with ",round(percentage*100,0),"% of data",sep="") )
lines(predictions,col="blue")
lines(predictions+se,col="blue",lty=2)
lines(predictions-se,col="blue",lty=2)
legend("bottomleft",c("Observed","Prediction","Standard Error"),lty=c(1,1,2), col = c("black","blue","blue"), cex=0.7)


lowerbd = min((cond.sd)[xlimits[1]:xlimits[2]], na.rm=TRUE)
upperbd = max((cond.sd)[xlimits[1]:xlimits[2]], na.rm=TRUE)
lowerbd = ifelse(lowerbd<0,lowerbd*1.2,lowerbd*0.8)
upperbd = ifelse(upperbd <0,upperbd *0.8,upperbd *1.2)
ylimits2 = c(lowerbd , upperbd)
par(mar=(c(5*0.4, 4, 4*0.3, 2*0.3) + 0.1))
plot(as.ts(cond.sd),xlim=xlimits, ylim = ylimits2,ylab="Conditional Standard Deviation",col="red" )
plot(as.ts(meantests),xlim=xlimits, ylim = c(0,1),ylab="P-Values",main="Box Tests",sub="Lags = 12",pch=1,col="blue", type = "p")
points(as.ts(voltests),pch=2,col="red")
legend("bottomleft",c("Mean","Volatility"),pch=c(1,2), col = c("blue","red"), cex=0.7,bg="white")
par(new=T)
abline(h=0.05,lty=2,col="black")




###end temporal holdout


plot(as.ts(data.temp), xlim=c(340,345),col="blue", ylim=c(225,230),type="b")
lines(as.ts(data.temp2),type="b")
lines(as.ts(data.temp3),type="b",col="red")