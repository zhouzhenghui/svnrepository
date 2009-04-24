####simulation tests
####tim####

library(Rmetrics)
library(fGarch)
data = current.state.data.diff
current.state.garchoxfit =garchOxFit(formula.mean=~arma(1,1),formula.var=~garch(1,1),series=current.state.data.diff,cond.dist = c("gaussian"))
current.state.garchfit = garchFit(formula = ~arma(1,1)+garch(1,1), data=current.state.data.diff,cond.dist = c("QMLE"), include.mean=TRUE)
sresi=current.state.garchoxfit$residuals/current.state.garchoxfit$condvars^.5

current.state.garchoxfit$coef
current.state.garchfit@fit$coef
qqnorm(sresi) # Obtain normal probability plot (ideal case: a straight line)
qqline(sresi) # Impose a straight line on the QQ-plot.

ar.coefs = as.numeric(current.state.garchfit@fit$coef[which(substr(names(current.state.garchfit@fit$coef),1,2)=="ar")])
ma.coefs = as.numeric(current.state.garchfit@fit$coef[which(substr(names(current.state.garchfit@fit$coef),1,2)=="ma")])
mu.coefs = as.numeric(current.state.garchfit@fit$coef[which(substr(names(current.state.garchfit@fit$coef),1,2)=="mu")])
omega.coefs = as.numeric(current.state.garchfit@fit$coef[which(substr(names(current.state.garchfit@fit$coef),1,2)=="om")])
alpha.coefs = as.numeric(current.state.garchfit@fit$coef[which(substr(names(current.state.garchfit@fit$coef),1,2)=="al")])
beta.coefs = as.numeric(current.state.garchfit@fit$coef[which(substr(names(current.state.garchfit@fit$coef),1,2)=="be")])


#sim.size = length(current.state.data.diff)
sim.size = 1000
current.state.garch.spec = garchSpec(model = list(ar =ar.coefs , ma =ma.coefs,alpha =alpha.coefs, beta = beta.coefs, mu = mu.coefs,omega = omega.coefs), cond.dist = "norm")
current.state.garch.sim = garchSim(spec = current.state.garch.spec, n = sim.size, n.start = 0.2*sim.size, extended = FALSE)



sim.data = as.numeric(current.state.garch.sim)

plot(as.ts(current.state.data.diff))
dev.new()
plot(sim.data,type = "l")


##ACF and PACF of SIM vs OBSERVED##
dev.new()
par(mfrow=c(2,2))
acf(sim.data,main="ACF of Simulated Process", lag.max = 40)
acf(sim.data^2,main="ACF of Simulated Process^2", lag.max = 40)
acf(current.state.data.diff,main="ACF of Observed Process", lag.max = 40)
acf(current.state.data.diff^2,main="ACF of Observed Process^2", lag.max = 40)
dev.new()
par(mfrow=c(2,2))
pacf(sim.data,main="PACF of Simulated Process", lag.max = 40)
pacf(sim.data^2,main="PACF of Simulated Process^2", lag.max = 40)
pacf(current.state.data.diff,main="PACF of Observed Process", lag.max = 40)
pacf(current.state.data.diff^2,main="PACF of Observed Process^2", lag.max = 40)


##burn in period determination##
sim.size = 1000
bucketsize = 10
aggsize = sim.size/bucketsize/2
counter = (1:aggsize)*bucketsize 


current.state.garch.spec = garchSpec(model = list(ar =ar.coefs , ma =ma.coefs,alpha =alpha.coefs, beta = beta.coefs, mu = mu.coefs,omega = omega.coefs), cond.dist = "norm")
current.state.garch.sim = garchSim(spec = current.state.garch.spec, n = sim.size, n.start = 1, extended = FALSE)

	parameters.vector = NULL
for (j in counter){

	current.state.garchfit = garchFit(formula = ~arma(3,0)+garch(1,1), data=as.numeric(current.state.garch.sim)[0:j],cond.dist = c("QMLE"), include.mean=TRUE)	
	parameters.vector = rbind(parameters.vector,as.numeric(current.state.garchfit@fit$coef))

}
dev.new()
par(mfrow=c(dim(parameters.vector)[2],1))
for(i in 1:dim(parameters.vector)[2]){
	dev.new()
	plot(parameters.vector[,i], type = "l")
}

for(i in 1:dim(parameters.vector)[2]){
	dev.new()
	plot(var(parameters.vector[,i]), type = "l")
}

for (j in 1:dim(parameters.vector)[2]{
	for (k in 1:dim(parameters.vector)[1]{

	
	}
}