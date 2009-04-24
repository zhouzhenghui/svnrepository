####simulation tests####
##Tim

library(TSA)
library(fGarch)
data = current.state.data.diff
plot(data,type="l")
pacf(data, lag.max = 100)
dev.new()
acf(data^2, lag.max = 100)

x=garchFit(formula=~arma(3,0)+garch(1,1), data, cond.dist = c("norm"))
names(x@fit)
as.list(x@fit$coef)
ar.coeffs = as.numeric(x@fit$coef[which(substr(names(x@fit$coef),1,2)=="ar")])
mu.coeffs = as.numeric(x@fit$coef[which(substr(names(x@fit$coef),1,2)=="mu")])
alpha.coeffs = as.numeric(x@fit$coef[which(substr(names(x@fit$coef),1,2)=="al")])
beta.coeffs = as.numeric(x@fit$coef[which(substr(names(x@fit$coef),1,2)=="be")])
omega.coeffs = as.numeric(x@fit$coef[which(substr(names(x@fit$coef),1,2)=="om")])
current.state.garch.fit=garchOxFit(formula.mean=~arma(3,0),formula.var=~garch(1,1),series=current.state.data.diff)
current.state.garch.fit
spec.VT = garchSpec(model = list(mu= mu, ar = ar.coeffs, alpha =alpha.coeffs, beta = beta.coeffs, omega = omega.coeffs), 
	presample = NULL,cond.dist = c("norm"))

acf(data.sim)
acf(data.sim^2)
pacf(data.sim)
pacf(data.sim^2)
dev.new()
data.sim = garchSim(spec = spec.VT, n = length(data), n.start = 1, extended = FALSE)

plot(as.ts(data.sim[,1]))
