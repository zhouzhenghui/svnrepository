"get.drift.forecast.error" = function(driftmodel,responses,predictors,predictors.forecasts,forecasts,resids,resids.forecasts, alpha){

Y=as.matrix(responses)
P = as.matrix(driftmodel$coefficients)
P=t(P)
X = as.matrix(predictors)
G=1
V = as.matrix(as.numeric(resids))

X.Xt = as.matrix(t(X) %*% X)
N=max(dim(X))
K=min(dim(X))
se= NULL
forecasts.number = length(as.matrix(forecasts))
for (i in 1:forecasts.number){
YstarF = as.matrix(forecasts)[i]
VF = as.matrix(resids.forecasts)[i]
XF = as.matrix(predictors.forecasts)[i,]


PI = P - t(V) %*% X %*% solve(X.Xt)

YF = -1*((P-PI) %*% XF - t(VF) - t(YstarF))



q= t(XF) %*% solve(X.Xt) %*% XF

Svv = ((t(Y) %*% Y)  - P %*% (t(X)%*%X) %*% t(P))/(N-K)
Sff=(1+q)*Svv
Tsquared = (YF-t(YstarF)) %*% solve(Sff) %*% t(YF-t(YstarF))
stat = (N-G-K+1)/((N-K)*G)*Tsquared
crit = qf((1-alpha/2),G,N-K-G+1)

se=c(se,stat*crit)
}


}