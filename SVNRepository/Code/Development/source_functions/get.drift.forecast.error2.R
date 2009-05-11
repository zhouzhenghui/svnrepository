"get.drift.forecast.error2" = function(driftmodel,responses,predictors,predictors.forecasts,forecasts,resids,resids.forecasts, alpha){

Y=as.matrix(responses)
P = as.matrix(driftmodel$coefficients)
P=t(P)
X = as.matrix(predictors)
G=1
V = as.matrix(as.numeric(resids))

X.Xt = as.matrix(t(X) %*% X)
N=max(dim(X))
K=min(dim(X))

SSj = t(V) %*% V
DFj = (N-K)
MSj= SSj  / DFj

se= NULL
forecasts.number = length(as.matrix(forecasts))
for (i in 1:forecasts.number){
YstarF = as.matrix(forecasts)[i]
VF = as.matrix(resids.forecasts)[i]
XF = as.matrix(predictors.forecasts)[i,]

stuff = t(XF) %*% solve(X.Xt) %*% XF

error.margin = qt((1-alpha),N-2)*sqrt(MSj*(1+stuff))
se=c(se,error.margin)
}


}