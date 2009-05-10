"get.drift.forecast.error" = function(){

P = driftmodel$coefficients
X = as.matrix(shifted.multivar.test.data.vector[,which(names()
V = as.matrix(resids)
X.Xt = as.matrix(t(X) %*% X)

PI = P - t(V) %*% X %*% solve(X.Xt)

solve(X.Xt)

}