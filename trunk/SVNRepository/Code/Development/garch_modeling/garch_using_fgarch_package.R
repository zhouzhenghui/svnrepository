####simulation tests
####tim####

library(Rmetrics)
library(fGarch)
data = current.state.data.diff
###MAKE SURE TO USE SIMPLE RETURNS####
current.state.garchfit = garchFit(formula = ~arma(1,1)+garch(1,1), data=current.state.data.diff,cond.dist = c("QMLE"), include.mean=TRUE)
