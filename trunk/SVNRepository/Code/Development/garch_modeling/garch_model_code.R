#####START HERE FOR GARCH####
#Tim 
#Created 1-10-09

library(Rmetrics)
library(fArma)
library(fracdiff)


setwd("Z:/SVNRepository/Code/Development/garch_modeling/")
all.data = read.table("all.txt")
all.data.state = all.data[,1]
states = read.csv("states.csv")

i=1
state.names = "ME"
number.of.params = 4
current.state.name = as.matrix(state.names)[i]
current.state.data = subset(all.data,all.data[,1]==current.state.name)[,4]
current.state.data.diff = diff(current.state.data)

plot(current.state.data.diff,type= "l")
current.state.fit = arima(current.state.data.diff, order = c(4,0,0))
current.state.box.test.pval = Box.Ljung.test(current.state.fit$resid,lag = 12,adj.DF = 12-number.of.params)$p.value
current.state.arch.test.pval = Box.Ljung.test(current.state.fit$resid^2,lag = 12, adj.DF = 12-number.of.params)$p.value
current.state.garch.fit=garchOxFit(formula.mean=~arma(4,0),formula.var=~garch(1,1),series=current.state.data.diff)
sresi=current.state.garch.fit$residuals/current.state.garch.fit$condvars^.5

qqnorm(sresi) ; qqline(sresi)
#
acf(sresi)
acf(sresi^2)
Box.Ljung.test(sresi, lag = 12, adj.DF =  12-number.of.params)  
Box.Ljung.test(sresi^2, lag = 12, adj.DF = 12-number.of.params) 
plot(current.state.data.diff,type = "l")
plot(current.state.garch.fit$residuals,type = "l")
 dev.new()

