##simulation of farima process##
##Tim

library(Rmetrics)
library(fArma)
library(fracdiff)

plot(current.state.data.diff, type = "l")

d = fdGPH(current.state.data.diff)
d.est = d$d
d.ase = d$sd.as

current.state.diffseries = diffseries(current.state.data.diff,d.est)
current.state.arfima = arima(current.state.diffseries, order = c(0,0,1))
sim.data = as.numeric(armaSim(model = list(ma = c(0.3049), d = 0.5000001), n =1000, n.start = 1))
length(sim.data)
plot(as.numeric(sim.data), type = "l")

armaFit(~arma(0,1),x = current.state.diffseries)
##ACF and PACF of SIM vs OBSERVED##
dev.new()
par(mfrow=c(2,2))
acf(sim.data,main="ACF of Simulated process", lag.max = 100)
acf(sim.data^2,main="ACF of Simulated process^2", lag.max = 100)
acf(current.state.data.diff,main="ACF of Observed process", lag.max = 100)
acf(current.state.data.diff^2,main="ACF of Observed process^2", lag.max = 100)

dev.new()
par(mfrow=c(2,2))
pacf(sim.data,main="PACF of Simulated process", lag.max = 100)
pacf(sim.data^2,main="PACF of Simulated process^2", lag.max = 100)
pacf(current.state.data.diff,main="PACF of Observed process", lag.max = 100)
pacf(current.state.data.diff^2,main="PACF of Observed process^2", lag.max = 100)




