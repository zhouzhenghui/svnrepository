library("fracdiff")
source("../source_functions/fractional.diff.est.sim.R")
source("../source_functions/rand.select.R")
nile=as.ts(read.table("../../../Data/Nile/nile.dat"))
gaussian.test.data = rnorm(n=134, mean = 0, sd = 1)
rand.select(data)
plot(nile,type = "l")
	nile.sreturn = (nile[2:length(nile)]-nile[1:(length(nile)-1)])/nile[1:(length(nile)-1)]
plot(nile.sreturn,type = "l")


#simulation
sim.results = fractional.diff.est.sim(state.name="nile", data = nile,arma.order = c(0,0,0),n.to.use = length(nile), no.of.sims=100, include.model = FALSE, method = "GPH", resample = TRUE, alpha = 0.05)
sim.results = fractional.diff.est.sim(state.name="nile", data = nile,arma.order = c(0,0,0),n.to.use = length(data), 
						no.of.sims=1000, include.model = FALSE, method = "WhittleFARIMA", 
						alpha = 0.05, sim = TRUE, doplot=TRUE,resample=TRUE, burn.in=200)


nile.test = nile.sreturn

#Normality Test
ksnormTest(nile.test)
shapiroTest(nile.test)
jarqueberaTest(nile.test)
ad.Test = adTest(nile.test)
dagoTest(nile.test)
lillieTest(nile.test)

armaFit(

d = fdGPH(nile.test)
d.est = d$d
d.sperio=fdSperio(nile.test)$d
nile.diffseries  = diffseries(nile.test,d.est)

d.new.GPH = fdGPH(nile.diffseries)
d.new.Sperio = fdSperio(nile.diffseries)


d.est.new.GPH = d.new.GPH$d
d.est.new

nile.hurst=diffseries(nile.test,0.3991992)


whittle.fit = whittleFit(nile.hurst, order = c(0, 0), subseries = 1, method = c("farma"),trace = TRUE, spec = FALSE, title = NULL, description = NULL)

whittle.fit2 = whittleFit(nile.hurst, order = c(0, 0), subseries = 1, method = c("fgn"),trace = TRUE, spec = FALSE, title = NULL, description = NULL)


par(mfrow=c(1,2))
#
plot(density(nile.test), xlab = "simple return", main = "")
x <- seq(min(nile.test),max(nile.test),length=100)
hx <- dnorm(x,mean(nile.test),sd(nile.test))
lines(x,hx, lty = 2, col = 2)
#


plot(density(lnnile.test), xlab = "log return", main = "")
x <- seq(min(lnnile.test),max(lnnile.test),length=100)
hx <- dnorm(x,mean(lnnile.test),sd(lnnile.test))
lines(x,hx, lty = 2, col = 2)
#
dev.new()

qqnorm(nile.test) # Obtain normal probability plot (ideal case: a straight line)
qqline(nile.test) # Impose a straight line on the QQ-plot.


