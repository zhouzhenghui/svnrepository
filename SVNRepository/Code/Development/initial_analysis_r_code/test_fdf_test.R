library(fracdiff)
library(fArma)
source("../source_functions/fcritical.R")


source("../source_functions/fadf_test.R")
n.to.use=134
d.hat=0.25
dhat.vec=c(-0.49,-0.4,-0.3,-0.2,-0.1,0,.1,0.2,0.3,0.4,0.49)
burn.in=200
lags = 1
numofsims=200
precentpassed=NULL
i=7
for(i in 1:length(dhat.vec)){
	numbergood=	0
	##mcsim
	for (j in 1:numofsims){
		#sim.data = as.numeric(rnorm(n.to.use,0,1))
		d.hat = dhat.vec[i]
		sim.data = as.numeric(fracdiff.sim(n.to.use, ar = NULL, ma = NULL, d = d.hat)$series)
		
		#sim.data = as.numeric(as.matrix(armaSim(model = list(ar = 0, ma = 0, d = d.hat), n =n.to.use, n.start = burn.in,rand.gen = rnorm)));			
		critval =	abs(fdfcritical(0,lags ,n.to.use,d.hat,1)$fivepercent)
		
		
		tstat = as.numeric(abs(fadf.test(sim.data, alternative = c("stationary"), k = lags, dparam=sample(dhat.vec,1))$statistic))	
		numbergood=	numbergood + ifelse(tstat>=critval,1,0)
	}
	precentpassed[i]=numbergood/numofsims
}

percentpassed_dsim = precentpassed
percentpassed_rnormsim= precentpassed

percentpassed_unitrootsim= precentpassed


plot(dhat.vec,percentpassed_dsim, ylim=c(0,1),pch=2,col="blue",xaxt="n", xlab = "d-parameters",ylab = "Percentage Passed", main="Fractional Dickey Fuller Test")
axis(side=1,at=dhat.vec ,las=3, cex.axis = 0.9)
points(dhat.vec,percentpassed_unitrootsim,col="black",pch=4)
legend("bottomleft", c("d (-0.5,0.5) sim","d=1 sim"), pch=c(2,4),col=c("blue","black"))


dhat.vec=seq(from = -0.49, to = 0.49, length.out = 100)
x=NULL
y=NULL
for(i in 1:length(dhat.vec)){

	d.est.vec=NULL
	for (j in 1:numofsims){
		#sim.data = as.numeric(rnorm(n.to.use,0,1))
		d.hat = dhat.vec[i]
		sim.data = as.numeric(fracdiff.sim(n.to.use, ar = NULL, ma = NULL, d = d.hat)$series)
		
		#sim.data = as.numeric(armaSim(model = list(ar = 0, ma = 0, d = d.hat), n =n.to.use, n.start = burn.in,rand.gen = rnorm));			
		d.est.vec = c(d.est.vec ,whittleFit(sim.data, order = c(0, 0), subseries = 1, method = c("farma"),trace = TRUE, spec = FALSE, title = NULL, description = NULL)@hurst$H  - 0.5)
		
)
	}
	x=c(x,rep(d.hat,numofsims))
	y=c(y,d.est.vec)
}

plot(x,y,main="GPH estimator for T=134",xlab="Specified d",ylab="Estimated d")
abline(lm(y~x),col="white",lwd=3)

##compute the means
mean=NULL
ninetyfiveupper =NULL
ninetyfivelower =NULL
i=1
for (i in 1:length(dhat.vec)){
mean[i]=mean(y[which(x==dhat.vec[i])])
quantiles = quantile(y[which(x==dhat.vec[i])], probs = c(0.025, 0.975),type=7)

ninetyfiveupper = c(ninetyfiveupper ,quantiles[2])
ninetyfivelower = c(ninetyfivelower ,quantiles[1])
}


ylimits = c(min(y),max(y))
plot(x,y,main="GPH estimator for T=134",sub="100 simulations for each value of d",,xlab="Specified d",ylab="Estimated d",ylim=ylimits,col="blue",type="h")
par(new=T)
#plot(dhat.vec,mean,col="white", pch=3,xaxt="n",yaxt="n",xlab="",ylab="",ylim=ylimits)
#par(new=T)

errbar(dhat.vec,mean,ninetyfiveupper ,ninetyfivelower ,xaxt="n",xlab = "",ylab="",  lty = 1,lwd=2, pch = 4, ylim = ylimits,cap = 0.005,col="black")


par(new=T)
errbar(state.numbers,stacked.d.stars,stacked.d.stars+stacked.d.stars.ses,stacked.d.stars-stacked.d.stars.ses,xaxt="n",ylab="d-estimate",xlab="",main="Fractional Difference Parameter Estimate", col="blue", lty = 2, pch = 2,lwd=3, ylim = c(-1,1),cap = 0.1)

abline(lm(y~x),col="white",lwd=3)

lm(y~x)

