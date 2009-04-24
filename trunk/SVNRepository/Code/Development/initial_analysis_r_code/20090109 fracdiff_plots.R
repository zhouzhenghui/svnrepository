#Tim 
#Created 1-10-09
####INITIALIZATION--RUN THIS FIRST#####
library(Rmetrics)
library(fArma)
library(fracdiff)
library(GeneCycle)
library(tseries)
library(car)
library(Hmisc)
#RH453
setwd("Z:/SVNRepository/Code/Development/initial_analysis_r_code/")
#Home
#setwd("C:/Users/tim/Documents/ClassesSpring09/MastersProject/SVNRepository/timr_FinEngProj09/Code/Development/initial_analysis_r_code")


#mattesons class file, revised box.ljung.test to give p-val
source("../ATS_2008.R")

#our own project source files
source("../source_functions/initialplots.R")
source("../source_functions/aicbicadfTest.R")
source("../source_functions/completearmamodelwithstats.R")
source("../source_functions/UnitRootNormality.R")
source("../source_functions/fractional.diff.est.sim.R")
source("../source_functions/return_box_cox.R")


all.data = read.table("../../../Data/States/all.txt")
all.data.state = all.data[,1]
states = read.csv("../../../Data/States/states2.csv")

all.data.state = all.data[,1]
state.names = as.matrix(states$State)
state.ar.orders = as.matrix(states$AR.order)
state.ma.orders = as.matrix(states$MA.order)
state.d.orders = as.matrix(states$d)
state.alpha.orders = as.matrix(states$alpha.order)
state.beta.orders = as.matrix(states$beta.order)
state.unit.roots = as.matrix(states$unitroot)


#########################################
###########Set test parameters here######
#lag to use in all Box Jenkin's tests
lags.to.use = 12;
#adjust d.o.f. (TRUE/FALSE)
adjust.dof = TRUE;
#significance of tests
alpha = 0.05;
##Loop that runs through states starts here
##############################
d.vector = NULL
unit.root.test.results = NULL
normality.test.results = NULL
box.cox.test.results = NULL
box.cox.test.results.params =NULL
box.cox.test.results.pvals = NULL
all.p.values = NULL
whittle.farima.d.est.sim.resuts.total = NULL
whittle.fgn.d.est.sim.resuts.total = NULL
GPH.d.est.sim.resuts.total = NULL
Sperio.d.est.sim.resuts.total = NULL
####END INITIALIZATION#####
for (i in 1:length(as.matrix(state.names))){
#use following to select just 1 state
state.num = 4
#for (i in 1:3){
	current.state.name = as.matrix(state.names)[i]
	current.state.data = subset(all.data,all.data[,1]==current.state.name)[,4]
	current.state.data.diff = diff(current.state.data)
	current.state.data.logreturn = diff(log(current.state.data))
	current.state.data.sreturn = (current.state.data[2:length(current.state.data)]-current.state.data[1:(length(current.state.data)-1)])/current.state.data[1:(length(current.state.data)-1)]
	
	#####SET TEST DATA HERE########
	if (state.unit.roots[i] > 0){
		test.data= diff(current.state.data.sreturn,lag=1,differences=state.unit.roots[i])
	}else{
		test.data =current.state.data.sreturn
	}

	

	#unit.root.test.results = rbind(unit.root.test.results, unit.root.test(test.data))

	#normality.test.results = rbind(normality.test.results,normality.test(test.data))

	#box.cox.test.results = rbind(box.cox.test.results,return.box.cox(test.data))
	
	
	whittle.farima.d.est.sim.results.temp = fractional.diff.est.sim(state.name=current.state.name, data = test.data,arma.order = c(0,0),n.to.use = length(test.data), 
						no.of.sims=1000, include.model = FALSE, method = "WhittleFARIMA", 
						alpha = 0.05, sim = TRUE, doplot=FALSE,resample=TRUE, burn.in=200)

	#whittle.fgn.d.est.sim.results.temp = fractional.diff.est.sim(state.name=current.state.name, data = test.data,arma.order = c(0,0),n.to.use = length(test.data), 
	#					no.of.sims=1000, include.model = FALSE, method = "WhittleFGN", 
	#					alpha = 0.05, sim = TRUE, doplot=FALSE,resample=TRUE, burn.in=200)

	GPH.d.est.sim.results.temp = fractional.diff.est.sim(state.name=current.state.name, data = test.data,arma.order = c(0,0),n.to.use = length(test.data), 
						no.of.sims=1000, include.model = FALSE, method = "GPH", 
						alpha = 0.05, sim = TRUE, doplot=FALSE,resample=TRUE, burn.in=200)

	Sperio.d.est.sim.results.temp = fractional.diff.est.sim(state.name=current.state.name, data = test.data,arma.order = c(0,0),n.to.use = length(test.data), 
						no.of.sims=1000, include.model = FALSE, method = "Sperio", 
						alpha = 0.05, sim = TRUE, doplot=FALSE,resample=TRUE, burn.in=200)

	whittle.farima.d.est.sim.results = subset(as.data.frame(whittle.farima.d.est.sim.results.temp), select = c(d.hat.param,d.hat.se,d.star.mean.param,d.star.se,D.hat.param,D.hat.se,D.star.mean.param,D.star.se))	
	whittle.farima.d.est.sim.resuts.total = rbind(whittle.farima.d.est.sim.resuts.total,whittle.farima.d.est.sim.results)
	
	#whittle.fgn.d.est.sim.results = subset(as.data.frame(whittle.fgn.d.est.sim.results.temp), select = c(d.hat.param,d.hat.se,d.star.mean.param,d.star.se,D.hat.param,D.hat.se,D.star.mean.param,D.star.se))	
	#whittle.fgn.d.est.sim.resuts.total = rbind(whittle.fgn.d.est.sim.resuts.total,whittle.fgn.d.est.sim.results)

	GPH.d.est.sim.results = subset(as.data.frame(GPH.d.est.sim.results.temp), select = c(d.hat.param,d.hat.se,d.star.mean.param,d.star.se,D.hat.param,D.hat.se,D.star.mean.param,D.star.se))	
	GPH.d.est.sim.resuts.total = rbind(GPH.d.est.sim.resuts.total,GPH.d.est.sim.results)

	Sperio.d.est.sim.results = subset(as.data.frame(Sperio.d.est.sim.results.temp), select = c(d.hat.param,d.hat.se,d.star.mean.param,d.star.se,D.hat.param,D.hat.se,D.star.mean.param,D.star.se))	
	Sperio.d.est.sim.resuts.total = rbind(Sperio.d.est.sim.resuts.total,Sperio.d.est.sim.results)

#initial.plots(current.state.data.sreturn)
}	
	#sink()
	d.hat.frame=data.frame(		 GPH.dhat=GPH.d.est.sim.resuts.total[,1]
						,GPH.dhat.se=GPH.d.est.sim.resuts.total[,2]
						,Sperio.dhat=Sperio.d.est.sim.resuts.total[,1]
						,Sperio.dhat.se=Sperio.d.est.sim.resuts.total[,2]
						,wfarima.dhat=whittle.farima.d.est.sim.resuts.total[,1]
						,wfarima.dhat.se=whittle.farima.d.est.sim.resuts.total[,2]
						#,wfgn.dhat=whittle.fgn.d.est.sim.resuts.total[,1]
						#,wfgn.dhat.se=whittle.fgn.d.est.sim.resuts.total[,2]
					)
	d.star.frame=data.frame(	GPH.dstar=GPH.d.est.sim.resuts.total[,3]
						,GPH.dstar.se=GPH.d.est.sim.resuts.total[,4]
						,Sperio.dstar=Sperio.d.est.sim.resuts.total[,3]
						,Sperio.dstar.se=Sperio.d.est.sim.resuts.total[,4]
						,wfarima.dstar=whittle.farima.d.est.sim.resuts.total[,3]
						,wfarima.dstar.se=whittle.farima.d.est.sim.resuts.total[,4]
						#,wfgn.dstar=whittle.fgn.d.est.sim.resuts.total[,3]
						#,wfgn.dstar.se=whittle.fgn.d.est.sim.resuts.total[,4]
					)

	D.hat.frame =data.frame(	 GPH.Dhat=GPH.d.est.sim.resuts.total[,5]
						,GPH.Dhat.se= GPH.d.est.sim.resuts.total[,6]
						,Sperio.Dhat=Sperio.d.est.sim.resuts.total[,5]
						,Sperio.Dhat.se= Sperio.d.est.sim.resuts.total[,6]
						,wfarima.Dhat=whittle.farima.d.est.sim.resuts.total[,5]
						,wfarima.Dhat.se= whittle.farima.d.est.sim.resuts.total[,6]
						#,wfgn.Dhat=whittle.fgn.d.est.sim.resuts.total[,5]
						#,wfgn.Dhat.se= whittle.fgn.d.est.sim.resuts.total[,6]
				)

	D.star.frame =data.frame(	 GPH.Dstar=GPH.d.est.sim.resuts.total[,7]
						,GPH.Dstar.se= GPH.d.est.sim.resuts.total[,8]
						,Sperio.Dstar=Sperio.d.est.sim.resuts.total[,7]
						,Sperio.Dstar.se= Sperio.d.est.sim.resuts.total[,8]
						,wfarima.Dstar=whittle.farima.d.est.sim.resuts.total[,7]
						,wfarima.Dstar.se= whittle.farima.d.est.sim.resuts.total[,8]
						#,wfgn.Dstar=whittle.fgn.d.est.sim.resuts.total[,7]
						#,wfgn.Dstar.se= whittle.fgn.d.est.sim.resuts.total[,8]
					)
	
	box.cox.test.results.params =	as.matrix(subset(as.data.frame(box.cox.test.results),select = c("lambda.param","lambda.se")))
	box.cox.test.results.pvals =	as.matrix(subset(as.data.frame(box.cox.test.results),select = c("LR0.p.value","LR1.p.value")))
	all.p.values = as.data.frame(cbind(unit.root.test.results,normality.test.results,box.cox.test.results.pvals))

	all.params.plus.ses = as.data.frame(cbind(whittle.farima.d.est.sim.resuts.total.frame,
								whittle.fgn.d.est.sim.resuts.total.frame,
								GPH.d.est.sim.resuts.total.frame,
								Sperio.d.est.sim.resuts.total.frame))
###TYPE I plots##
		##plot all of the hypothesis test data
	n=dim(all.p.values)[1]
	for (i in 1:dim(all.p.values)[2]){
		

	if (i==1){
			
			par(mai=c(0.5, 0.5, 0.5, 1.7))
			plot(round(as.numeric(all.p.values[,i]),2),col=i, pch = i,xaxt="n",yaxt="n", xlab = "States",ylab="Significance", main = paste("P-Values for Statistical Tests"))
			at.val = axTicks(1, axp = c(1,n,n-1), usr = NULL, log = NULL)

			y.at.val = axTicks(2, axp = c(0,1,20), usr = NULL, log = NULL)

			axis(1,labels = state.names, at = at.val, cex.axis = 0.75, las = 3)
			axis(2,at = y.at.val, cex.axis = 0.75)

		}
		else
		{
			points(round(as.numeric(all.p.values[,i]),2), col=i, pch=i)
		}

	}
	abline(h=0.05,col="blue",lty=2)	
	abline(v=seq(1,51),col="lightgrey",lwd=0.5,lty=2)

	legend.names = names(all.p.values)
	sequence = seq(1:dim(all.p.values)[2])
	###LEGEND CODE###
	par(xpd=NA)

		tmp <- cnvrt.coords(1,.7, 'plt')$usr
	legend(tmp,lty=1,legend=legend.names, fill = NULL, col = sequence ,
       pch=sequence )
	###END LEGEND CODE###

###END TYPE I Code####

sink("parameter_tests.txt")
print(all.params.plus.ses)
sink()
###TYPE III CODE####
x= read.csv("parameter_tests.txt")
x.axis = sort(rep(state.names,5))

y = 
errbar(x.axis, y, yplus, yminus, cap, xlab, ylab, add=FALSE, 
       lty=1, ylim, lwd=1, Type=rep(1,length(y)), ... )


###END TYPE III CODE####
 	

	

	


for(j in 1:100){

	sink()

}

	
sink("output.txt",append=TRUE);
		print(d.vector);	
sink();

for (i in 1:length(as.matrix(state.names))){
	current.state.name = as.matrix(state.names)[i]
	current.state.data = subset(all.data,all.data[,1]==current.state.name)[,4]
	current.state.data.diff = diff(current.state.data)
	
	current.state.fit = arima(current.state.data.diff, order = c(state.ar.orders[i],0,state.ma.orders[i]))
	adfTest.pval = adfTest(current.state.data ,lags=state.ar.orders[i]+1,type=c("c"))@test$p.value
	adfTest.statistic = adfTest(current.state.data ,lags=state.ar.orders[i]+1,type=c("c"))@test$statistic
	adfTest.lags = state.ar.orders[i]+1
	current.state.box.test.stat = Box.test(current.state.fit$resid,lag = 12, type = "Ljung")$statistic
	
	

	sink("data.txt",append=TRUE);
	print(current.state.name)
	print(coeffs)
	print(se.vector)
	print(t.stat.vector)
	print(fitmodel)
	
	
	print(paste("ADF test test stat",adfTest.statistic))
	print(paste("ADF test p-val",adfTest.pval))
	print(paste("ADF test lags",adfTest.lags))	
	sink();

 }

