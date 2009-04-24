#Tim 
#Created 1-10-09
####INITIALIZATION--RUN THIS FIRST#####
library(Rmetrics)
library(fArma)
library(fracdiff)
library(GeneCycle)
library(fGarch)
library(tseries)
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

all.data = read.table("../../../Data/States/all.txt")
all.data.state = all.data[,1]
states = read.csv("../../../Data/States/states.csv")

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
lag.to.use = 12;
#adjust d.o.f. (TRUE/FALSE)
adjust.dof = TRUE;
#significance of tests
alpha = 0.05;
##Loop that runs through states starts here
##############################
d.vector = NULL

####END INITIALIZATION#####
for (i in 1:length(as.matrix(state.names))){
#use following to select just 1 state
state.num = 41
i=state.num
#for (i in state.num:state.num){
	current.state.name = as.matrix(state.names)[i]
	current.state.data = subset(all.data,all.data[,1]==current.state.name)[,4]
	current.state.data.diff = diff(current.state.data)
	current.state.data.sreturn = (current.state.data[2:length(current.state.data)]-current.state.data[1:(length(current.state.data)-1)])/current.state.data[1:(length(current.state.data)-1)]
		
	#set test data here
	if (state.unit.roots[i] > 0){
		current.state.data.testdata = diff(current.state.data.sreturn,lag=1,differences=state.unit.roots[i])
	}else{
		current.state.data.testdata =current.state.data.sreturn
	}
		current.state.data.testdata.sdiff=diff(current.state.data.testdata,lag=2,differences=1)
		acf(current.state.data.testdata)
		acf(current.state.data.testdata.sdiff)
	sink("output.txt",append=TRUE);
		mean.number.of.params = max(0,state.ar.orders[i] + state.ma.orders[i]);
		vol.number.of.params = max(0,state.alpha.orders[i] + state.beta.orders[i]);
		mean.adj.DF = ifelse(adjust.dof,lags.to.use-mean.number.of.params,NULL)
		vol.adj.DF = ifelse(adjust.dof,lags.to.use-vol.number.of.params,NULL)
		params = c(lags.to.use,mean.adj.DF, vol.adj.DF)

#		current.state.garchfit = garchFit(formula = ~arma(state.ar.orders[i],state.ma.orders[i])+garch(state.alpha.orders[i],state.beta.orders[i]), data=as.ts(current.state.data.testdata),cond.dist = c("QMLE"), include.mean=TRUE)
		current.state.garchfit = garchFit(formula = ~arma(2,0)+garch(1,1), data=current.state.data.testdata,cond.dist = c("QMLE"), include.mean=TRUE)
		sresi = current.state.garchfit@residuals/current.state.garchfit@sigma.t
		acf(sresi)		
		 Box.Ljung.test(sresi,lag = lag.to.use,adj.DF = lag.to.use-4)$p.value;
		 Box.Ljung.test(sresi^2,lag = lag.to.use,adj.DF = lag.to.use-2)$p.value;
		test = arima(current.state.data.testdata.sdiff,order = c(0,0,1)),seasonal = list(order = c(2,0,0),period = 2-))
		resids = test$residuals
		Box.Ljung.test(resids ,lag = lag.to.use,adj.DF = lag.to.use-2)$p.value;
		acf(resids)
		fdGPH(current.state.data.testdata)
				print(current.state.name)
		current.state.fit = complete.arma.model.with.stats(current.state.data.testdata,ord=c(10,0,0), alpha = alpha, box.params = c(12,2,0))	;
		print(current.state.fit)	
		print(current.state.garchfit)
		pacf(diff(current.state.data.sreturn))
	#done
		adf.test(current.state.data.testdata,k = 8)

		unit.root.test(current.state.data.testdata)
		pacf(current.state.data.testdata)


	#initial plots of ACF, PACF, EACF of original and return series
	#initial.plots(current.state.testdata)
	
 	#state name
	#print(paste("test for ",current.state.name))
	
	#DOF adjustments
	#mean.number.of.params = max(0,state.ar.orders[i] + state.ma.orders[i]);
	#vol.number.of.params = max(0,state.alpha.orders[i] + state.beta.orders[i]);
	#mean.adj.DF = ifelse(adjust.dof,lags.to.use-mean.number.of.params,NULL)
	#vol.adj.DF = ifelse(adjust.dof,lags.to.use-vol.number.of.params,NULL)


	#perform ADF test on original data 
		#print(aic.bic.adf.Test(current.state.data,1))
		#print(aic.bic.adf.Test(current.state.data.sreturn,0))
	
	#fit an ARMA and get test stats			
		#params = c(lags.to.use,mean.adj.DF, vol.adj.DF)
		#current.state.fit = complete.arma.model.with.stats(current.state.data.diff,c(state.ar.orders[i],0,state.ma.orders[i]), alpha = alpha, box.params = params)	;
		#print(current.state.fit)		
	#investigate fractional differencing here
		#state.frac.diff.param =fdGPH(current.state.data.diff)
		#d.est = state.frac.diff.param$d
		#d.se = state.frac.diff.param$sd.reg
		#d.ase= state.frac.diff.param$sd.as

		#print(paste("d=",d.est))
		#print(paste("d.se=",d.se))
	#if(d.est < 0.5){
		#d.vector = rbind(d.vector,paste(current.state.name,fdGPH(current.state.data.diff)$d))
	#}		
	
	
	#fit an ARFIMA(p,d,q) and get test stats
		#diff.state.fit = complete.arma.model.with.stats(diffseries(current.state.data.diff,d.est),c(state.ar.orders[i],0,state.ma.orders[i]), alpha = alpha, box.params = params);
		#print(diff.state.fit)	
	sink();
}
	


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

