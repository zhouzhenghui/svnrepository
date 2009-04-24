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
all_states = read.csv("../../../Data/States/states2.csv")
stationary_state_names = read.csv("../../../Data/States/nonstationarystates.csv")

##########################
#set states to use here
#states = all_states
states = all_states[match(as.matrix(stationary_state_names$State),as.matrix(all_states$State)),]

##########################

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
adfpvalvec=NULL
pppvalvec=NULL
kpsspvalvec=NULL
adflagvec=NULL
critvalvec=NULL
tstatvec=NULL
state.num = 1
i=1
####END INITIALIZATION#####
for (i in 1:length(as.matrix(state.names))){
#use following to select just 1 state


#for (i in state.num:state.num){
	current.state.name = as.matrix(state.names)[i]
	current.state.data = subset(all.data,all.data[,1]==current.state.name)[,4]
	current.state.data.diff = diff(current.state.data)
	current.state.data.sreturn = (current.state.data[2:length(current.state.data)]-current.state.data[1:(length(current.state.data)-1)])/current.state.data[1:(length(current.state.data)-1)]
		
	#set test data here
	#if (state.unit.roots[i] > 0){
	#	current.state.data.testdata = diff(current.state.data.sreturn,lag=1,differences=state.unit.roots[i])
	#}else{
		current.state.data.testdata = current.state.data.sreturn
	#}

		
		current.state.data.testdata.sdiff=diff(current.state.data.testdata,lag=2,differences=1)
		
	#sink("output.txt",append=TRUE);
		mean.number.of.params = max(0,state.ar.orders[i] + state.ma.orders[i]);
		vol.number.of.params = max(0,state.alpha.orders[i] + state.beta.orders[i]);
		mean.adj.DF = ifelse(adjust.dof,lags.to.use-mean.number.of.params,NULL)
		vol.adj.DF = ifelse(adjust.dof,lags.to.use-vol.number.of.params,NULL)
		params = c(lags.to.use,mean.adj.DF, vol.adj.DF)
	
		#if(vol.number.of.params>0){
			
			
		#	formula1 = as.formula(paste("~arma(",state.ar.orders[i],",",state.ma.orders[i],")+garch(",state.alpha.orders[i],",",state.beta.orders[i],")",sep=""))
		#	current.state.garchfit = garchFit(formula = formula1 , data=as.matrix(current.state.data.testdata),cond.dist = c("QMLE"), include.mean=TRUE)
			
		#	sresi = current.state.garchfit@residuals/current.state.garchfit@sigma.t
		#	meanboxtestvec = Box.Ljung.test(sresi,lag = lag.to.use,adj.DF = mean.adj.DF)$p.value;
		#	volboxtestvec Box.Ljung.test(sresi^2,lag = lag.to.use,adj.DF = vol.adj.DF)$p.value;

		#}
			

		#}
		#acf(sresi)		
	##	Box.Ljung.test(sresi,lag = lag.to.use,adj.DF = lag.to.use-4)$p.value;
	#	Box.Ljung.test(sresi^2,lag = lag.to.use,adj.DF = lag.to.use-2)$p.value;
		
	
	#	adf.test(current.state.data.testdata,k = 8)

		urt = unit.root.test(current.state.data.testdata)
		adfpvalvec= c(adfpvalvec,urt$adf.p.value)
		pppvalvec = c(pppvalvec,urt$pp.p.value)
		kpsspvalvec = c(kpsspvalvec,urt$kpss.p.value)
		adflagvec= c(adflagvec,urt$adf.lag.value)

		#pacf(current.state.data.testdata)
		#lags=5
		#d.hat = fdGPH(current.state.data.testdata)$d
		#critval=	abs(fdfcritical(1,lags ,length(current.state.data.testdata),d.hat,0)$fivepercent)

		#tstat = as.numeric(abs(fadf.test(current.state.data.testdata, alternative = c("stationary"), k = lags, dparam= d.hat)$statistic))	
		#critvalvec=c(critvalvec,critval)
		#tstatvec=c(tstatvec,tstat)
		
	#sink();
}

plot(statenumbers,critvalvec, xaxt = "n", pch = 1, col = 1, main = "FI(1) vs FI(d) for Nonstationary States",xlab="States",ylab="Test Statistics")
points(statenumbers,tstatvec, pch = 2, col = 2)

statelabels = paste(state.names, "-(",5,")",sep="")

axis(side=1,at = statenumbers, labels = statelabels, las = 3)

dev.new()	
statenumbers = seq(1,length(state.names))
plot(statenumbers,adfpvalvec, xaxt = "n", pch = 1, col = 1, main = "Unit Root Test for All States",xlab="States",ylab="P-Values")
points(statenumbers,pppvalvec, pch = 2, col = 2)
points(statenumbers,kpsspvalvec, pch = 3, col = 3)
abline(h=0.05,lty=2,col="blue")
statelabels = paste(state.names, "-(",adflagvec,")",sep="")

axis(side=1,at = statenumbers, labels = statelabels, las = 3)
legend("topright",c("ADF","Philip Peron","KPSS"),pch=c(1,2,3), col=c(1,2,3), cex = 0.8)

sink("stationarystates.txt")	
state.names[which(adfpvalvec < 0.05)]

sink()

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

