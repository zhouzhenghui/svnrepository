#Source files
library(Rmetrics)
library(tseries)
library(gplots)
library(FinTS)
library(zoo)
library(rgl)
library(fracdiff)
demo(hist3d)
setwd("C:/Users/tim/Documents/ClassesFall08/FTS/project2/project/raw_datasets") 
source("../../../ATS_2008.R")

master.index = read.csv("master_index.csv")
i = 4

#START EXECUTION HERE#
source("../project_functions.R")
#############################################################33
#master loop for iterating through entire dataset
	onefile = TRUE
	ma=3
	sink_to_pdf = TRUE
	
	if (sink_to_pdf){
	pdf(file = ifelse(onefile, "Rplots.pdf", "Rplot%03d.pdf"),width=11, height=7, paper="a4r")
	}
	#choose data to analyze here
	selector = cbind("l")
	orders = rbind(7,6,5,13)
	order.types = rbind("se","sh","i","l")
	order.to.use=max(subset(orders,!is.na(match(order.types,selector))))
	exclude_zero_HR_days = TRUE

       ######aggregate statistics
	average.HR = NA
	average.pace = NA
	max.HR = NA
	min.HR = NA

	adf.test.HR.pval = NA
	adf.test.alt.pval = NA
	adf.test.pace.pval = NA
	BIC.order.all = NA
	box.test.pval = NA	
	partition = subset(master.index,(!is.na(match(master.index$Partition,selector))))
	#exclude data where no HR data was taken
	if (exclude_zero_HR_days){
			partition = subset(partition,!(substr(partition$File,nchar(as.matrix(partition$File)),nchar(as.matrix(partition$File)))=="n"))
	}
	for (i in 1:length(partition$File)){
		
		daily.data = na.omit(read.csv(paste(as.matrix(partition[["File"]])[i],".csv", sep="")))
		#format pace data and time data for R analysis and plotting		
		daily.data.formatted = clean.data(daily.data,ma)
		#time axis
		y = stack(daily.data.formatted, select = -Time)
		x = daily.data.formatted$Time
		x_all=rep(x,length(daily.data.formatted)-1)

		#gets rid of noisy pace data (just for plotting)
		daily.data.formatted.subset = overplot.format.data(daily.data.formatted)
		
		#plot time series plot using overplot with custom axes
		#p = custom.overplot(daily.data.formatted.subset,daily.data.formatted,partition,y,x_all,sink_to_pdf)

		#################################
		#correlation analysis

		#difference altitude and heart rate, leave everything else, take out distance
		acftest = daily.data.formatted
		r = length(acftest$Altitude.ft)
		acftest = subset(acftest,select=-Distance.miles)
		
		
		acftest$Altitude.ft[2:r] = diff(daily.data.formatted$Altitude.ft)
		acftest$Pace.min.mile[2:r] = diff(daily.data.formatted$Pace.min.mile)
		#acf(acftest$Pace.min.mile)
		acfaugmented = as.data.frame(cbind(as.matrix(acftest),rbind(0,as.matrix(diff(acftest$HR.bpm)))),row.names=rbind(as.matrix(names(acftest)),"HRDIFF"))
		#dev.new()
		model_data = subset(acfaugmented,select=-Time)
		#acf(model_data)
		model_data$HR.bpm = model_data$V5
		model_data = subset(model_data,select=-V5)
		#acf(subset(acfaugmented,select=-Time) , lag.max = 1000)
		y = as.ts(model_data)
		
		#############################################
		###get AIC and BIC order for VAR(p) model
		VAR20 = ar.ols(y, aic = T, order.max = 20, demean = F, intercept = T)
		
		VAR20$aic                # Compare AIC values of nested models
		aic.order = as.numeric(names(which.min(VAR20$aic)))  ; aic.order
		n = dim(y)[1] ; n
		k = dim(y)[2] ; k
		d = k+0:20*k*2

		VAR20$bic = VAR20$aic - 2*d + d*log(n)
		VAR20$bic
		bic.order = as.numeric(names(which.min(VAR20$bic)))

		#model fitting ad aggregation of parameters
		VARp = ar.ols(y, aic = F, order.max = order.to.use, demean = F, intercept = T)
		
		#parameter checking
		VARp$x.intercept / VARp$asy.se.coef$x.mean
		cut.coeffs = 0
		for(b in 1:order.to.use){
			cut.coeffs = cut.coeffs + (k^2 - length(which(as.numeric(abs(VARp$ar[b,,] / VARp$asy.se.coef$ar[b,,])) > 1.96)))
					
		}
		#truncate parameters and vectorize for analysis
		#parameter.vector.concatenated = NA
		#get the x axis labels for the parameter matrix
		x.axis.lab = rbind("phi(0,0,1)","phi(0,0,2)","phi(0,0,3)")

		#x.axis.lab = NA
		for(b in 1:order.to.use){
			for(col in 1:k){
				for(row in 1:k){
				x.axis.lab = na.omit(rbind(x.axis.lab,paste("phi(",b,",",col,",",row,")",sep="")))
				}
			}			
		}
		#initialize with contant parameter estimates
		parameter.vector.concatenated = NA
		t.ratio.vector = as.numeric(abs(VARp$x.intercept / VARp$asy.se.coef$x.mean))
		parameter.vector = as.numeric(VARp$x.intercept)
		parameter.vector[which(t.ratio.vector  < 1.96)] =0
		parameter.vector=as.matrix(parameter.vector)
		parameter.vector.concatenated = na.omit(rbind(parameter.vector.concatenated,parameter.vector))
		
		for(b in 1:order.to.use){
			t.ratio.vector = as.numeric(abs(VARp$ar[b,,] / VARp$asy.se.coef$ar[b,,]))
			parameter.vector = as.numeric(VARp$ar[b,,])
			parameter.vector[which(t.ratio.vector  < 1.96)] =0
			parameter.vector=as.matrix(parameter.vector)
			parameter.vector.concatenated = na.omit(rbind(parameter.vector.concatenated,parameter.vector))
		}
		
		#gathering 
		#model testing
		g = order.to.use*k^2 - cut.coeffs
		#g = order.to.use*k^2
		a.hat = VARp$resid[!is.na(VARp$resid[,1]),]
		#mq(y,15, df.adj = g)
		box.test = mq(a.hat,15, df.adj = g);
				
		#acf(a.hat)
		#ccf(a.hat[,1],a.hat[,3])
		#ccf(y[,1],y[,3])
		#####################
		#aggregate statistics
		average.HR=cbind(average.HR,mean(acfaugmented$HR.bpm))
		average.pace=cbind(average.pace,mean(acfaugmented$Pace.min.mile))
		max.HR = rbind(max.HR,max(acfaugmented$HR.bpm, na.rm= TRUE))
		min.HR = rbind(min.HR,min(acfaugmented$HR.bpm, na.rm= TRUE))
		adf.test.HR.pval = rbind(adf.test.HR.pval,adfTest(acfaugmented$HR.bpm,lags=11,type=c("nc"))@test$p.value)
		adf.test.alt.pval = rbind(adf.test.alt.pval,adfTest(acfaugmented$Altitude.ft,lags=2,type=c("c"))@test$p.value)
		adf.test.pace.pval = rbind(adf.test.pace.pval,adfTest(acfaugmented$Pace.min.mile,lags=6,type=c("nc"))@test$p.value)
		BIC.order.all = rbind(BIC.order.all,bic.order)
		box.test.pval = rbind(box.test.pval,box.test[["p-value"]][15])
		n = length(parameter.vector.concatenated)
		if (i==1){
			
			plot(parameter.vector.concatenated,col=i,xaxt="n", xlab = "", main = paste("Parameter estimates for",selector))
			at.val = axTicks(1, axp = c(1,n,n-1), usr = NULL, log = NULL)


			axis(1,labels = x.axis.lab, at = at.val, cex.axis = 0.75, las = 2)
		}
		else
		{
			points(parameter.vector.concatenated, col=i)
		}
		

	}
	#plot aggregate statistics
	box.test.pval=as.numeric(subset(as.matrix(as.list(box.test.pval)), !is.na(as.matrix(as.list(box.test.pval)))))
	plot(box.test.pval,type="o",main=paste("Daily Data Box Test P-Vals for",selector),yaxp=c(0,1,20))

	dev.new()
	plot.new()
	BIC.order.all=as.numeric(subset(as.matrix(as.list(BIC.order.all)), !is.na(as.matrix(as.list(BIC.order.all)))))
	plot(BIC.order.all,type="h",main=paste("BIC Order selection for",selector))
	

	adf.test.HR.pval =as.numeric(subset(as.matrix(as.list(adf.test.HR.pval)), !is.na(as.matrix(as.list(adf.test.HR.pval)))))
	adf.test.alt.pval = as.numeric(subset(as.matrix(as.list(adf.test.alt.pval)), !is.na(as.matrix(as.list(adf.test.alt.pval)))))
	adf.test.pace.pval = as.numeric(subset(as.matrix(as.list(adf.test.pace.pval)), !is.na(as.matrix(as.list(adf.test.pace.pval)))))
	plot.new()

	plot(adf.test.pace.pval,yaxp=c(0,1,20))

	plot.new()

	plot(adf.test.HR.pval,yaxp=c(0,1,20))
	plot.new()
	dev.new()
	plot(adf.test.alt.pval,yaxp=c(0,1,20))
	abline(0.05)
	average.pace=as.numeric(subset(as.matrix(as.list(average.pace)), !is.na(as.matrix(as.list(average.pace)))))
	average.HR = as.numeric(subset(as.matrix(as.list(average.HR)), !is.na(as.matrix(as.list(average.HR)))))
	plot(average.HR,average.pace)
	
	z=lm(average.HR~average.pace)
	abline(lm(average.HR~average.pace),col="red")
	sum((z$residuals)^2)
	cor(average.pace,average.HR)
	max(max.HR, na.rm = TRUE)
	min(min.HR, na.rm = TRUE)
	plot(max.HR, type = "l")
	#turn pdf device off if needed
	dev.off()


		