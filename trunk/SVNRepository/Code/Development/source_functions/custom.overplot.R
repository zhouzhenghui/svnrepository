library(gplots)
library(survival)
source("../source_functions/my.overplot.R")
"custom.overplot" = function(test.data.vector, state, dates,same.scale=FALSE){
	#get limits based on discarded outliers
		y = stack(test.data.vector)
		
		#monthyear = as.numeric(unlist(strsplit(dates, "/")))
		#months = monthyear[seq(1,length(monthyear),by=2)]
		#years = monthyear[seq(2,length(monthyear),by=2)]
		#for (i in 0:8){	
		#	years[which(years==i)]=2000 + i
		#}
		#	years[which(years<2000)] = years[which(years<2000)] + 1900

		#realdates = mdy.date(months, 1, years , nineteen = TRUE)

		#realdates2 = date.mmddyy(realdates , sep = "/")

		x = seq(1,dim(test.data.vector)[1])
		#x = realdates2 
		x_all=rep(x,length(test.data.vector))


		miny = sapply(test.data.vector,"min")
		miny	= miny[unique(match(y[,2],names(miny)))]
		maxy = sapply(test.data.vector,"max")
		maxy	= maxy[unique(match(y[,2],names(maxy)))]
		dev.new()	
		plot.new()
	
		p=myoverplot(y[,1] ~ x_all | y[,2], dates = dates,data = test.data.vector,state=state, xlab="Date",ylab="", main=paste(state," Time Series Plot"), plot = T,f=0,same.scale=same.scale);
		
		#maxy = ceiling(maxy[match(names(p),names(maxy))])
		#miny = floor(miny[match(names(p),names(miny))])			
		#p=overplot(y[,1] ~ x_all | y[,2], data = test.data.vector, xlab="Date", ylab="", main=paste(state," Time Series Plot"), plot = T, min.y = miny, max.y = maxy,  f= 0) #yaxs="i",
		#p=myoverplot(y[,1] ~ x_all | y[,2], data = test.data.vector,xaxt="n", xlab="Date", ylab="", main=paste(state," Time Series Plot"), plot = T, min.y = miny, max.y = maxy,  f= 0) #yaxs="i",

		
	p 
}
