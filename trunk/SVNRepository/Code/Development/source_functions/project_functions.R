#takes data from 
"clean.data" = function(daily.data.arg, ma){
	daily.data.formatted.rtn = daily.data.arg
	pace = as.POSIXlt(as.POSIXct(as.matrix(daily.data.arg[["Pace.min.mile"]]), format="%M:%S"))
	if (ma == 0){
		pacemm.ss = pace$min + pace$sec/60
		
		}
		else{
		pacemm.ss = pace$min + pace$sec/60
		pacemm.ss[ma:length(pacemm.ss)] = rollmean(pace$min + pace$sec/60,ma)
		

		}

	time = as.POSIXlt(as.POSIXct(as.matrix(daily.data.arg[["Time"]]), format="%H:%M:%S"))
	timess = time$hour*3600 + time$min*60 + time$sec

		
	daily.data.formatted.rtn[["Pace.min.mile"]]=pacemm.ss
	daily.data.formatted.rtn[["Time"]]=timess 

	daily.data.formatted.rtn = na.omit(daily.data.formatted.rtn) #if there were bogus conversions

daily.data.formatted.rtn

}
#this function formats the pace data for clean plotting, since its soo noisy (gets rid of outliers)
"overplot.format.data" = function(daily.data.formatted.arg){

	overplot.format.data.rtn=daily.data.formatted.arg
	pacemm.ss = daily.data.formatted.arg$Pace.min.mile
	pace.subset = subset(pacemm.ss,pacemm.ss>quantile(pacemm.ss,0.03, na.rm=T)&pacemm.ss<quantile(pacemm.ss,0.96, na.rm=T))
	pace.pad = rep(mean(pace.subset),length(pacemm.ss)-length(pace.subset))
	pace.adjusted = as.numeric(as.matrix(cbind(pace.subset,pace.pad)))[1:length(na.omit(pacemm.ss))]
	overplot.format.data.rtn[["Pace.min.mile"]] = pace.adjusted

overplot.format.data.rtn	

}
#invoke an overplot, using customized parameters
"custom.overplot" = function(daily.data.formatted.subset,daily.data.formatted,partition,y,x_all,sink_to_pdf=FALSE){
	#get limits based on discarded outliers
		miny = sapply(daily.data.formatted.subset,"min")
		miny	= miny[unique(match(y[,2],names(miny)))]
		maxy = sapply(daily.data.formatted.subset,"max")
		maxy	= maxy[unique(match(y[,2],names(maxy)))]
		if(!sink_to_pdf){
			dev.new()
		}
		plot.new()

		p=overplot(y[,1] ~ x_all | y[,2], data = daily.data.formatted, xlab="Time(sec)", main=partition[["File"]][i], plot = FALSE,f=0);
		if(!sink_to_pdf){
			dev.off()
			dev.new()
			plot.new()
		}
		
		maxy = ceiling(maxy[match(names(p),names(maxy))])
		miny = floor(miny[match(names(p),names(miny))])			
		p=overplot(y[,1] ~ x_all | y[,2], data = daily.data.formatted, xlab="Time(sec)", ylab="", main=paste("Date:",partition[["File"]][i]), plot = T, min.y = miny, max.y = maxy,  f= 0) #yaxs="i",
		
		if(!sink_to_pdf){
			dev.off()
		}
	p 
}

