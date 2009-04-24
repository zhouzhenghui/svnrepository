#takes data from 
"clean.data" = function(data.arg, ma){
	data.formatted.rtn = data.arg
	data.formatted.rtn = na.omit(data.formatted.rtn) #if there were bogus conversions

data.formatted.rtn

}
#this function formats the pace data for clean plotting, since its soo noisy (gets rid of outliers)
"overplot.format.data" = function(data.formatted.arg){

	overplot.format.data.rtn=data.formatted.arg
	#pacemm.ss = data.formatted.arg$Pace.min.mile
	#pace.subset = subset(pacemm.ss,pacemm.ss>quantile(pacemm.ss,0.03, na.rm=T)&pacemm.ss<quantile(pacemm.ss,0.96, na.rm=T))
	#pace.pad = rep(mean(pace.subset),length(pacemm.ss)-length(pace.subset))
	#pace.adjusted = as.numeric(as.matrix(cbind(pace.subset,pace.pad)))[1:length(na.omit(pacemm.ss))]
	#overplot.format.data.rtn[["Pace.min.mile"]] = pace.adjusted

overplot.format.data.rtn	

}
#invoke an overplot, using customized parameters
"custom.overplot" = function(data.formatted.subset,data.formatted,y,x_all,state_labels,sink_to_pdf=FALSE){
	#get limits based on discarded outliers
		miny = sapply(data.formatted.subset,"min")
		miny	= miny[unique(match(y[,2],names(miny)))]
		maxy = sapply(data.formatted.subset,"max")
		maxy	= maxy[unique(match(y[,2],names(maxy)))]
		if(!sink_to_pdf){
			dev.new()
		}
		plot.new()

		p=overplot(y[,1] ~ x_all | y[,2], data = data.formatted, xlab="State", main="Type III Plot", plot = FALSE,f=0);
		if(!sink_to_pdf){
			dev.off()
			dev.new()
			plot.new()
		}
		
		maxy = ceiling(maxy[match(names(p),names(maxy))])
		miny = floor(miny[match(names(p),names(miny))])			
		
		p=overplot(y[,1] ~ x_all | y[,2], data = data.formatted, xlab="", ylab="", main="Type III Plot", plot = T, min.y = miny, max.y = maxy,  f= 0,type = "p", col.main = "blue") #yaxs="i",
		#par(mar = (50, 4, 4, 2) + 0.1)
		axis(side=1,at = x,labels =  state_labels, las = 3, cex.axis = 2, col.axis = "black")
		points(y[,2])
		if(!sink_to_pdf){
			dev.off()
		}
	p 
}

