
"pretty.ts.graph"=function(data,title,dates,col="black") {

#dev.new()	
plot(as.ts(data),xaxt="n",xlab="", main=title, lwd = 2,lty=2,col=col,ylab="")
x.coords = seq(from=1,to=length(data),by=12)
tick.coords = seq(from=1,to=length(data), by = 1)
quarter.coords = seq(from=1,to=length(data), by = 3)

axis(side=1,at=x.coords,labels = dates[x.coords], las =2,  tick = TRUE, tck=-.04)
axis(side=1,at=quarter.coords,labels = rep("",length(quarter.coords)), las =2, cex = 0.5, tick = TRUE)


}