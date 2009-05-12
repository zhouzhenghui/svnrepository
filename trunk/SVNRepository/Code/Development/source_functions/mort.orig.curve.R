"mort.orig.curve"=function(state,data,lag=0){

#replicate 24 months of last value
	lastval=data[length(data)]
	piece1 = rep(lastval,(24))
	#for the next 36 months, fit a curve to the first part of the data
	#then adjust the intercept  to patch where piece 1 leaves-off

	X=c(1:(36+lag))-1
	Y=data[X+1]
	#X=c(1:(60+lag))-1

	piece1=rep(lastval,24)
	model <- lm(Y ~ X + I(X^2))
	model$coeff[3]=abs(model$coeff[3])/2
	int=as.numeric(model$coeff[1])
	#piece2 = model$coeff[2]*X + model$coeff[3]*X^2 
	piece2=as.numeric(model$fit-int)
	min.where = which(piece2<0)
	if (length(min.where)==0){
	minimum = 0
	}else{
		minimum = abs(min(piece2[min.where]))
	}
	piece2= piece2 + minimum
	if(piece2[1]<lastval){
		piece2 = piece2 + lastval
	}
	#concatenate
	#piece2=as.numeric(piece2)
	first.value = 1

	#sreturn = get.sreturn(piece2)	
	#sreturn2 = smoother(cbind(sreturn,sreturn),iterations=5)[,1]
	#piece2 = get.orig.data(first.value,sreturn2)
#	data2 = data[c(1:36),]

	data2 = c(piece1,data[c(1:36)])
	interpolated_data = as.data.frame(data2)
	data2 = as.data.frame(data2)
	names(data2) = "interpolated_data"
	#polynomial regressions
	time = c(1:dim(data2)[1])
	polyfit = loess(interpolated_data ~ time ,data2, degree = 2, control = loess.control(surface = "interpolate"))      

	
	#plot(as.ts(as.matrix(interpolated_data)), col="red", lty = 1)
	#lines(as.ts(polyfit$fitted), col = "blue", lwd = 2)
	piece3 = as.ts(polyfit$fitted)[1:(60+lag)]
	height.adjust = piece3[1]-lastval
	piece3 = piece3-height.adjust
	piece3= as.numeric(piece3)
	piece3[which(piece3<0)]=0
	#projection = c(piece1,piece2)
	projection = c(piece3)

	newdata = c(data,projection)
	#first.value = newdata [1]
	##plot(as.ts(projection ))
	##plot(as.ts(newdata ))

	#sreturn = get.sreturn(newdata )	
	#sreturn2 = smoother(cbind(sreturn,sreturn),iterations=5)[,1]
	#plot(as.ts(sreturn))
	#lines(as.ts(sreturn2))
	
	#plot(as.ts(sreturn))
	#sreturn2 = smoother(cbind(sreturn,sreturn),iterations=2)[,1]
	#plot(as.ts(sreturn2))
#	orig.data = get.orig.data(first.value,sreturn2)
	#plot(as.ts(orig.data))


	#plot(as.ts(newdata))
	#plot(as.ts(get.sreturn(newdata)))
	
structure(list(newdata=newdata,data=data,total=sum(newdata)))

}
