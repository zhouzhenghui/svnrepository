"mort.orig.curve"=function(state,data,lag=0){

#replicate 24 months of last value
	lastval=data[length(data)]
	piece1 = rep(lastval,(24))
	#for the next 36 months, fit a curve to the first part of the data
	#then adjust the intercept  to patch where piece 1 leaves-off

	X=c(1:(36+lag))-1
	Y=data[X+1]
	X=c(1:(60+lag))-1

	Y=c(rep(0,24),Y)
	model <- lm(Y ~ X + I(X^2) + I(X^2))
	int=as.numeric(model$coeff[1])
	piece2=as.numeric(model$fit-int)
	min.where = which(piece2<0)
	if (length(min.where)==0){
	minimum = 0
	}else{
		minimum = abs(min(piece2[min.where]))
	}
	piece2= piece2 + minimum
	#concatenate
	#piece2=as.numeric(piece2)
	projection = piece2
	newdata = c(data,projection)

structure(list(newdata=newdata,data=data,total=sum(newdata)))

}
