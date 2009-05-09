"mort.orig.curve"=function(state,data){

#replicate 24 months of last value
	lastval=data[length(data)]
	piece1 = rep(lastval,24)
	#for the next 36 months, fit a curve to the first part of the data
	#then adjust the intercept  to patch where piece 1 leaves-off
	X=c(1:36)-1
	Y=data[X+1]
	model <- lm(Y ~ X + I(X^2) + I(X^2))
	int=as.numeric(model$coeff[1])
	piece2=model$fit-int
	#concatenate
	projection = c(piece1,piece2)
	newdata = c(data,projection)

structure(list(newdata=newdata,data=data,total=sum(newdata)))

}
