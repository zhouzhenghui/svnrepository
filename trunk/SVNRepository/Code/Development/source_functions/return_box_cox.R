## Function that takes in a data set and returns the
## Box-Cox power transformation and its standard error
##
## Created Jan 21 2009
## Shirley
##edited Jan 23 2009 Tim

"return.box.cox" = function(current.state.data) {
	if(min(current.state.data)<0){

		adjusted.current.state.data = current.state.data + 1.00001*abs(min(current.state.data))
	
	}else{
		adjusted.current.state.data = current.state.data
	}	
	test = box.cox.powers(adjusted.current.state.data)
	current.state.lambda = test$lambda
	current.state.bc.se = test$stderr
	current.state.LR0.p.value = 1-pchisq(test$LR0, df=1)
	current.state.LR1.p.value = 1-pchisq(test$LR1, df=1)
	
	
	structure(list(lambda.param=current.state.lambda, lambda.se=current.state.bc.se,
	LR0.p.value = current.state.LR0.p.value, LR1.p.value = current.state.LR1.p.value))
	

}