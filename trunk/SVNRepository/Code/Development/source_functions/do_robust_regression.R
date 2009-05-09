"do_robust_regression"=function(state,test.data.vector,dates,independent.variables,automatic=T,plot=T){

#do a robust regression
shifted.multivar.test.data.vector = shift.df.multi(test.data.vector,state,independent.variables)

if(automatic==TRUE){
	#convert all ranges to # demarkations in lagranges for this particular state
	#convert.ranges(state)

}
predictors = shifted.multivar.test.data.vector
predictors=as.matrix(predictors[,which(names(predictors)!=state)])
response = as.matrix(subset(shifted.multivar.test.data.vector, select = state))
robust.lm = rlm(x=predictors ,y=response)
summary(robust.lm)
resids.temp=as.ts(robust.lm$residuals)


#model check
if(min(abs(summary(robust.lm)$coefficients[,3]))<20){
	drop=names(which.min(abs(summary(robust.lm)$coefficients[,3])))
}else{
	drop=""
}
adf.test = adf.urca.test(as.matrix(resids.temp))
#end robust regression
if(plot){
	fit1 = robust.lm$fitted	
	residuals1 = robust.lm$residuals
	actual = response
	regression.dataframe = as.data.frame(cbind(actual, fit1))
	names(regression.dataframe) = c(state,"Phase 1 Fit")
	plot.actual.fitted(regression.dataframe,state,dates,sreturn=TRUE,same.scale=TRUE)
	regression.dataframe = NULL
	regression.dataframe = as.data.frame(cbind(actual, residuals1))
	names(regression.dataframe) = c(state,"Residuals 1")
	plot.actual.fitted(regression.dataframe,state,dates,sreturn=TRUE,same.scale=FALSE)
}
structure(list(robust.lm=robust.lm,resids=resids.temp,adf.test=adf.test,summary=summary(robust.lm), drop=drop))
}