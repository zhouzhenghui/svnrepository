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
resids=as.ts(robust.lm$residuals)


#model check
if(min(abs(summary(robust.lm)$coefficients[,3]))<1.6){
	drop=names(which.min(abs(summary(robust.lm)$coefficients[,3])))
}else{
	drop=""
}
adf.test = adf.urca.test(resids)
#end robust regression
if(plot){
	fit1 = robust.lm$fitted	
	residuals1 = resids
	actual = response
	regression.dataframe = as.data.frame(cbind(actual, fit1,resids))
	names(regression.dataframe) = c(state,"Phase 1 Fit","Res 1")
	plot.actual.fitted(regression.dataframe,state,dates,sreturn=TRUE)
}
structure(list(robust.lm=robust.lm,resids=resids,adf.test=adf.test,summary=summary(robust.lm), drop=drop))
}