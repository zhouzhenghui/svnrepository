"forecaster" = function(driftmodel,resids,volmodel,independent.variables,test.data.vector,new.curves,plot=T,plotindex=T, plot.ses = T){

alpha = 0.05
projections.shifted = shift.df.multi(new.curves,state,independent.variables)
projections.shifted = as.data.frame(projections.shifted)
projections.shifted = get.tim.data(projections.shifted)
new.forecasting.data = projections.shifted[,which(names(projections.shifted)!=state)]
length = max(dim(new.forecasting.data))
phase2.predictions = predict(volmodel,n.ahead=60, se.fit=TRUE)$pred;
phase2.se = 1.96*predict(volmodel,n.ahead=60, se.fit=TRUE)$se;

shifted.multivar.test.data.vector = shift.df.multi(test.data.vector,state,independent.variables)
predictors = shifted.multivar.test.data.vector[,which(names(shifted.multivar.test.data.vector)!=state)]
responses = shifted.multivar.test.data.vector[,which(names(shifted.multivar.test.data.vector)==state)]

predictions=as.ts(predictor(driftmodel,newdata = new.forecasting.data))
phase1.predictions = predictions[c((length(predictions)-59):length(predictions))]
forecasts= phase1.predictions
predictors.forecasts = new.forecasting.data[c((length-59):length),]
resids.forecasts = phase2.predictions

phase1.se = get.drift.forecast.error(driftmodel,responses,predictors,predictors.forecasts,forecasts,resids,resids.forecasts, alpha)

total.predictions = phase1.predictions+phase2.predictions
total.se = phase1.se + phase2.se
total.se = rm.outlier(total.se,fill=T)


total.fit = c(responses,total.predictions)
errors1 = total.fit+c(rep(NA,length(responses)),total.se)
errors2 = total.fit-c(rep(NA,length(responses)),total.se)


original.fit = c(responses,rep(NA,length(total.predictions)))

if(plot){
	if(plot.ses == T){
		regression.dataframe = as.data.frame(cbind(original.fit,total.fit,errors1,errors2) )
		names(regression.dataframe) = c("Forecast",state,"+Prediction Inverval","-Prediction Inverval")
	}else{	
		regression.dataframe = as.data.frame(cbind(original.fit,total.fit) )
		names(regression.dataframe) = c("Forecast",state)
	}

	dates2 = dategen(begin_month,begin_year, end_month, end_year+5)  #begin month + 1 because we had to chop off the first month

	plot.actual.fitted(regression.dataframe, state, dates=dates2,sreturn=T, same.scale=T)

	regression.dataframe = NULL
	regression.dataframe = as.data.frame(cbind(actual, residuals1))
	names(regression.dataframe) = c(state,"Residuals 1")
	plot.actual.fitted(regression.dataframe,state,dates,sreturn=TRUE,same.scale=FALSE)
}

}