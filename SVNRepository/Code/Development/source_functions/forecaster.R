"forecaster" = function(driftmodel,resids,volmodel,independent.variables,test.data.vector,new.curves,plot=T,plotindex=T, plot.ses = T){
projectedindex = NULL
alpha = 0.05
projections.shifted = shift.df.multi(new.curves,state,independent.variables)
projections.shifted = as.data.frame(projections.shifted)
projections.shifted = get.tim.data(projections.shifted)
new.forecasting.data = projections.shifted[,which(names(projections.shifted)!=state)]
length = max(dim(new.forecasting.data))
nahead = 60
		 if ((class(volmodel)) == "fGARCH"){
               phase2.predictions = predict(volmodel,n.ahead=nahead)$meanForecast;
			  phase2.se = 	predict(volmodel,n.ahead=nahead)$standardDeviation;		
            }else{
	             phase2.predictions = predict(volmodel,n.ahead=60, se.fit=TRUE)$pred;
			phase2.se = 1.96*predict(volmodel,n.ahead=60, se.fit=TRUE)$se;
            }



shifted.multivar.test.data.vector = shift.df.multi(test.data.vector,state,independent.variables)
predictors = shifted.multivar.test.data.vector[,which(names(shifted.multivar.test.data.vector)!=state)]
responses = shifted.multivar.test.data.vector[,which(names(shifted.multivar.test.data.vector)==state)]

predictions=as.ts(predictor(driftmodel,newdata = new.forecasting.data))
phase1.predictions = predictions[c((length(predictions)-59):length(predictions))]
forecasts= phase1.predictions
predictors.forecasts = new.forecasting.data[c((length-59):length),]
resids.forecasts = phase2.predictions

phase1.se = get.drift.forecast.error2(driftmodel,responses,predictors,predictors.forecasts,forecasts,resids,resids.forecasts, alpha)
phase1.se = 0
total.predictions = phase1.predictions+phase2.predictions
total.se = phase1.se + phase2.se
total.se = rm.outlier(total.se,fill=T)

orig.responses = test.data.vector[,which(names(test.data.vector)==state)]

total.fit = c(orig.responses,total.predictions)
errors1 = total.fit+c(rep(NA,length(orig.responses)),total.se)
errors2 = total.fit-c(rep(NA,length(orig.responses)),total.se)


original.fit = c(orig.responses,rep(NA,length(total.predictions)))
projectedreturns = total.fit

if(plot==TRUE){
#	if(plot.ses == T){
#		regression.dataframe = as.data.frame(cbind(original.fit,total.fit,errors1,errors2) )
#		names(regression.dataframe) = c("Forecast",state,"+Prediction Inverval","-Prediction Inverval")
#	}else{	
		regression.dataframe = as.data.frame(cbind(original.fit,total.fit) )
		names(regression.dataframe) = c("Forecast",state)
#	}

	dates2 = dategen(begin_month,begin_year, end_month, end_year+5)  #begin month + 1 because we had to chop off the first month

	plot.actual.fitted(regression.dataframe, state, dates=dates2,sreturn=T, same.scale=T, adjust=F)
	if(plot.ses==T){
		#dev.off()
		lines(errors1, col="blue",lty=2,lwd=2)
		lines(errors2, col="blue",lty=2,lwd=2)	
	}
	if(plotindex==T){
		data = grab.data(state,begin_month,begin_year,end_month,end_year,sreturn=F)
		test.data.vector3 = data$test.data.vector
		test.data.vector3 = as.data.frame(test.data.vector3)
		statedata= test.data.vector3[,which(names(test.data.vector3)==state)]
		first.value = statedata[1]
		errors1 = na.omit(total.fit-errors1)
		errors2 = na.omit(total.fit-errors2)
		
		original.fit=get.orig.data(first.value,original.fit)
		total.fit=get.orig.data(first.value,total.fit)
		
		ref= total.fit[c((length(total.fit)-length(errors1)):(length(total.fit)))]
		errors1 = ref*(1+errors1)
		errors2 = ref*(1+errors2)
		lengthdiff = length(total.fit)-length(errors1)
		
		errors1 = c(rep(NA,lengthdiff),errors1)
		errors2 = c(rep(NA,lengthdiff),errors2)
		
		regression.dataframe = as.data.frame(cbind(original.fit,total.fit) )
		names(regression.dataframe) = c("Forecast",state)
		dates2 = dategen(begin_month,begin_year, end_month, end_year+5)  #begin month + 1 because we had to chop off the first month
		
		if(plot.ses==T){
			
			regression.dataframe=cbind(regression.dataframe,errors1,errors2)		
			#lines(errors1, col="blue",lty=2,lwd=2)
			#lines(errors2, col="blue",lty=2,lwd=2)	
		}

		plot.actual.fitted(regression.dataframe, state, dates=dates2,sreturn=T, same.scale=T,adjust=F)
		projectedindex = total.fit
		
		new.obs = get.sd.data("housing_price_index",state =state,begin_month,begin_year,end_month+6,end_year)$interpolated_data
		new.obs[c(1:(length(new.obs)-6))]=NA
		lines(new.obs,col="green",lwd=2,lty=2,type="p",pch=2)
		

	}
	projectedindex = total.fit

}
structure(list(projectedindex = projectedindex, projectedreturns = projectedreturns))
}