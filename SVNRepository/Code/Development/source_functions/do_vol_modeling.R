"do_vol_modeling"=function(state,test.data.vector,phase1fits,resids,dates,automatic=T,plot=T,useGPH=F){
	shifted.multivar.test.data.vector = shift.df.multi(test.data.vector,state,independent.variables)
	response = as.matrix(subset(shifted.multivar.test.data.vector, select = state))

	if(automatic){
		resid.model=auto.arima(resids)
		mean.equation.params = length(resid.model$coef)
		model.check.temp = model.check(resid.model,mean.equation.params,0,12)

	}else{
		resid.model.temp=auto.model(current.state.name=state, percentage=1, cut.off=FALSE, interp=FALSE, data=resids, useGPH=useGPH)
		resid.model=resid.model.temp$model
		d=resid.model.temp$d
		model.check.temp = resid.model.temp$model.check
	}
	if ((class(resid.model)) == "fGARCH"){
               res2 	= as.numeric(resid.model@residuals)/as.numeric(resid.model@sigma.t);		  
			fit2 = as.numeric(resid.model@fitted)
            }else{
               res2 	= as.numeric(resid.model$residuals);
			fit2 = resids - res2
		}

	adf.test = adf.urca.test(res2)
	if(plot){
	
	
		fit1 = phase1fits
		
		residuals1 = resids
		residuals2 = res2
		actual = response		
		regression.dataframe = as.data.frame(cbind(actual, fit1+fit2,residuals2))
		names(regression.dataframe) = c(state,"Phase 1/2 Fit","Res1/2")
		plot.actual.fitted(regression.dataframe,state,dates,sreturn=TRUE,same.scale=T)
		eacf(resids)
	}
	structure(list(model=resid.model,d=d,model.check=model.check.temp, adftest = adf.test,resids=res2))
}
