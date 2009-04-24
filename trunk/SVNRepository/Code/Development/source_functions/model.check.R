#checks mean equation, volatility equation (for ARCH effects)
# and model invertability

"model.check" = function(model,mean.number.of.params,vol.number.of.params,lags.to.use) {
	adjust.dof=TRUE
	if ((class(model)) == "fGARCH"){
               res 	= as.numeric(model@residuals)/as.numeric(model@sigma.t);
		   res.sq	= res^2;
            }else{
               res 	= as.numeric(model$residuals);
		   res.sq	= res^2;
            }
	
	
	
	mean.adj.DF = ifelse(adjust.dof,lags.to.use-mean.number.of.params,NULL)
	vol.adj.DF = ifelse(adjust.dof,lags.to.use-vol.number.of.params,NULL)


	mean.check.pval = as.numeric(Box.Ljung.test(res,lag = lags.to.use,adj.DF = mean.adj.DF)$p.value);	
	vol.check.pval = as.numeric(Box.Ljung.test(res.sq,lag = lags.to.use,adj.DF = vol.adj.DF)$p.value);
	


	structure(list(mean.check.p.value=mean.check.pval,vol.check.p.value=vol.check.pval))

}