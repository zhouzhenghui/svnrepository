##Function returns p-values for:
## Unit Root Tests (ADF, PP, KPSS)
##
## Created Jan 21 2009

##--Unit Root Tests--##

library(tseries)


"unit.root.test" = function(data){
	VAR20 = ar.ols(data, aic = T, order.max = 20, demean = F, intercept = T)
	aic.order = as.numeric(names(which.min(VAR20$aic)))  ; 
	# compare this with BIC
	n = length(data) ;
	k = 1 ;
	d = k+0:20*k*2;
	VAR20$bic = VAR20$aic - 2*d + d*log(n);
	bic.order = as.numeric(names(which.min(VAR20$bic)));
	adfLag  = min(bic.order,aic.order)	;	

<<<<<<< .mine
	adf.test.results = adf.test(data, k=adfLag, alternative = c("stationary"))$p.value
=======
	adfLag = adf.test.order

	#adf.test.results = adf.test(data, k=adfLag,alternative = c("stationary"))$p.value
	adf.test.results = adfTest(data,lags=adfLag,type=c("ct"))@test$p.value

>>>>>>> .r253
	pp.test.results = PP.test(data)$p.value
	kpss.test.results = kpss.test(data, null="Level", lshort=TRUE)$p.value
<<<<<<< .mine
		
=======
	
	#adf.test.results2 

>>>>>>> .r253
	structure(list(adf.p.value=adf.test.results,pp.p.value=pp.test.results,kpss.p.value=kpss.test.results, adf.lag.value = adfLag))
	
}






