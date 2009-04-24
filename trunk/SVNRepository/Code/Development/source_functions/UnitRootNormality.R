##Function returns p-values for:
## Unit Root Tests (ADF, PP, KPSS)
##
## Created Jan 21 2009

##--Unit Root Tests--##

"unit.root.test" = function(data){
	VAR20 = ar.ols(diff(data), aic = T, order.max = 20, demean = F, intercept = T)
	aic.order = as.numeric(names(which.min(VAR20$aic)))  ; 
	# compare this with BIC
	n = length(data) ;
	k = 1 ;
	d = k+0:20*k*2;
	VAR20$bic = VAR20$aic - 2*d + d*log(n);
	bic.order = as.numeric(names(which.min(VAR20$bic)));
	adf.test.order = min(bic.order,aic.order)	;	

	adfLag = adf.test.order

	#adf.test.results = adf.test(data, k=adfLag,alternative = c("stationary"))$p.value
	adf.test.results = adfTest(data,lags=adfLag,type=c("c"))@test$p.value

	pp.test.results = PP.test(data)$p.value
	kpss.test.results = kpss.test(data, null="Level", lshort=TRUE)$p.value
	
	#adf.test.results2 

	structure(list(adf.p.value=adf.test.results,pp.p.value=pp.test.results,kpss.p.value=kpss.test.results, adf.lag.value = adfLag))
	
}






