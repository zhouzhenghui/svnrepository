"adf.urca.test"=function(data){
	VAR20 = ar.ols(diff(data), aic = T, order.max = 20, demean = F, intercept = T)
	aic.order = as.numeric(names(which.min(VAR20$aic)))  ; 
	# compare this with BIC
	n = length(data) ;
	k = 1 ;
	d = k+0:20*k*2;
	VAR20$bic = VAR20$aic - 2*d + d*log(n);
	bic.order = as.numeric(names(which.min(VAR20$bic)));
	adf.test.order = min(bic.order,aic.order)	;	

	
	lag=adf.test.order+1
	critvals=ur.df(resids,lags=lag,type="none")@cval
	teststat=ur.df(resids,lags=lag,type="none")@teststat
	#adf.test(resids,k=1)


structure(list(critvals=critvals,teststat=teststat,lag=lag))
}