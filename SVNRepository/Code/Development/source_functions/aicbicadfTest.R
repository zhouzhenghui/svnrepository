"aic.bic.adf.Test" = function (adf.data, unitroot){

#ar order determination
		VAR20 = ar.ols(adf.data, aic = T, order.max = 20, demean = F, intercept = T)	;;
		aic.order = as.numeric(names(which.min(VAR20$aic)))  ; 
		# compare this with BIC
		n = length(adf.data) ;
		k = 1 ;
		d = k+0:20*k*2;
		VAR20$bic = VAR20$aic - 2*d + d*log(n);
		bic.order = as.numeric(names(which.min(VAR20$bic)));
		adf.test.order = min(bic.order,aic.order)	;	
		
		#perform adf TEST on original
		
		
		adfTest.c.statistic = adfTest(adf.data ,lags=adf.test.order+unitroot,type=c("c"))@test$statistic;
		adfTest.c.pval = adfTest(adf.data ,lags=adf.test.order+unitroot,type=c("c"))@test$p.value;

		adfTest.ct.statistic = adfTest(adf.data ,lags=adf.test.order+unitroot,type=c("ct"))@test$statistic;
		adfTest.ct.pval = adfTest(adf.data ,lags=adf.test.order+unitroot,type=c("ct"))@test$p.value;

		adfTest.nc.statistic = adfTest(adf.data ,lags=adf.test.order+unitroot,type=c("nc"))@test$statistic;
		adfTest.nc.pval = adfTest(adf.data ,lags=adf.test.order+unitroot,type=c("nc"))@test$p.value;
		
		descriptors = c(	"intercept - c statistic",
					"intercept - c p-val",
					"no intercept - nc statistic",
					"no intercept - nc p-val",
					"intercept trend - ct statistic",
					"intercept trend- ct p-val",
					"lags");
		values = 		as.numeric(c(	adfTest.c.statistic,
					adfTest.c.pval,
					adfTest.ct.statistic ,
					adfTest.ct.pval ,
					adfTest.nc.statistic,
					adfTest.nc.pval ,
					adf.test.order));

		as.data.frame(cbind(descriptors,as.numeric(values)))




}