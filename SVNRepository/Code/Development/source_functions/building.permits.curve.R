"building.permits.curve"=function(state,data){
	#plot(as.ts(data))
	replicant=data[c(1:60)]	
	replicant2= diff(replicant,lag=12,differences = 1)
	VAR20 = ar.ols(replicant2, aic = T, order.max = 20, demean = F, intercept = T)
	#pacf(replicant2)
	aic.order = as.numeric(names(which.min(VAR20$aic)))  ; 
	# compare this with BIC
	n = length(data) ;
	k = 1 ;
	d = k+0:20*k*2;
	VAR20$bic = VAR20$aic - 2*d + d*log(n);
	bic.order = as.numeric(names(which.min(VAR20$bic)));
	adf.test.order = min(bic.order,aic.order)	;	

	#fit=auto.arima(replicant,max.P = 14, max.Q = 14,start.P=12, start.Q=12)
	fit2=arima(x=replicant, order = c(bic.order , 0, 0), seasonal = list(order = c(1, 0, 0), period = 12), xreg = NULL, include.mean = TRUE)
	prediction=as.matrix(predict(fit2,n.ahead=60)$pred)
	#plot(as.ts(prediction))
	#acf(prediction,lag.max = 60)
	#height.adjust=prediction[1]- data[length(data)]
	#prediction = prediction - height.adjust
	newdata=c(data,prediction)
	
	#plot(as.ts(newdata))
	#lines(as.ts(data),col="blue")
	structure(list(newdata=newdata,data = data,total=sum(newdata)))

}