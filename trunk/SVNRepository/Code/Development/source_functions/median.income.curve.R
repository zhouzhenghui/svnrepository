"median.income.curve"=function(state,data){
	inflation= 0.03
	middle.element =  data[(length(data)-59)]

	last.element = data[length(data)]
	#x.s = c((length(data)-59):length(data))
	#x.s.2 = c((length(data)+1):(length(data)+60))
	x.s = c(0:59)
	x.s.2 = x.s
	xreg.var = .03*x.s +  middle.element
	xreg.var2 = .03*x.s.2 + last.element
	replicant=data[c((length(data)-59):length(data))]
	fit=auto.arima(replicant,xreg=xreg.var)
	#plot(as.ts(replicant))
	lines(as.ts(replicant-fit$residuals))
	prediction = as.matrix(predict(fit,n.ahead=60,newxreg=xreg.var2)$pred)
	#plot(as.ts(prediction))
	#plot(as.ts(c(data,prediction)),col="blue")
	#lines(as.ts(data),col="black")
	height.adjust = prediction[1]-last.element
	prediction= prediction - height.adjust

	newdata=c(data,prediction)
	
	#plot(as.ts(newdata))
stucture(list(newdata=newdata,data = data,total=sum(newdata)))

}