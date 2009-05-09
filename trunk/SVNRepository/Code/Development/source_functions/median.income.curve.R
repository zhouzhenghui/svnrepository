"median.income.curve"=function(state,data){
	inflation= 0.015
	middle.element =  mean(data)

	last.element =  data[length(data)]

	#x.s = c((length(data)-59):length(data))
	#x.s.2 = c((length(data)+1):(length(data)+60))
	x.s = c(0:(length(data)-1))
	
	x.s.2 = c((length(data)-59):length(data))


	y1 = c(middle.element,middle.element*(1+inflation)^4)
	y2 = c(last.element,last.element*(1+inflation)^4)

	slope1= (y1[2]-y1[1])/60
	slope2= (y2[2]-y2[1])/60

	xreg.var = slope1*x.s + middle.element

	xreg.var2 = slope2*x.s.2 + last.element
	replicant=data[c((length(data)-59):length(data))]
	replicant=data
	fit=auto.arima(replicant,xreg=xreg.var)
	#fit=arima(x=replicant,order = c(2, 0, 0),xreg=xreg.var)

	#plot(as.ts(replicant))
	#lines(as.ts(xreg.var))
	#lines(as.ts(replicant-fit$residuals))
	prediction = as.matrix(predict(fit,n.ahead=60,newxreg=xreg.var2)$pred)
	prediction=prediction[,dim(prediction)[2]]
	#plot(as.ts(prediction))
	#plot(as.ts(c(data,prediction)),col="blue")
	#lines(as.ts(data),col="black")
	height.adjust = prediction[1]-last.element
	prediction= prediction - height.adjust

	newdata=c(data,prediction)
	
	#plot(as.ts(newdata))
structure(list(newdata=newdata,data = data,total=sum(newdata)))

}