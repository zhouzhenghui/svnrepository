"ty.cr.curve"=function(state,data){
	
garch.model.tycr = garchFit(formula= ~arma(1,0) + garch(1,1),data)
newdata = c(data,predict(garch.model.tycr,60)$meanForecast)
structure(list(newdata=newdata,data = data,total=sum(newdata)))

}
