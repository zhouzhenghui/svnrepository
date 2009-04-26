"do_bic_lars_regressions"=function(state,test.data.vector,independent.variables){
counter=0
independent.variable=independent.variables[1]
for (independent.variable in independent.variables){
#do the time shiftin here, lags depend on ranges entered in lagranges.csv
shifted.multivar.test.data.vector = shift.df.multi(test.data.vector,state,independent.variable)

if(dim(shifted.multivar.test.data.vector)[2]!=0){

		#try a best subset BIC/R^2 regression to determine the most significant lag(s)
		form = as.formula(paste(state," ~ .",sep=""))
		bs.reg = regsubsets(x= form, nbest=1, data = shifted.multivar.test.data.vector, nvmax=30,intercept=FALSE, method=c("exhaustive"), really.big=FALSE)
		dev.new()
		plot(x=bs.reg, labels=bs.reg$xnames, scale=c("bic"), col="blue", main = paste("Stepwise BIC Regression of",state))
		#try a least angle regression (LARS) to determine the most significant lag(s)
		predictors = shifted.multivar.test.data.vector
		predictors=as.matrix(predictors[,which(names(predictors)!=state)])
		response = as.matrix(subset(shifted.multivar.test.data.vector, select = state))

		lars.reg = lars(x=predictors, y=response, type = c("lar"))
		dev.new()
		plot(lars.reg,main=independent.variable)		
		sink(paste(state,"-lars.txt",sep=""), append = (counter!=0))
		print(lars.reg)
		sink()
	}
	counter = counter + 1
}

}