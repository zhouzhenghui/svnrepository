"forecaster" = function(driftmodel,volmodel,independent.variables,new.curves,plot=T,plotindex=T, plot.ses = T){

projections.shifted = shift.df.multi(new.curves,state,independent.variables)
projections.shifted = as.data.frame(projections.shifted)
projections.shifted = get.tim.data(projections.shifted)
new.forecasting.data = projections.shifted[,which(names(projections.shifted)!=state)]

predictions=as.ts(predictor(model,newdata = new.forecasting.data))
phase1.predictions = predictions[c((length(predictions)-59):length(predictions))]
phase1.se = 
phase2.predictions = predict(volmodel,n.ahead=60, se.fit=TRUE)$pred;
phase2.se = predict(volmodel,n.ahead=60, se.fit=TRUE)$se;

total.predictions = phase1.predictions+phase2.predictions



dev.new()
lines(phase1fits)


}