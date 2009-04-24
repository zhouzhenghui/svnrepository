	#creates a shifted dataframe with ONE indpendent variable at different lags
"shift.df" = function(test.data.vector,state,independent.variable){
#get the lag vector for this variable
lag.vectors =  get.lagvectors(state)

indep.lagvector = as.matrix(na.omit(lag.vectors[which(names(lag.vectors)==independent.variable)]))

#replicate columns in test.data.vector for each version of the lagged variable
new.dataframe = NULL
replicant.vector = test.data.vector[,which(names(test.data.vector)==independent.variable)]
for (i in 1:(length(indep.lagvector))){
	if (i==1){
		new.dataframe = cbind(new.dataframe,replicant.vector)
		new.dataframe = as.data.frame(new.dataframe)
	}else{
		new.dataframe = cbind(new.dataframe,replicant.vector)
	}
	names(new.dataframe)[i]=paste(independent.variable,"-Lag",indep.lagvector[i],sep="")
}
independents= names(new.dataframe)
#add the hpi to the end
	new.dataframe = cbind(new.dataframe,test.data.vector[,which(names(test.data.vector)==state)])
	names(new.dataframe)[i+1]=state
return.dataframe = shifter(new.dataframe,state,independents,indep.lagvector)
return.dataframe
}