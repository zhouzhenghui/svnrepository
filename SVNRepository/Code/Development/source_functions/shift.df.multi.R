	#creates a shifted dataframe with multiple indpendent variables at different lags
"shift.df.multi" = function(test.data.vector,state,independent.variables){
#get the lag vector for this variable
lag.vectors =  get.lagvectors(state)
new.dataframe.total = NULL
independents.total = NULL
indep.lagvector.total = NULL
new.dataframe.total = NULL
#do the 1 var procedure, as in shift.df, for each independent variable
k=4
for(k in 1:length(independent.variables)){
	new.dataframe = NULL
	indep.lagvector = as.matrix(na.omit(lag.vectors[which(names(lag.vectors)==independent.variables[k])]))
if(length(indep.lagvector)==0){
	#skip this one
}else{


	#replicate columns in test.data.vector for each version of the lagged variable
	#unless the lagvector column is all na

	replicant.vector = test.data.vector[,which(names(test.data.vector)==independent.variables[k])]
	for (i in 1:(length(indep.lagvector))){
		if (i==1){
			new.dataframe = cbind(new.dataframe,replicant.vector)
			new.dataframe = as.data.frame(new.dataframe)
		}else{
			new.dataframe = cbind(new.dataframe,replicant.vector)
		}
		names(new.dataframe)[i]=paste(independent.variables[k],"-Lag",indep.lagvector[i],sep="")
	}
	independents.total = c(independents.total,names(new.dataframe))
	indep.lagvector.total = c(indep.lagvector.total,indep.lagvector)
		if(is.null(new.dataframe.total)){
			new.dataframe.total=new.dataframe
		}else{
			new.dataframe.total = cbind(new.dataframe.total,new.dataframe)
		}
	}
}

#done iterating through all of the lagged independent variables
#add the hpi to the end
	new.dataframe.total = cbind(new.dataframe.total,test.data.vector[,which(names(test.data.vector)==state)])
	names(new.dataframe.total)[length(names(new.dataframe.total))]=state
return.dataframe = shifter(new.dataframe.total,state,independents.total,indep.lagvector.total)
return.dataframe
}