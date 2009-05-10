"curvefit"=function(test.data.vector,state,plot=F){
curve=NULL
lagranges = get.lagvectors(state)
if(length(which(lagranges<0))==0){
max.negative.lag =1
}else{
max.negative.lag = abs(min(as.numeric(lagranges[which(lagranges<0)]))) + 1
}

if(plot==TRUE){
par(mfrow=c(2,4))

}

for (k in 1:(length(names(test.data.vector))-5)){
	
	lag.to.use = lagranges[,which(names(lagranges)==names(test.data.vector)[k])]
	lag.to.use = ifelse(is.na(lag.to.use),1,lag.to.use)
	lag.to.use = ifelse(lag.to.use<0,abs(lag.to.use),1)

	if (names(test.data.vector)[k]=="median_income"){
		data = test.data.vector[,k]		
		curvefit = median.income.curve(state,data,lag=lag.to.use)
	}
	if (names(test.data.vector)[k]=="ty_cr"){
		data = test.data.vector[,k]
		curvefit = ty.cr.curve(state,data,lag=lag.to.use)
	}
	if (names(test.data.vector)[k]=="mort_orig"){
		data = test.data.vector[,k]
		curvefit = mort.orig.curve(state,data,lag=lag.to.use)
	}
	if (names(test.data.vector)[k]=="unemp_rate"){
		data = test.data.vector[,k]
		curvefit = unemployment.rate.curve(state,data,lag=lag.to.use)
	}
	if (names(test.data.vector)[k]=="pop_size"){
		data = test.data.vector[,k]
		curvefit = population.size.curve(state,data,lag=lag.to.use)
	}
	if (names(test.data.vector)[k]=="foreclosures"){
		data = test.data.vector[,k]
		curvefit = foreclosure.curve(state,data,lag=lag.to.use)
	}
	if (names(test.data.vector)[k]=="building_permits"){
		data = test.data.vector[,k]
		curvefit = building.permits.curve(state,data,lag=lag.to.use)
	}
	if((max.negative.lag-lag.to.use)>0){
		na.pad = rep(NA,(max.negative.lag-lag.to.use))
		curvefit = curvefit$newdata
		curvefit = c(curvefit,na.pad)		
	}else{
		curvefit = curvefit$newdata

	}
	
if(is.null(curve)){
	curve = curvefit
	curve = as.data.frame(curve)
	names(curve)[dim(curve)[2]]=names(test.data.vector)[k]

}else{
	curve=cbind(curve,curvefit)
	names(curve)[dim(curve)[2]]=names(test.data.vector)[k]
}
	if(plot==TRUE){
		plot(as.ts(curvefit$newdata),col="black", main = state, ylab = names(test.data.vector)[k])
		lines(as.ts(curvefit$data),col="blue")
	}
	

}
statedata = test.data.vector[,which(names(test.data.vector)==state)]
na.pad = rep(NA,(60+max.negative.lag))
statedata = c(statedata,na.pad)
curve=cbind(curve,statedata)
names(curve)[dim(curve)[2]]=state

curve 
}