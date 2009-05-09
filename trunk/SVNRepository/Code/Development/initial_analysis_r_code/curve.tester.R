#mort.orig.curve.R
#foreclosure.curve.R
#median.income.curve.R
#population.size.curve.R
#building.permits.curve.R
#unemployment.rate.curve.R
#ty.cr.curve

states = read.csv("../../../Data/States/states.csv")
state.names = as.matrix(states$State)

begin_month=1
begin_year=1998
end_month = 6
end_year = 2008

for (i in 1:length(state.names)){
state = state.names[i] #our dependent variable 
if(state!="DC"){

data = grab.data(state,begin_month,begin_year,end_month,end_year,sreturn=F)
test.data.vector = data$test.data.vector
test.data.vector = as.data.frame(test.data.vector)
dates = data$dates
dates2 = dategen(begin_month,begin_year, end_month, end_year+5)  #begin month + 1 because we had to chop off the first month
par(mfrow=c(2,4))
for (k in 1:(length(names(test.data.vector))-5)){

	if (names(test.data.vector)[k]=="median_income"){
		data = test.data.vector[,k]
		curvefit = median.income.curve(state,data)
	}
	if (names(test.data.vector)[k]=="ty_cr"){
		data = test.data.vector[,k]
		curvefit = ty.cr.curve(state,data)		

	}
	if (names(test.data.vector)[k]=="mort_orig"){
		data = test.data.vector[,k]
		curvefit = mort.orig.curve(state,data)
	}
	if (names(test.data.vector)[k]=="unemp_rate"){
		data = test.data.vector[,k]
		curvefit = unemployment.rate.curve(state,data)
	}
	if (names(test.data.vector)[k]=="pop_size"){
		data = test.data.vector[,k]
		curvefit = population.size.curve(state,data)
	}
	if (names(test.data.vector)[k]=="foreclosures"){
		data = test.data.vector[,k]
		curvefit = foreclosure.curve(state,data)
	}
	if (names(test.data.vector)[k]=="building_permits"){
		data = test.data.vector[,k]
		curvefit = building.permits.curve(state,data)
	}

	plot(as.ts(curvefit$newdata),col="black", main = state, ylab = names(test.data.vector)[k])
	lines(as.ts(curvefit$data),col="blue")
	

}
dev.new()
}
}
