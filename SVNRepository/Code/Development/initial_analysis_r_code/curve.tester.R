#mort.orig.curve.R
#foreclosure.curve.R
#median.income.curve.R
#population.size.curve.R
#building.permits.curve.R
#unemployment.rate.curve.R

states = read.csv("../../../Data/States/states.csv")
state.names = as.matrix(states$State)

begin_month=1
begin_year=1998
end_month = 6
end_year = 2008

for (i in 1:length(state.names)){
state = state.names[i] #our dependent variable 
data = grab.data(state,begin_month,begin_year,end_month,end_year,sreturn=T)
test.data.vector = data$test.data.vector
test.data.vector = as.data.frame(test.data.vector)
dates = data$dates
dates2 = dategen(begin_month,begin_year, end_month, end_year+5)  #begin month + 1 because we had to chop off the first month

for (k in 1:(length(names(test.data.vector))-1)){

	if (names(test.data.vector)=="median_income"){
		data = test.data.vector[,k]
		curvefit = median.income.curve(state,data)
	}
	if (names(test.data.vector)=="ty_cr"){
		#data = test.data.vector[,k]
		#curvefit = ty_cr.curve(state,data)
		data = data
		curvefit = curvefit 

	}
	if (names(test.data.vector)=="mort_orig"){
		data = test.data.vector[,k]
		curvefit = mort.orig.curve(state,data)
	}
	if (names(test.data.vector)=="unemp_rate"){
		data = test.data.vector[,k]
		curvefit = unemployment.rate.curve(state,data)
	}
	if (names(test.data.vector)=="pop_size"){
		data = test.data.vector[,k]
		curvefit = population.size.curve(state,data)
	}
	if (names(test.data.vector)=="foreclosures"){
		data = test.data.vector[,k]
		curvefit = foreclosure.curve(state,data)
	}
	if (names(test.data.vector)=="building_permits"){
		data = test.data.vector[,k]
		curvefit = building.permits.curve(state,data)
	}

	plot(as.ts(curvefit$newdata),col="black")
	lines(as.ts(curvefit$data),col="blue")
}



}


}
