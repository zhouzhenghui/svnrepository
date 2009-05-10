source("../source_functions/chop.R")
"plot.actual.fitted"=function(regression.dataframe,state,dates, sreturn=TRUE, same.scale=FALSE,adjust=T){
	start.adjust = ifelse(sreturn,1,0)
	chop.end = abs(min(get.lagvectors(state),na.rm=T))
	chop.begin = abs(max(get.lagvectors(state), na.rm=T)) + start.adjust
	if(adjust==F){
		dates.temp = dates
	}else{
	
		dates.temp = chop(dates,chop.begin, location = "beginning")
		dates.temp = chop(dates.temp,chop.end,location = "end")
	}
	test.and.plot = dataframe.eda(regression.dataframe, state, dates.temp, basicTS = F, comparisonTS = F, allTS = T, CCF = F, scatter = F, ACF = F, PACF = F, same.scale=same.scale)
	#test.and.plot
}