sp500=get.sd.data("sp500",state = "",begin_month,begin_year,end_month,end_year+1)$interpolated_data
amzn = get.sd.data("amzn",state = "",begin_month,begin_year,end_month,end_year+1)$interpolated_data

state.data = projections$projectedindex
length1 = length(sp500)
length2 = length(state.data)

na.pad = rep(NA,(length2-length1))
sp500=c(sp500,na.pad)
amzn = c(amzn,na.pad)


regression.dataframe = as.data.frame(cbind(state.data,sp500,amzn) )
names(regression.dataframe) = c(state,"SP 500", "Amazon")

regression.dataframe = as.data.frame(cbind(state.data,sp500) )
names(regression.dataframe) = c(state,"SP 500")

dates2 = dategen(begin_month,begin_year, end_month, end_year+5)  #begin month + 1 because we had to chop off the first month

plot.actual.fitted(regression.dataframe, state, dates=dates2,sreturn=F, same.scale=F,adjust=F)

