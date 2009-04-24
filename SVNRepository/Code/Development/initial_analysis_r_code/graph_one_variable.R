#local copy wd
setwd("Z:/SVNRepository/Code/Development/initial_analysis_r_code/")

source("../source_functions/get.sd.data.R")
source("../source_functions/get.sreturn.R")
source("../source_functions/format.dates.R")

begin_month=1
begin_year=1995
state = "AK"
end_month = 12
end_year = 2008

data_code="building_permits"
var_data = as.data.frame(get.sd.data(data_code,state,begin_month,begin_year,end_month,end_year))
dates = format.dates(as.matrix(var_data$data.Date))
#dates = as.matrix(var_data$data.Date)

data = as.numeric(as.matrix(var_data$interpolated_data))
dates.sreturns = dates
data.sreturns = get.sreturn(data)
data.sreturns = c(data.sreturns[1],data.sreturns)
plot(as.ts(data),xaxt="n",ylab=paste(state,"Building Permits Issuance"),xlab="", main=paste("Time Series Plot of",state,"Building Permits Issuance"), lwd = 2)
x.coords = seq(from=1,to=length(data),by=12)
tick.coords = seq(from=1,to=length(data), by = 1)
quarter.coords = seq(from=1,to=length(data), by = 3)

axis(side=1,at=x.coords,labels = dates[x.coords], las =2,  tick = TRUE, tck=-.04)
axis(side=1,at=quarter.coords,labels = rep("",length(quarter.coords)), las =2, cex = 0.5, tick = TRUE)
dev.new()
plot(as.ts(data.sreturns),xaxt="n",ylab=paste(state,"Building Permits Issuance","Simple Returns"),xlab="", main=paste("Time Series Plot of",state,"Building Permits Issuance","Simple Returns"), lwd = 2)

axis(side=1,at=x.coords,labels = dates.sreturns[x.coords], las =2,  tick = TRUE, tck=-.04)
axis(side=1,at=quarter.coords,labels = rep("",length(quarter.coords)), las =2, cex = 0.5, tick = TRUE)




