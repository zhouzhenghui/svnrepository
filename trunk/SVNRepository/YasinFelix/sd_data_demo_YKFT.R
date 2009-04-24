#median_income
#30_year_current_coupon
#30_year_commitment_rate
#mortgage_originations
#unemployment_rate
#primary_interest_rate
#population_size
#population_growth
#median_house_median_income
#delta_from_median_house_median_income
#foreclosures
#housing_stock
#inventory_turnover
#building_permits


##set directory for the initial analysis here, function calls will move relative to this
#setwd("Z:/SVNRepository/Code/Development/initial_analysis_r_code/")
setwd("C:/Users/tim/Documents/ClassesSpring09/MastersProject/SVNRepository/timr_FinEngProj09/Code/Development/initial_analysis_r_code")
setwd("Z:/M.Eng Project/SVNRepository/Code/Development/initial_analysis_r_code/")

source("../source_functions/get.sd.data.R")

data_code = "median_house_median_income"

begin_month=1
begin_year=1985
state = ""
end_month = 12
end_year = 2008


response = get.sd.data(data_code,state,begin_month,begin_year,end_month,end_year)
response.interp = as.ts(as.matrix(response$interpolated_data))

##response.interp = response$interpolated_data[7:length(response$interpolated_data)]


housing_data_code = c("housing_price_index")
ar.data = as.ts(as.matrix(get.sd.data(housing_data_code,state,begin_month,begin_year,end_month,end_year)$interpolated_data))



## GET HPI FOR AR
data.edit = as.ts(read.table("../../../Data/States/ar.txt"))
data.toTruncate = data.edit[,4]
length(data.edit[,4])
length(data.toTruncate)
data = data.toTruncate[41:length(data.toTruncate)]

data.temp = NULL

for(i in 1:(length(data)-1)){
		interpolated = approx(data[i:(i+1)],n=4)
		data.temp=c(data.temp,data[i],interpolated$y[2:3])
}

ar.data = c(data.temp, data[length(data)])
ar.data = data.temp

length(ar.data)
length(response.interp)

## ANALYSIS
dataname = "Median House / Median Income"

plot(response.interp, type = "l", lwd = 1, xlab = "", ylab = dataname)

x = as.ts(as.numeric(as.matrix(response.interp)))
y = as.ts(as.numeric(as.matrix(ar.data)))

LR = lm(y ~ x)
LR
summary(LR)$adj.r.squared
data.toPlot = as.matrix(cbind(x,y) )
plot(x,y, lwd = 1, xlab = dataname, ylab = "AR HPI")
abline(coef(LR)[1],coef(LR)[2], lwd = 1)

CI = predict(LR,interval="confidence",level=0.95)
abline(line(CI[,2])$coefficients[1], line(CI[,2])$coefficients[2], lty = 2)
abline(line(CI[,3])$coefficients[1], line(CI[,3])$coefficients[2], lty = 2)

plot(data.toPlot, xlab = "Unemployment", ylab = "AR HPI")
acf(data.toPlot)

