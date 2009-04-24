#data codes
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
#housing_price_index

##set directory for the initial analysis here, function calls will move relative to this
#setwd("Z:/SVNRepository/Code/Development/initial_analysis_r_code/")
setwd("C:/Users/tim/Documents/ClassesSpring09/MastersProject/SVNRepository/timr_FinEngProj09/Code/Development/initial_analysis_r_code")
setwd("Z:/M.Eng Project/SVNRepository/Code/Development/initial_analysis_r_code/")

source("../source_functions/get.sd.data.R")
source("../source_functions/get.sreturn.R")

data_code="population_size"
data_code = "building_permits"
data_code = "median_house_median_income"
state = ""

supply_demand_data_code_vector=c(

#"median_income",
"30_year_current_coupon",
"30_year_commitment_rate",
#"mortgage_originations",
"unemployment_rate",
#"primary_interest_rate",
"population_size",
#"population_growth",
"median_house_median_income"
#"delta_from_median_house_median_income"
#"foreclosures",
#"housing_stock",
#"inventory_turnover",
#"building_permits"
)


housing_data_code = c("housing_price_index")



begin_month=1
begin_year=1985
state = "AR"
end_month = 12
end_year = 2008

sd_data_vector = NULL
state_data = get.sd.data(housing_data_code,state,begin_month,begin_year,end_month,end_year)$interpolated_data

i=1
for (i in 1:length(supply_demand_data_code_vector)){
	if(length(sd_data_vector)==0){
		sd_data_vector_temp = as.numeric(as.matrix(get.sd.data(supply_demand_data_code_vector[i],state,begin_month,begin_year,end_month,end_year)$interpolated_data))
		#sd_data_vector_temp = sd_data_vector_temp[dim(sd_data_vector_temp)[2]]
		sd_data_vector_temp = as.data.frame(sd_data_vector_temp)
		names(sd_data_vector_temp)=supply_demand_data_code_vector[i]
		sd_data_vector = sd_data_vector_temp
	}else{
		sd_data_vector_temp = as.numeric(as.matrix(get.sd.data(supply_demand_data_code_vector[i],state,begin_month,begin_year,end_month,end_year)$interpolated_data))
		#sd_data_vector_temp = sd_data_vector_temp[dim(sd_data_vector_temp)[2]]
		sd_data_vector_temp = as.data.frame(sd_data_vector_temp)

		names(sd_data_vector_temp)=supply_demand_data_code_vector[i]
		sd_data_vector = cbind(sd_data_vector,sd_data_vector_temp)
	}
}

acf(sd_data_vector)



## 30_year_current_coupon 30_year_commitment_rate unemployment_rate population_size median_house_median_income
sd_data_vector_sreturn = sapply(sd_data_vector, "get.sreturn")

acf(sd_data_vector_sreturn)

## HPI DATA
ar.data = as.data.frame(ar.data)
names(ar.data) = "AR"
ar.data.sr = sapply(ar.data, "get.sreturn")

## ANALYSIS
xlabel = "30 Year Current Coupon"
x = sd_data_vector_sreturn[,4]
y = ar.data.sr

plot(x, type = "l", lwd = 1, xlab = "", ylab = "30 Year Current Coupon")



LR = lm(y ~ x)
LR
summary(LR)$adj.r.squared
data.toPlot = as.matrix(cbind(x,y) )
plot(x,y, , lwd = 2, xlab = "population_size", ylab = "AR HPI")
abline(coef(LR)[1],coef(LR)[2], lwd = 1)

CI = predict(LR,interval="confidence",level=0.95)
abline(line(CI[,2])$coefficients[1], line(CI[,2])$coefficients[2], lty = 2)
abline(line(CI[,3])$coefficients[1], line(CI[,3])$coefficients[2], lty = 2)

plot(x,y, xlab = xlabel, ylab = "AR HPI")
acf(data.toPlot)


