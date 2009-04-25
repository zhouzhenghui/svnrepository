"grab.data"=function(state,begin_month,begin_year,end_month,end_year,sreturn=T){
supply_demand_data_code_vector=c(
"median_income",
#"thirty_year_current_coupon",
"ty_cr",
"mort_orig",
"unemp_rate",
#"primary_interest_rate",
"pop_size",
#"population_growth",
#"median_house_median_income",
#"delta_from_median_house_median_income"
"foreclosures",
#"housing_stock",
#"inventory_turnover",
"building_permits"
)

adjustment = ifelse(sreturn,1,0)
test.data.frame = get.all.data(supply_demand_data_code_vector,state,begin_month,begin_year,end_month,end_year,smooth=TRUE) 
if(sreturn){
	test.data.vector = test.data.frame$combined_spec # this has the mean-centered rates (non-simple return), and simple returns
}else{
	test.data.vector = test.data.frame$combined
}
dates = dategen(begin_month+adjustment,begin_year, end_month, end_year)  #begin month + 1 because we had to chop off the first month

structure(list(test.data.vector=test.data.vector,dates=dates, independent.variables = supply_demand_data_code_vector))

}