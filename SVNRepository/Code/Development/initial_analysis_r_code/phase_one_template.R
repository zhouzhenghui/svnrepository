#PHASE 1

#get the data
one_massive_strucutre_with_all_dataframes = get_all_data(states )$combined_data_vector, simple_returns, difference
#do basic EDA stuff

basic_eda_stuff(one_massive_strucutre_with_all_dataframes$combined_data_vector.sreturns)

#model fitting
	#things to specify
	plot_types
	predictors to use
	order (1,2,cross)
	lagvector_range(2 numbers for each variable) - > compute all the lag vectors to try and number of dupicates to use
			- > compute all the shifted dataframes to use (*)

	stepwise regression (*)
	sandwich estimator for pvalues (*vikas)

#residual checking
	plotting of model fit vs hpi, resids on 2ndary axis
	add [shifted] predictors as TS subplots
	residual checks (ADF, serial correlation, normality)
