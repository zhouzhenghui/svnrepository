independent.vars = c("median_income","thirty_year_commitment_rate","mortgage_originations","unemployment_rate","population_size","foreclosures","building_permits")
dependent.var = "AZ"

#lag.vector = c(30,7,-24,29,0,8,3)
lag.vector = c(0	,35	,-25	,29,	0,	8	,0)


shifted.test.vector = shifter(olddataframe = combined_data_vector.sreturn,dependent=dependent.var,independents=independent.vars,lagvector=lag.vector)

