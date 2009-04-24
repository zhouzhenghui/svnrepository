"standardize" = function(test.dataframe){

	temp.dataframe.means = sapply(test.dataframe,"mean")
	temp.dataframe.sd = sapply(test.dataframe,"sd")
	temp.dataframe = test.dataframe -  temp.dataframe.means 
	temp.dataframe = temp.dataframe / temp.dataframe.sd
	temp.dataframe
}