#library(fSeries)

"get.sreturn" = function(vector){
	#handle 0 values by interpolating (or using previous values to force returns to "0"
	#vector[which(vector==0)]=NA
	#vector=interpNA(vector, method = c("before"))
	vector.sreturn = (vector[2:length(vector)]-vector[1:(length(vector)-1)])/vector[1:(length(vector)-1)]
	vector.sreturn[which(vector.sreturn==Inf)]=0
	vector.sreturn[which(vector.sreturn==-Inf)]=0
	vector.sreturn[which(is.na(vector.sreturn))]=0

	
	vector.sreturn

}