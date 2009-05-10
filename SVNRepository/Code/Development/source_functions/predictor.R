"predictor" = function(model, newdata){
 
tempsum = 0
for(i in 1:length(names(model$coefficients))){

tempsum = tempsum + as.numeric(model$coefficients[i])*newdata[,which(names(newdata)==names(model$coefficients)[i])]

}

tempsum
}