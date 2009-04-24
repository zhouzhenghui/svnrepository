#type is the order or cross (1=order1, 2= order2, 3=cross)
"get.res.fm" = function(predictors,type = 1) {
     predictor_code = NULL
     #predictors for order 1
     for (j in 1:length(predictors)){
	  if (j==length(predictors)){
		predictor_code = paste(predictor_code,predictors[j],sep="") 
	  }else{
		predictor_code = paste(predictor_code,predictors[j]," + ",sep="") 
	  }	
     }
     if (type == 1) return(predictor_code)

     #predictors for order 2
     predictor_code = paste(predictor_code," + ", sep="")
     for (j in 1:length(predictors)){
	  if (j==length(predictors)){
		predictor_code = paste(predictor_code,"I(",predictors[j],"^2)",sep="") 
	  }else{
		predictor_code = paste(predictor_code,"I(",predictors[j],"^2)"," + ",sep="") 
	  }	
	
     }
     if (type == 2) return(predictor_code)

     #predictors for cross pairs
     predictor_code = paste(predictor_code," + ", sep="")
     special_mapping = seq(1,length(predictors),by=1)
     for (j in 1:(length(special_mapping)-1)){
        for (k in (j+1):length(special_mapping)){
	     if (k==length(special_mapping) && j==length(special_mapping)-1){
		   predictor_code = paste(predictor_code,"I(",predictors[j],"*",predictors[k],")",sep="") 
	     }else{
		   predictor_code = paste(predictor_code,"I(",predictors[j],"*",predictors[k],")"," + ",sep="") 
	     }
        }	
     }
     if (type == 3) return(predictor_code)
}