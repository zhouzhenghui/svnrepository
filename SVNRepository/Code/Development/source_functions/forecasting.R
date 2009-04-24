
"forecasting" = function(current.state.name,percentage, cutoff=FALSE, interpolate=FALSE){
      setwd("Z:/SVNRepository/Code/Development/source_functions")

      source("ATS_2008.R")
      source("autoModel.R")
      all.data = read.table("../../../Data/States/all.txt");
      
      current.state.data = subset(all.data,all.data[,1]==current.state.name)[,4];
      current.state.data.sreturn= (current.state.data[2:length(current.state.data)]-current.state.data[1:(length(current.state.data)-1)])/current.state.data[1:(length(current.state.data)-1)];

      
      data 	= current.state.data.sreturn;
      pre = array(NA,(length(data)-n)); 
      condsd = array(NA,(length(data)-n));
	se = array(NA,(length(data)-n));      
      boxmean= array(NA,(length(data)-n));
	boxvol= array(NA,(length(data)-n));

	current.data = current.state.data.sreturn   

	if(cutoff==TRUE){
		current.data = cutoff(current.data,method="constant",constant=40)
   	}
    	if(interpolate==TRUE){
		current.data = interpolate(current.data,method="piecewise_spline")
    	}	

	n=round(length(current.data)*percentage,0)

         for(i in 1:((length(current.data)-n))){
            model = auto.model(current.state.name,n+i-1,cutoff,interpolate)$model
		
		boxmean[i] = model$model.check$mean.check.p.value
            boxvol[i] = model$model.check$vol.check.p.value
            if ((class(model)) == "fGARCH"){
               pre[i] = predict(model,1)$meanForecast;
		   se[i] = 	predict(model,1)$meanError;
		   condsd[i] = 	predict(model,1)$standardDeviation
            }else{
               pre[i] = predict(model,1, se.fit=TRUE)$pred;
		   se[i] = 	predict(model,1, se.fit=TRUE)$se;
            }
         }
      
      comp = data[(n+1):length(data)];
      rmse.temp = (sum((pre - comp)^2)/(length(data)-n))^(0.5);
      return(structure(list(rmse=rmse.temp,mean.predictions=pre, stderrs = se, 
		cond.sd = condsd,meantests=boxmean,voltests=boxvol)));	
}
