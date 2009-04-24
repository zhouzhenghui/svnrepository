
"temporal.holdout" = function(current.state.name,percentage, cutoff=FALSE, interpolate=FALSE, nahead = 1){
      #setwd("Z:/SVNRepository/Code/Development/source_functions")

     # source("ATS_2008.R")
      source("../source_functions/autoModel.R")
      all.data = read.table("../../../Data/States/all.txt");
      
     
      pre = NULL
      condsd = NULL
	se = NULL      
      boxmean= NULL
	boxvol= NULL

	#compute one model to figure out what "total n" is
	model = auto.model(current.state.name,1,cutoff,interpolate)
	N = model$n
	n = round(percentage*N,0)
	data = model$data
         for(i in 1:(N-n)){
		allmodel =  auto.model(current.state.name,percentage=(i+n-1)/N,cutoff,interpolate)
            model = allmodel$model
		
		boxmean = c(boxmean,allmodel$model.check$mean.check.p.value);
            boxvol = c(boxvol,allmodel$model.check$vol.check.p.value);
            if ((class(model)) == "fGARCH"){
               pre = c(pre,predict(model,n.ahead=nahead)$meanForecast);
		   se = 	c(se,predict(model,n.ahead=nahead)$meanError);
		   condsd = 	c(condsd,predict(model,n.ahead=nahead)$standardDeviation);
            }else{
               pre = c(pre,predict(model,n.ahead=nahead, se.fit=TRUE)$pred);
		   se = 	c(se,predict(model,n.ahead=nahead, se.fit=TRUE)$se);
            }
         }
      
      comp = data[(n+1):N];
      rmse.temp = (sum((pre - comp)^2)/(N-n))^(0.5);
      return(structure(list(rmse=rmse.temp,mean.predictions=pre, stderrs = se, 
		cond.sd = condsd,meantests=boxmean,voltests=boxvol, x = data, n = n)));	
}
