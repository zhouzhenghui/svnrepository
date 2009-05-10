
##Takes current state name, and derives appropriate model with the first n datapoints

"auto.model" = function(current.state.name, percentage, cut.off=FALSE, interp=FALSE, data=NULL){

	
all.data = read.table("../../../Data/States/all.txt");
Para = read.csv("../../../Data/Supply Demand/Data/states.csv");

for(k in 1:(length(Para$States))){
       if (Para$States[k] == current.state.name) i = k;
    }
		if(is.null(data)){
			current.state.data = subset(all.data,all.data[,1]==current.state.name)[,4]; 		
		}else{
			current.state.data = data
		}
	      
		current.data = current.state.data   

    		if(cut.off==TRUE){
			current.data = cutoff(current.data,method="constant",constant=40)
    		}
    		if(interp==TRUE){
			current.data = interpolate(current.data,method="linear")
    		}	
		if(is.null(data)){
			current.state.data.sreturn = (current.data[2:length(current.data)]-current.data[1:(length(current.data)-1)])/current.data[1:(length(current.data)-1)];   
			current.data = current.state.data.sreturn 		
		}

    		if (Para$unitroot[i] > 0){
       		current.data = diff(current.data,lag=1,differences=Para$unitroot[i]);
    		}else{
	 		current.data = current.data;
		}

    		n=round(length(current.data)*percentage,0)
    		current.data = current.data[1:n];
     
		if (Para$d[i] > 0){
			d.est = whittleFit(as.ts(current.data), order = c(0, 0), subseries = 1, method = c("farma"),trace = TRUE, spec = FALSE, title = NULL, description = NULL)@hurst$H  - 0.5
			#d.est = fdSperio(as.ts(current.data))$d
       		current.data = diffseries(as.ts(current.data),d.est);
    		}else{
	 		current.data = current.data;
			d.est = 0
		}


#     current.fix = array(NA,(1+Para$AR.order[i]+Para$MA.order[i]+Para$Seasonal.AR.order[i]+Para$Seasonal.MA.order[i]));
     current.fix = array(NA,(1+Para$AR.order[i]+Para$MA.order[i]));

    
    current.AR.Drop = as.numeric(unlist(strsplit(as.character(Para$AR.order.1[i]), ",")));
    current.MA.Drop = as.numeric(unlist(strsplit(as.character(Para$MA.order.1[i]), ",")));
    
    current.beta.Drop =  as.numeric(unlist(strsplit(as.character(Para$beta.order.1[i]), ",")));
    current.alpha.Drop = as.numeric(unlist(strsplit(as.character(Para$alpha.order.1[i]), ",")));
	
	
    if(current.AR.Drop[1] != 0){
      for(j in 1:(length(current.AR.Drop))){
          current.fix[current.AR.Drop[j]] = 0;
      }
    }
    if(current.MA.Drop[1] != 0){
      for(j in 1:(length(current.MA.Drop))){
          current.fix[Para$AR.order[i]+current.MA.Drop[j]] = 0;
      }
    }
    if(Para$intercept[i] != 1){
       current.fix[Para$AR.order[i]+Para$MA.order[i]+1] = 0;
    }

    
  

   if (Para$alpha.order[i]==0 && Para$beta.order[i]==0 ){ 
      model.temp = arima(current.data,order = c(Para$AR.order[i],Para$d[i],Para$MA.order[i]),seasonal = list(order=c(Para$Seasonal.AR.Order[i],Para$d.1[i],Para$Seasonal.MA.Order[i])),fix = current.fix)
	 

   }else{
      #garch.formula = as.formula(paste("~arma(",Para$AR.order[i],",",Para$MA.order[i],")+garch(",max(0,Para$alpha.order[i]-current.alpha.Drop),",",max(0,Para$beta.order[i]-current.beta.Drop),")"));
	garch.formula = as.formula(paste("~arma(",Para$AR.order[i],",",Para$MA.order[i],")+garch(",Para$alpha.order[i],",",Para$beta.order[i],")"));
	
      model.temp = garchFit(formula = garch.formula, data=current.data, cond.dist = c("QMLE"), include.mean=(Para$intercept[i]==1), trace=FALSE);
   }
		
   	mean.num.of.params = max(0,Para$AR.order[i]+Para$d[i]+Para$MA.order[i] - (ifelse(current.AR.Drop[1]==0,0,length(current.AR.Drop)) + ifelse(current.MA.Drop[1]==0,0,length(current.MA.Drop)))) 
	vol.number.of.params = max(0,Para$alpha.order[i] + Para$beta.order[i]- (ifelse(current.beta.Drop[1]==0,0,length(current.beta.Drop)) + ifelse(current.alpha.Drop[1]==0,0,length(current.alpha.Drop))));
	model.check.temp = model.check(model.temp,mean.num.of.params,vol.number.of.params,12)



   return(structure(list(model=model.temp, model.check=model.check.temp, n=n, data = current.data, d= d.est)));
}

