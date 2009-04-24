"auto.model" = function(current.state.name,n){

setwd("Z:/SVNRepository/Tommy")

source("ATS_2008.R")

all.data = read.table("../Data/States/all.txt");
Para = read.csv("../Data/States/states.csv");

for(k in 1:(length(Para$States))){
       if (Para$States[k] == current.state.name) i = k;
    }

    current.state.data = subset(all.data,all.data[,1]==current.state.name)[,4];
    current.state.data.sreturn = (current.state.data[2:length(current.state.data)]-current.state.data[1:(length(current.state.data)-1)])/current.state.data[1:(length(current.state.data)-1)];

    current.fix = array(NA,(1+Para$AR.order[i]+Para$MA.order[i]));
    
    current.AR.Drop = as.numeric(unlist(strsplit(as.character(Para$AR.order.1[i]), ",")));
    current.MA.Drop = as.numeric(unlist(strsplit(as.character(Para$MA.order.1[i]), ",")));
    
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
    
    if (Para$unitroot[i] > 0){
       current.data = diff(current.state.data.sreturn,lag=1,differences=Para$unitroot[i])[1:n];
    }else{
	 current.data = current.state.data.sreturn[1:n];
	}
   if (Para$alpha.order[i]==0 && Para$beta.order[i]==0 ){ 
      model = arima(current.data,order = c(Para$AR.order[i],Para$d[i],Para$MA.order[i]),seasonal = list(order=c(Para$Seasonal.AR.Order.1[i],Para$d.1[i],Para$Seasonal.MA.Order.1[i])),fix = current.fix)
   }else{
      garch.formula = as.formula(paste("~arma(",Para$AR.order[i],",",Para$MA.order[i],")+garch(",Para$alpha.order[i],",",Para$beta.order[i],")"));
      model = garchFit(formula = garch.formula, data=current.data, cond.dist = c("QMLE"), include.mean=(Para$intercept[i]==1));
   }
   return(model);
}

