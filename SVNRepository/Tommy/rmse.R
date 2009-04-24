
"Forecast.RMSE" = function(current.state.name,n){
      setwd("Z:/SVNRepository/Code/Development/source_functions")

      source("ATS_2008.R")
      source("autoModel.R")
      all.data = read.table("../../../Data/States/all.txt");
      
      current.state.data = subset(all.data,all.data[,1]==current.state.name)[,4];
      current.state.data.sreturn= (current.state.data[2:length(current.state.data)]-current.state.data[1:(length(current.state.data)-1)])/current.state.data[1:(length(current.state.data)-1)];

      
      data = current.state.data.sreturn;
      pre = array(NA,(length(data)-n);       
      
         for(i in 1:((length(data)-n)){
            model = auto.model(current.state.name,n+i-1)
            if ((class(model)) == "fGARCH"){
               pre[i] = predict(model,1)$meanForecast;
            }else{
               pre[i] = predict(model,1)$pred;
            }
         }
      
      comp = data[(n+1):length(data)];
      rmse = (sum((pre - comp)^2)/Points)^(0.5);
      return(rmse);	
}
