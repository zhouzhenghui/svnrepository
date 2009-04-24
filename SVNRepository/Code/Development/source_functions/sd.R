#Calculate the RMSE of Forecast error
#model: the model used
#data: the current state data

"Forecast.SD" = function(model,data){
   	Points = length(data)-length(model$residuals);
      comp = data[(length(data) - Points +1):length(data)];
      pre = as.numeric(predict(model,Points)$pred);
      sdev = sd(pre - comp);
      return(sdev);	
}