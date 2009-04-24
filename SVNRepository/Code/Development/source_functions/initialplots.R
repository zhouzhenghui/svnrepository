
"initial.plots" = function(current.state.data){
   	dev.new()	
	
	current.state.data.diff = diff(current.state.data)
	plot(current.state.data ,main = current.state.name, type = "l")

	dev.new()	
	
	acf(current.state.data ,main = current.state.name, lag.max = 30)

	dev.new()	
	
	plot(current.state.data.diff ,main = paste(current.state.name,"first difference"), type = "l")

	dev.new()	
	
	acf(current.state.data.diff ,main = paste(current.state.name,"first difference", "ACF"), lag.max = 30)
	
	dev.new()	
	par(new=T)
	pacf(current.state.data.diff ,main = paste(current.state.name,"first difference", "PACF"), lag.max = 30)

	print(current.state.name);
	eacf(current.state.data);
	eacf(current.state.data.diff);
}
