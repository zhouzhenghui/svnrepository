"cutoff"=function(data,method, constant = 40){

	if (method == "constant"){
		data.temp = data[constant:length(data)]
	}

}