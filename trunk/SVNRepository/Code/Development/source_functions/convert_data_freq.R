## Creates monthly data
## 'frequency' parameter = "annual" or "quarter"
## 'method' parameter = "total_spline" or "linear"
## NOTE: for annual data, only "linear" method works

"convert_data_freq"=function(data,frequency,method){

expanded.data = NULL
data.temp = NULL

if(frequency=="annual"){
	x = 12


for (i in 1:(length(data)-1)){

	for(j in 1:x){
		if (j==1){
			expanded.data=c(expanded.data,data[i])	

		}
		else{			
			expanded.data=c(expanded.data,0)
		}
	}
}


	#data.temp= approx(data, method="linear", n=length(data)*3)$y

	for(i in 1:(length(data)-1)){
			
		interpolated = approx(data[i:(i+1)],n=(x+1))
		data.temp=c(data.temp,data[i],interpolated$y[2:x])
	}

	data.temp=c(data.temp,data[length(data)])
	
	return(data.temp);
}

if(frequency=="quarter"){

for (i in 1:(length(data)-1)){

	for(j in 1:3){
		if (j==1){
			expanded.data=c(expanded.data,data[i])	

		}
		else{			
			expanded.data=c(expanded.data,0)
		}
	}
}


	if (method=="total_spline"){

		data.temp = spline(data, method = "natural")$y[1:((length(data)-1)*3)]
		#replace original values
		data.temp[which(expanded.data!=0)]=expanded.data[which(expanded.data!=0)]
		data.temp=c(data.temp,data[length(data)])
	}


	if (method=="linear"){
		#data.temp= approx(data, method="linear", n=length(data)*3)$y

		for(i in 1:(length(data)-1)){
			
			interpolated = approx(data[i:(i+1)],n=4)
			data.temp=c(data.temp,data[i],interpolated$y[2:3])
		}
		data.temp=c(data.temp,data[length(data)])
		
	}


	return(data.temp);
}
}
