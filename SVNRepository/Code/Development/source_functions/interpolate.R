##interpolates to create
"interpolate"=function(data,method){

expanded.data = NULL
data.temp = NULL

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

		data.temp = spline(data, method = "natural")$y[1:(length(data)*3)]
		#replace original values
		data.temp[which(expanded.data!=0)]=expanded.data[which(expanded.data!=0)]

	}

	if (method=="piecewise_spline"){


		for(i in 1:(length(data)-1)){
			interpolated = spline(data[i:(i+1)],n=4)
			data.temp=c(data.temp,data[i],interpolated$y[2:3])
		}
	}

	if (method=="linear"){
		#data.temp= approx(data, method="linear", n=length(data)*3)$y

		for(i in 1:(length(data)-1)){
			
			interpolated = approx(data[i:(i+1)],n=4)
			data.temp=c(data.temp,data[i],interpolated$y[2:3])
		}

		
	}


	return(data.temp);
}