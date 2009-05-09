"ty.cr.curve"=function(state,data,lag){

expanded.data = NULL
data.temp = NULL
newdata = c(5.46, 5.87, 5.69, 6.51 ,7.12,7.12,7.12 ,7.12 ,7.12 ,7.12)
for (i in 1:(length(newdata)-1)){

	for(j in 1:12){
		if (j==1){
			expanded.data=c(expanded.data,newdata[i])	

		}
		else{			
			expanded.data=c(expanded.data,0)
		}
	}
}

for(i in 1:(length(newdata)-1)){
	interpolated = spline(newdata[i:(i+1)],n=13)
	data.temp=c(data.temp,newdata[i],interpolated$y[2:12])
}

structure(list(newdata=c(data,data.temp[7:(lag+66)]),data = data,total=sum(newdata)))

}
