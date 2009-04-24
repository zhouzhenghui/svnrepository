"interpolation" = function(data, type){

library(base)
library(pspline)
if(type=="linear"){

data.temp = 
}

if(type=="linear"){
	data.temp = 
}

approx   (x, y = NULL, xout, method="linear", n=50,yleft, yright, rule = 1, f=0, ties = mean)

}
smoothspline = sm.spline(x=current.state.data,y=current.state.data, norder=2)
smoothspline$x 


expanded.data = NULL
expanded.data2 = NULL

for (i in 1:(length(current.state.data)-1)){

	for(j in 1:3){
		if (j==1){
			expanded.data=c(expanded.data,current.state.data[i])
			expanded.data2=c(expanded.data2,current.state.data[i])

		}
		else{
			expanded.data2=c(expanded.data2,mean(c(current.state.data[i],current.state.data[i+1])))
			expanded.data=c(expanded.data,0)

		}
	}
}


smoothspline = sm.spline(x=expanded.data, norder=2)
smoothspline$y 
smoothspline2 = smooth.spline(x=expanded.data, all.knots = FALSE, nknots=length(expanded.data),spar=0)
smoothspline2$y

splinedata = spline(expanded.data,n=length(expanded.data),xout = 2)
splinedata$y

splinedata = spline(current.state.data,method="natural")
newsplinedata = splinedata$y[1:(length(splinedata$y)-3)]
newsplinedata[which(expanded.data!=0)]=expanded.data[which(expanded.data!=0)]

splinedata2 = spline(newsplinedata,xout = xout.vec)
splinedata2$y


seq1 = seq(1,length(current.state.data)*3)
seq2 = seq(1,length(current.state.data))
xout.vec = which(expanded.data==0)

data=current.state.data

