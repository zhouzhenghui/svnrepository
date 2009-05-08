attach(test.data.vector)
data=mort_orig
plot(as.ts(data))
plot(as.ts(newdata))
case="foreclosures"
if(case=="mort_orig"){
	#replicate 24 months of last value
	lastval=data[length(data)]
	piece1 = rep(lastval,24)
	#for the next 36 months, fit a curve to the first part of the data
	#then adjust the intercept  to patch where piece 1 leaves-off
	X=c(1:36)-1
	Y=data[X+1]
	model <- lm(Y ~ X + I(X^2) + I(X^2))
	int=as.numeric(model$coeff[1])
	piece2=model$fit-int
	#concatenate
	projection = c(piece1,piece2)
	newdata = c(data,projection)
}

if(case=="foreclosures"){
#the following number taken as "best case" cumulative foreclosures in 2008/2009
#assume recovery (less foreclosures each month) by Q3'09
max.cumulative=10000

cumsum(data)
(length(foreclosures)-6)/12
cumulative = cumsum(data[c((length(data)-6):length(data))])
lines(as.ts(data),col="blue")
plot(as.ts(data))
plot(as.ts(cumulative))
plot(as.ts(cumsum(data)))

X=c(1:length(cumulative))-1
Y=cumulative[X+1]
X=c(X,length(cumulative)+24)
Y=c(Y,max.cumulative)
#optional
X=c(X,length(cumulative)+48)
Y=c(Y,max.cumulative+diff(cumulative[c(4:5)]))
#tapering-off
X=c(X,length(cumulative)+60)
Y=c(Y,max.cumulative+3*diff(cumulative[c(5:6)]))

prediction = c(1:(length(cumulative)+60))-1
plot(X,Y)

convexity=rep(1,length(X))
newfit=concon(x=X,y=Y,v=convexity,k=X)
lines(predict(newfit,newx))
newdata1=predict(newfit,newx)
lines(as.ts(newdata1))
newx=prediction

data[c((length(data)-6):length(data))]
c(data[(length(data)-6)],diff(cumulative))
newdata2=predict(model,newdata=prediction)

heightadjust=data[(length(data))]-diff(newdata1)[7]
conversion = c(diff(newdata1)[-c(1:6)]+heightadjust)
newconverteddata = c(data ,conversion)

plot(as.ts(conversion))
plot(as.ts(newconverteddata))
newconverteddata[which(newconverteddata<0)]=0
newdata=newconverteddata
length(data)
length(newconverteddata)

}


if(case=="unemp_rate"){


X=c(40:length(data))-1
Y=data[X+1]
X=c(X,length(data)+60-13)
Y=c(Y,data[50])
#optional
X=c(X,length(data)+60-13+25)
Y=c(Y,data[25])
plot(X,Y)
model <- lm(Y ~ X + I(X^2) + I(X^2) + I(X^3) + I(X^4)+ I(X^5))
newx=c(114:(length(data)+50-13+25))-1

int = model$coefficients[1]
coeff1 = ifelse(is.na(model$coefficients[2]),0,model$coefficients[2])
coeff2 = ifelse(is.na(model$coefficients[3]),0,model$coefficients[3])
coeff3 = ifelse(is.na(model$coefficients[4]),0,model$coefficients[4])
coeff4 = ifelse(is.na(model$coefficients[5]),0,model$coefficients[5])
coeff5 = ifelse(is.na(model$coefficients[6]),0,model$coefficients[6])
coeff6 = ifelse(is.na(model$coefficients[7]),0,model$coefficients[7])
newdata2=int+coeff1*newx + coeff2*newx^2 + coeff3*newx^3 +coeff4*newx^4 + coeff5*newx^5 + coeff6*newx^6
newdata=c(data[c(1:113)],newdata2)
plot(as.ts(newdata),col="blue")
lines(as.ts(data),col="black")

}


if(case=="median_income"){
	plot(as.ts(data))
	replicant=data[c((length(data)-60):length(data))]
	newdata=c(data,replicant)
}

if(case=="pop_size"){
	X=c(1:length(data))
	Y=data
	
	model <- lm(Y ~ X)

	newx=c((length(data)+1):(length(data)+60))-1

	int = model$coefficients[1]
	coeff1 = ifelse(is.na(model$coefficients[2]),0,model$coefficients[2])
	coeff2 = ifelse(is.na(model$coefficients[3]),0,model$coefficients[3])
	coeff3 = ifelse(is.na(model$coefficients[4]),0,model$coefficients[4])
	coeff4 = ifelse(is.na(model$coefficients[5]),0,model$coefficients[5])
	coeff5 = ifelse(is.na(model$coefficients[6]),0,model$coefficients[6])
	coeff6 = ifelse(is.na(model$coefficients[7]),0,model$coefficients[7])
	newdata2=int+coeff1*newx + coeff2*newx^2 + coeff3*newx^3 +coeff4*newx^4 + coeff5*newx^5 + coeff6*newx^6
	newdata=c(data,newdata2)

	

}

if(case=="building_permits"){
	plot(as.ts(data))
	replicant=data[c(1:60)]
	newdata=c(data,replicant)
}

if(case=="ty_cr"){
	garch.model.tycr = garchFit(formula= ~arma(1,0) + garch(1,1),data)
      newdata = c(data,predict(garch.model.tycr,60)$meanForecast)
)

}

