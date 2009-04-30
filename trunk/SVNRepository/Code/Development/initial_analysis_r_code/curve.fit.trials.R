attach(test.data.vector)
data=foreclosures
plot(as.ts(data))
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
max.cumulative=49605
cumsum(data)
(length(foreclosures)-6)/12
cumulative = cumsum(data[c((length(data)-6):length(data))])
lines(as.ts(data),col="blue")
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


plot(X,Y)
model <- lm(Y ~ X + I(X^2) + I(X^2) + I(X^3))
model$fit
prediction = c(1:(length(cumulative)+60))-1
int = model$coefficients[1]
coeff1 = ifelse(is.na(model$coefficients[2]),0,model$coefficients[2])
coeff2 = ifelse(is.na(model$coefficients[3]),0,model$coefficients[3])
coeff3 = ifelse(is.na(model$coefficients[4]),0,model$coefficients[4])
coeff4 = ifelse(is.na(model$coefficients[5]),0,model$coefficients[5])
coeff5 = ifelse(is.na(model$coefficients[6]),0,model$coefficients[6])
coeff6 = ifelse(is.na(model$coefficients[7]),0,model$coefficients[7])
convexity=rep(1,length(X))
newfit=concon(x=X,y=Y,v=convexity,k=X)
lines(predict(newfit,newx))
newdata1=predict(newfit,newx)

newx=prediction

lines(newx,int+coeff1*newx + coeff2*newx^2 + coeff3*newx^3 +coeff4*newx^4 + coeff5*newx^5 + coeff6*newx^6,type="l")
newdata2=int+coeff1*newx + coeff2*newx^2 + coeff3*newx^3 +coeff4*newx^4 + coeff5*newx^5 + coeff6*newx^6

data[c((length(data)-6):length(data))]
c(data[(length(data)-6)],diff(cumulative))
predict(model,newdata=prediction )

conversion = c(data[(length(data)-6)],diff(newdata1))
newconverteddata = c(data[c(1:(length(data)-6))] ,conversion)

plot(as.ts(conversion))
plot(as.ts(newconverteddata))

plot(as.ts(data))
piece2