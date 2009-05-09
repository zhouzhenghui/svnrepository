"unemployment.rate.curve"=function(state,data){

peak = max(data)
peak.location = min(which.max(data))
trough = min(data)
trough.location = max(which.min(data))
curve.fit.start = floor(peak.location*0.9)
curve.fit.end = length(data)
semi.period = abs(trough.location-peak.location)

X=c(curve.fit.start,peak.location,trough.location,curve.fit.end ,next.peak-30,last.location)
Y=c(data[curve.fit.start],peak,trough,data[curve.fit.end],peak,copy.value)

X=c(curve.fit.end ,next.peak-30,last.location)
Y=c(data[curve.fit.end],peak,copy.value)



#X=c(curve.fit.start:curve.fit.end)-1
#Y=data[X+1]
#next peak
#next.peak = semi.period + trough.location
#X=c(X,next.peak)
#Y=c(Y,peak)
##last value
#last.location = length(data) + 60
#last.location.copy = peak.location + (next.peak-last.location)
#copy.value = data[last.location.copy]
#X=c(X,last.location)
#Y=c(Y,copy.value)
#plot(X,Y)
#model <- lm(Y ~ X + I(X^2) + I(X^2) + I(X^3) + I(X^4))
#newx=c(curve.fit.end:last.location)-1
newx=c(0:last.location)-1

int = model$coefficients[1]
coeff1 = ifelse(is.na(model$coefficients[2]),0,model$coefficients[2])
coeff2 = ifelse(is.na(model$coefficients[3]),0,model$coefficients[3])
coeff3 = ifelse(is.na(model$coefficients[4]),0,model$coefficients[4])
coeff4 = ifelse(is.na(model$coefficients[5]),0,model$coefficients[5])
coeff5 = ifelse(is.na(model$coefficients[6]),0,model$coefficients[6])
coeff6 = ifelse(is.na(model$coefficients[7]),0,model$coefficients[7])
newdata2=int+coeff1*newx + coeff2*newx^2 + coeff3*newx^3 +coeff4*newx^4 + coeff5*newx^5 + coeff6*newx^6



#newdata=c(data,newdata2)
#lines(as.ts(newdata),col="blue")
#lines(as.ts(data),col="black")
#lines(as.ts(newdata2),col="red")
#convexity=1
#convexity = c(convexity,-1)
#convexity=c(convexity,-1)
#convexity = c(convexity,1)#next peak
#convexity = c(convexity,1)#last value

#length(X)
#length(convexity)
#length(newx)
#newfit=concon(x=X,y=Y,v=convexity ,k=X)
#lines(c(data,predict(newfit,X)),col="blue")
prediction=newdata2[c(curve.fit.end:last.location)]
height.adjust = prediction[1]-data[length(data)]
prediction = prediction - height.adjust
newdata=c(data,prediction)
#plot(as.ts(newdata))
#lines(as.ts(data),col="blue")

structure(list(newdata=newdata,data = data,total=sum(newdata)))

}
