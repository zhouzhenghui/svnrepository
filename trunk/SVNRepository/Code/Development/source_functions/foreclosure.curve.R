#this assumes we're ending at q122008 and projecting forward 60 months (q122013
"foreclosure.curve"=function(state,data){
#the following number taken as "best case" cumulative foreclosures in 2008/2009
#assume recovery (less foreclosures each month) by Q3'09
cumulatives = read.csv("../../../Data/Supply Demand/Data/foreclosurecurves.csv")
rownum=which(cumulatives$State==state)

cumulative.2008Q12 = cumsum(data[c((length(data)-6):length(data))])
cumulative.2008Q34 = cumulatives$X2008Q34.1[rownum] + cumulative.2008Q12[length(cumulative.2008Q12)]
cumulative.2009 = cumulatives$X2009[rownum] + cumulative.2008Q34
cumulative.2010 = cumulatives$X2010[rownum] + cumulative.2009
cumulative.2011 = cumulatives$X2011[rownum] + cumulative.2010
cumulative.2012 = cumulatives$X2012[rownum] + cumulative.2011
origin=length(cumulative.2008Q12)


X=c(1:origin)-1
Y=cumulative.2008Q12[X+1]
X=c(X,origin+6)
Y=c(Y,cumulative.2008Q34)
X=c(X,origin+18)
Y=c(Y,cumulative.2009)
X=c(X,origin+30)
Y=c(Y,cumulative.2010)
X=c(X,origin+42)
Y=c(Y,cumulative.2011)
X=c(X,origin+56)
Y=c(Y,cumulative.2012)

newx = c(1:(origin+60))-1
#plot(X,Y)
model <- lm(Y ~ X + I(X^2) + I(X^2) + I(X^3))


int = model$coefficients[1]
coeff1 = ifelse(is.na(model$coefficients[2]),0,model$coefficients[2])
coeff2 = ifelse(is.na(model$coefficients[3]),0,model$coefficients[3])
coeff3 = ifelse(is.na(model$coefficients[4]),0,model$coefficients[4])
coeff4 = ifelse(is.na(model$coefficients[5]),0,model$coefficients[5])
coeff5 = ifelse(is.na(model$coefficients[6]),0,model$coefficients[6])
coeff6 = ifelse(is.na(model$coefficients[7]),0,model$coefficients[7])
newdata2=int+coeff1*newx + coeff2*newx^2 + coeff3*newx^3 +coeff4*newx^4 + coeff5*newx^5 + coeff6*newx^6



#convexity=rep(1,length(X))
#newfit=concon(x=X,y=Y,v=convexity,k=X)
#lines(predict(newfit,newx))
#lines(newdata2)
#newdata1=predict(newfit,newx)
#lines(as.ts(newdata1))
#heightadjust=data[(length(data))]-diff(newdata2)[7]
heightadjust=0
conversion = c(diff(newdata2)[-c(1:6)]+heightadjust)
newconverteddata = c(data ,conversion)
#plot(as.ts(conversion))
#plot(as.ts(newconverteddata))
newconverteddata[which(newconverteddata<0)]=0
newdata=newconverteddata
#plot(as.ts(newdata),col="blue")
#lines(as.ts(data))
structure(list(newdata=newdata,data = data,total=sum(newdata)))

}
