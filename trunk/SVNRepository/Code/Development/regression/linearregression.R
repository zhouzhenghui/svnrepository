data<-read.table("Z:/CH01PR20.txt")
y<-data[,1]
x<-data[,2]

#Calculate Sxx from a sample variance, var(x)
n<-length(x)
Sxx=var(x)*(n-1)

## Linear Regression
LR<-lm(y~x)

## Beta, R^2
coef(LR)[2]
summary(LR)$adj.r.squared

## ANOVA, CI, PI
anova(LR)
predict(LR,interval="confidence",level=.95)
predict(LR,interval="prediction",level=.95)

## Plot Data and Fitted Line
plot(data)
fitLine = line(predict(LR))
abline(fitLine$coefficients[1],fitLine$coefficients[2])

## Add Confidence Interval
alpha = 0.95
CI = predict(LR,interval="confidence",level=alpha)
abline(line(CI[,2])$coefficients[1], line(CI[,2])$coefficients[2], lty = 2)
abline(line(CI[,3])$coefficients[1], line(CI[,3])$coefficients[2], lty = 2)

## Plot Residuals
plot(x,resid(LR))
abline(0,0)

## Multiple plots in the same window
par(mfrow=c(2,1))