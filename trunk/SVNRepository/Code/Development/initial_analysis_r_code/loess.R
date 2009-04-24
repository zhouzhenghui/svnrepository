#local copy wd
library(stats)
library(cobs)
setwd("Z:/SVNRepository/Code/Development/initial_analysis_r_code/")
setwd("C:/Users/tim/Documents/ClassesSpring09/MastersProject/SVNRepository/timr_FinEngProj09/Code/Development/initial_analysis_r_code")

source("../source_functions/get.sd.data.R")
source("../source_functions/get.sreturn.R")
source("../source_functions/format.dates.R")

begin_month=1
begin_year=1997
state = "AK"
end_month = 6
end_year = 2008

data_code="mortgage_originations"
var_data = as.data.frame(get.sd.data(data_code,state,begin_month,begin_year,end_month,end_year))

dates = format.dates(as.matrix(var_data$data.Date))
#dates = as.matrix(var_data$data.Date)

sdata = as.numeric(as.matrix(var_data$interpolated_data))
dates.sreturns = dates
data.sreturns = get.sreturn(sdata)
data.sreturns = c(data.sreturns[1],data.sreturns)

#attach(var_data)
#detach(var_data)
#detach(interpolated_data)
#detach()
time = seq(1:length(sdata))
interpolated_data = as.data.frame(var_data$interpolated_data)
#polynomial regressions
polyfit = loess(interpolated_data ~ time ,var_data, degree = 2, control = loess.control(surface = "direct"))      
names(polyfit)

plot(as.ts(as.matrix(interpolated_data)), col="red", lty = 1)
lines(as.ts(polyfit$fitted), col = "blue", lwd = 2)

convex_concave_fit = conreg(x=time[seq(length(time)-96,length(time),by=1)], y = polyfit$fitted[seq(length(time)-96,length(time),by=1)], convex = FALSE   )
cc_predictions = predict(convex_concave_fit,x=seq(time[length(time)]+1, time[length(time)]+59,by=1))
summary(convex_concave_fit)
plot(convex_concave_fit)

#plot(polyfit$fitted,as.matrix(interpolated_data))

summary(polyfit)
predictions = predict(polyfit, data.frame(time = seq(time[length(time)]+1, time[length(time)] +60, 1)), se = TRUE)$fit

fitted_plus_predicted = c(as.ts(polyfit$fitted),predictions)


plot(as.ts(fitted_plus_predicted), col = "blue", lwd = 2)
lines(as.ts(interpolated_data) , col="red")



polyfit$pars
summary(polyfit)


#convex/concave regression


convex_concave_fit = conreg(x=time, y = interpolated_data, convex = FALSE ,maxit = c(2000, 200)   )
names(convex_concave_fit)
convex_concave_fit$yf
convex_concave_fit$x
convex_concave_fit$conv.loc

plot(as.ts(convex_concave_fit$yf) , col = "blue", lwd = 2)
par(new=T)
plot(as.ts(interpolated_data), col="red")

convex_concave_fit$iKnots

predictions = predict(convex_concave_fit)
plot(predictions)
plot(as.ts(polyfit$fitted), col = "blue", lwd = 2)
lines(interpolated_data, col="red")

#polynomial fit

Y = sdata
length(Y)
X = seq(1,length(Y),by=1)

model <- lm(Y ~ X + I(X^2)+ I(X^3) + I(X^4)+I(X^5) +I(X^6))
summary(model)

int=model$coefficients[1]
coeff1 = model$coefficients[2]
coeff2 = model$coefficients[3]
coeff3 = model$coefficients[4]
coeff4 = model$coefficients[5]
coeff5 = model$coefficients[6]
coeff6 = model$coefficients[7]

plot(Y,type="l")

lines(int+coeff1*X + coeff2*X^2 + coeff3*X^3 +coeff4*X^4 + coeff5*X^5 + coeff6*X^6,type="l")
lines(coeff1*X + coeff2*X^2 + coeff3*X^3 +coeff4*X^4 + coeff5*X^5 + coeff6*X^6,type="l")

X=c(1:200)


