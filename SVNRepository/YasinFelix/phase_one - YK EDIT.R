#set wd
#setwd("Z:/SVNRepository/Code/Development/initial_analysis_r_code/")
#setwd("C:/Users/tim/Documents/ClassesSpring09/MastersProject/SVNRepository/timr_FinEngProj09/Code/Development/initial_analysis_r_code")

#some libraries
library(leaps)

#source initial files
source("../source_functions/get.all.data.R")
source("../source_functions/get.sd.data.R")
source("../source_functions/get.sreturn.R")
source("../source_functions/multi.adf.test.R")
source("../source_functions/multi.adf.test.lags.R")
source("../source_functions/format.dates.R")
source("../source_functions/chop.R")
source("../source_functions/shift.R")
source("../source_functions/get.lagvectors.R")
source("../source_functions/dataframe.eda.R")
source("../source_functions/dategen.R")
source("../source_functions/get.res.formula.R")
source("../source_functions/shift.df.R")
source("../source_functions/shift.df.multi.R")
source("../source_functions/smoother.R")

begin_month=1
begin_year=1998
state = "ID" #our dependent variable 
end_month = 6
end_year = 2008



supply_demand_data_code_vector=c(
"median_income",
#"thirty_year_current_coupon",
"ty_cr",
"mort_orig",
"unemp_rate",
#"primary_interest_rate",
"pop_size",
#"population_growth",
#"median_house_median_income",
#"delta_from_median_house_median_income"
"foreclosures",
#"housing_stock",
#"inventory_turnover",
"building_permits"
)


#basic tests
test.data.frame = get.all.data(supply_demand_data_code_vector,state,begin_month,begin_year,end_month,end_year, smooth=TRUE) 

test.data.vector = test.data.frame$combined # NON - SR
test.data.vector = test.data.frame$combined_spec # this has the mean-centered rates (non-simple return), and simple returns

dates = dategen(begin_month+1,begin_year, end_month, end_year)  #begin month + 1 because we had to chop off the first month

test.and.plot = dataframe.eda(test.data.vector, state, dates, basicTS = F, comparisonTS = T, allTS = F, CCF = F, scatter = F, ACF = F, PACF = F)
test.and.plot$adf.test


#get 1 shifted independent variables
#independent.variables = c("population_size")
#get multiple shifted independent variables
independent.variables = supply_demand_data_code_vector
#do the time shiftin here, lags depend on ranges entered in lagranges.csv
shifted.multivar.test.data.vector = shift.df.multi(test.data.vector,state,independent.variables)

dataframe.eda(shifted.multivar.test.data.vector,state,dates,basicTS = F, comparisonTS = T, allTS = F, CCF = F, scatter = F, ACF = F, PACF = F) 
#X out for a cheap time shifting animation, just do it you'll see

## --try a stepwise regression on 1 variable to determine the most significant lag
form = as.formula(paste(state," ~ .",sep=""))
bs.reg = regsubsets(x= form, nbest=1, data = shifted.multivar.test.data.vector, nvmax=30,intercept=FALSE, method=c("exhaustive"), really.big=FALSE)
summary(bs.reg)
names(bs.reg)
plot(x=bs.reg, labels=bs.reg$xnames, scale=c("bic"), col="blue", main = paste("Stepwise BIC Regression of",state))
dev.new()
plot(x=bs.reg, labels=bs.reg$xnames, scale=c("adjr2"), col="blue", main = paste("Stepwise R^2 Regression of",state))

## --PHASE 1
n = ncol(shifted.multivar.test.data.vector)
y = rlm(shifted.multivar.test.data.vector[,1:(n-1)],shifted.multivar.test.data.vector[,n], maxit = 20)
summary(y)

acf(y$residuals, lag = 40, main = "ACF of Residuals")
acf(y$residuals^2, lag = 40, main = "ACF of Sq-Residuals")

Box.test(y$residuals, lag = 12, type = c("Ljung-Box"))

pacf(diff(y$residuals), main = "PACF of Differenced Residuals")
adf.test(y$residuals, k = 3)
adf.test(y$residuals, k = 2, alternative = "e")

library(urca)
ur.df(y$residuals, lags = 3, type = "none")@cval
ur.df(y$residuals, lags = 3, type = "none")@teststat

multi.adf.test(y$residuals)
multi.adf.test.lags(y$residuals)

plot(y$fitted.values, type="l", ylim=c(-0.010,0.035), ylab="Simple Return", xlab="Time", main="Plot of Fitted vs. Actual HPI")
lines(shifted.multivar.test.data.vector[,n],col="red")
legend("bottomright",c("Fitted HPI", "Actual HPI"), col=c("black","red"), lty=c(1,1))

## --PHASE 2

resid.data = y$residuals

## --Plot EACF
eacf(resid.data)

## --Build ARIMA Model
library(forecast)
auto.arima(resid.data, stationary = TRUE)

arima.fit = arima(resid.data,order = c(1,0,1), include.mean = TRUE)
arima.fit

## --LjungBox test on Model Resids and Sq. Resids 
params = 2
Box.Ljung.test(arima.fit$resid,lag = 12, adj.DF =  12-params)
Box.Ljung.test(arima.fit$resid^2,lag = 12, adj.DF =  12)

## --BUILD GARCH ON RESIDUALS
library(fGarch)
?garchFit

resid.garch = garchFit(formula = ~arma(1,0)+garch(1,1), data=resid.data, include.mean=FALSE)
summary(resid.garch)

## --CHECK OF GARCH ADEQUACY--
meanParams = 1
volParams =2

?garchFit

sresi=resid.garch@residuals/resid.garch@sigma.t
acf(sresi, main = "ACF of Standardized Residuals")
acf(sresi^2, main = "ACF of Sq-Standardized Residuals")
Box.Ljung.test(sresi, lag = 12, adj.DF =  12-meanParams)  
Box.Ljung.test(sresi^2, lag = 12, adj.DF = 12-volParams ) 

pacf(diff(sresi), lag.max=40)
adf.test(sresi, k = 3)

## --LM MODEL
lm.reg = lm(formula = form, data = shifted.multivar.test.data.vector)

acf(lm.reg$residuals, lag = 40)

Box.test(lm.reg$residuals, lag = 12, type = c("Ljung-Box"))
adf.test(lm.reg$residuals, k = 7)
?adf.test
pacf(diff(lm.reg$residuals))

summary(lm.reg)
names(summary(lm.reg))

plot(lm.reg$fitted.values, type="l")
n = ncol(shifted.multivar.test.data.vector)
lines(shifted.multivar.test.data.vector[,n],col="red")