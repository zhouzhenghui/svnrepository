#set wd
#setwd("Z:/SVNRepository2/SVNRepository/Code/Development/initial_analysis_r_code/")
#setwd("C:/Users/tim/Documents/ClassesSpring09/MastersProject/svnrepository/SVNRepository/Code/Development/initial_analysis_r_code/")

#some libraries needed for regression and such
library(leaps)
library(lars)
library(outliers)
library(urca)

#source initial files
source("../ATS_2008.R")
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
source("../source_functions/plot.actual.fitted.R")


begin_month=1
begin_year=1999
state = "FL" #our dependent variable 
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
test.data.frame = get.all.data(supply_demand_data_code_vector,state,begin_month,begin_year,end_month,end_year,smooth=TRUE) 
test.data.vector = test.data.frame$combined_spec # this has the mean-centered rates (non-simple return), and simple returns
dates = dategen(begin_month+1,begin_year, end_month, end_year)  #begin month + 1 because we had to chop off the first month
test.and.plot = dataframe.eda(test.data.vector, state, dates, basicTS = F, comparisonTS = T, allTS = F, CCF = F, scatter = F, ACF = F, PACF = F)
test.and.plot$adf.test

as.data.frame(sapply(test.data.vector,"get.sreturn"))

#get 1 shifted independent variables
#independent.variables = c("building_permits")
#independent.variables = supply_demand_data_code_vector
#get multiple shifted independent variables
independent.variables.vector = supply_demand_data_code_vector
for (independent.variables in independent.variables.vector){
#do the time shiftin here, lags depend on ranges entered in lagranges.csv
shifted.multivar.test.data.vector = shift.df.multi(test.data.vector,state,independent.variables)
#dataframe.eda(shifted.multivar.test.data.vector,state,dates, basicTS = F, comparisonTS = F, allTS = F, CCF = F, scatter = T, ACF = F, PACF = F)


#try a best subset BIC/R^2 regression to determine the most significant lag(s)
form = as.formula(paste(state," ~ .",sep=""))
bs.reg = regsubsets(x= form, nbest=1, data = shifted.multivar.test.data.vector, nvmax=30,intercept=FALSE, method=c("exhaustive"), really.big=FALSE)
dev.new()
plot(x=bs.reg, labels=bs.reg$xnames, scale=c("bic"), col="blue", main = paste("Stepwise BIC Regression of",state))
#dev.new()
#plot(x=bs.reg, labels=bs.reg$xnames, scale=c("adjr2"), col="blue", main = paste("Stepwise R^2 Regression of",state))

#try a least angle regression (LARS) to determine the most significant lag(s)
predictors = shifted.multivar.test.data.vector
predictors=as.matrix(predictors[,which(names(predictors)!=state)])
response = as.matrix(subset(shifted.multivar.test.data.vector, select = state))

lars.reg = lars(x=predictors, y=response, type = c("lar"))
dev.new()
plot(lars.reg)
sink("lars.txt", append = T)
print(lars.reg)
sink()
}


#do a robust regression
shifted.multivar.test.data.vector = shift.df.multi(test.data.vector,state,independent.variables)
#dataframe.eda(shifted.multivar.test.data.vector, state, dates, basicTS = F, comparisonTS = T, allTS = F, CCF = F, scatter = T, ACF = F, PACF = F)
#plot.actual.fitted(shifted.multivar.test.data.vector,state,dates,sreturn=TRUE)

predictors = shifted.multivar.test.data.vector
predictors=as.matrix(predictors[,which(names(predictors)!=state)])
response = as.matrix(subset(shifted.multivar.test.data.vector, select = state))


robust.lm = rlm(x=predictors ,y=response)
summary(robust.lm)

resids=as.ts(robust.lm$residuals)


#residual analysis
#dataframe.eda(resids, state = "", dates, basicTS = F, comparisonTS = F, allTS = F, CCF = F, scatter = F, ACF = T, PACF = T)
multi.adf.test(resids)
multi.adf.test.lags(resids)

#model check
which.min(abs(summary(robust.lm)$coefficients[,3]))
#plot(resids)
#acf(resids^2, lag.max = 40, main = "ACF of Phase 1 Sq-Residuals")
pacf(diff(resids))
#Box.Ljung.test(resids,lag = 12, adj.DF =  12)

lag=2
ur.df(resids,lags=lag,type="none")@cval
ur.df(resids,lags=lag,type="none")@teststat
#adf.test(resids,k=1)

#end robust regression

#do a phase 2 time series fit before this...see phase_two.R


#make frame of fitted vs actual
dates = dategen(begin_month,begin_year, end_month, end_year)  #begin month + 1 because we had to chop off the first month

fit1 = robust.lm$fitted
fit2 = robust.lm$residuals - arima.fit$residuals
residuals1 = robust.lm$residuals
residuals2 = arima.fit$residuals
actual = response
regression.dataframe = as.data.frame(cbind(actual, fit1, fit1+fit2,residuals1,residuals2))
names(regression.dataframe) = c(state,"Phase 1 Fitted","Phase 1+2 Fitted","Residuals Phase 1","Residuals Phase 1+2")

#regression.dataframe = as.data.frame(cbind(actual, fit1+fit2,residuals2))
#names(regression.dataframe) = c(state,"Phase 1+2 Fit","Res1+2")
plot.actual.fitted(regression.dataframe,state,dates,sreturn=TRUE)


#end make a dataframe

#back-transform the data to get the original index + fitted
test.data.vector.orig = test.data.frame$combined
shifted.multivar.test.data.vector.orig = shift.df.multi(test.data.vector.orig,state,independent.variables)

start.value = shifted.multivar.test.data.vector.orig[1,which(names(shifted.multivar.test.data.vector.orig)==state)]
orig.data.actual = get.orig.data(start.value,response)
orig.data.fit = get.orig.data(start.value,fit1+fit2)

regression.dataframe.orig = as.data.frame(cbind(orig.data.actual,orig.data.fit))
names(regression.dataframe.orig)  = c(state,paste(state,"Fitted"))
plot.actual.fitted(regression.dataframe.orig,state,dates,sreturn=FALSE)



#phase_two(shifted.multivar.test.data.vector)

