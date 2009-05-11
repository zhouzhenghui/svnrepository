#this script does phase 1 plus phase 2 modeling
#this script is as automatic as possible, 
#and converges on the best robust model that has stationary residuals
#saving the results in both lagranges and states csv model files

#set your intial working directory (intial analysis folder)
#setwd("Z:/SVNRepository/SVNRepository/Code/Development/initial_analysis_r_code/")
#setwd("C:/Users/tim/Documents/ClassesSpring09/MastersProject/svnrepository/SVNRepository/Code/Development/initial_analysis_r_code/")

#install/source all the packages used in this project (this will take a minute)
source("../source_functions/source.all.R")
source.all()
install.all.packages(install=F)
detach(package:fSeries)

#set your initial state parameters and variables lists
begin_month=1
state = "MT" #our dependent variable 
begin_year=get_start_year(state)
end_month = 6
end_year = 2008

#Step 1
#eda--determine some rough lag ranges
data = grab.data(state,begin_month,begin_year,end_month,end_year,sreturn=T)
test.data.vector = data$test.data.vector
test.data.vector = as.data.frame(test.data.vector)
dates = data$dates
#dates2 = dategen(begin_month,begin_year, end_month, end_year+5)  #begin month + 1 because we had to chop off the first month
independent.variables=data$independent.variables
test.and.plot = dataframe.eda(test.data.vector, state, dates, basicTS = F, comparisonTS = T, allTS = F, CCF = F, scatter = F, ACF = F, PACF = F)
shifted.multivar.test.data.vector = shift.df.multi(test.data.vector,state,independent.variables)
test.and.plot = dataframe.eda(shifted.multivar.test.data.vector, state, dates, basicTS = F, comparisonTS = T, allTS = F, CCF = F, scatter = F, ACF = F, PACF = F)


#Step 2
#bic-stepwise regression - refine to get initial lag ranges
do_bic_lars_regressions(state,test.data.vector,independent.variables)


#Step 3
#Phase 1-robust regression - set automatic or sequential
robust.lm=do_robust_regression(state,test.data.vector,dates,independent.variables,automatic=T,plot=T)
robust.lm
resids = robust.lm$resids
acf(resids, lag.max=40)
phase1fits=robust.lm$robust.lm$fit

#Step 4
#Phase 2-vol modeling 

vol.model = do_vol_modeling(state,test.data.vector,phase1fits,resids,dates,automatic=F,plot=T)
vol.model
resids2= vol.model$resids
acf(resids2,lag.max=40)

#Step 5
#Curve fitting of explanatory variables
begin_year=1998
data = grab.data(state,begin_month,begin_year,end_month,end_year,sreturn=F)
test.data.vector2 = data$test.data.vector
test.data.vector2 = as.data.frame(test.data.vector2)
dates = data$dates
new.curves=curvefit(test.data.vector2,state,plot=T)
shifted.multivar.test.data.vector2 = shift.df.multi(new.curves,state,independent.variables)

plot(as.ts(get.tim.data(shifted.multivar.test.data.vector2)))
#new.curves = smoother(new.curves)

#Step 6
#Get forecasts, check forecasts, store, and plot
driftmodel = robust.lm$robust.lm
volmodel = vol.model$model
begin_year=1998
data = grab.data(state,begin_month,begin_year,end_month,end_year,sreturn=T)
test.data.vector = data$test.data.vector
test.data.vector = as.data.frame(test.data.vector)
dates = data$dates
projections = forecaster(driftmodel,resids,volmodel,independent.variables,test.data.vector,new.curves,plot=T,plotindex=T, plot.ses =T)
projections$projectedindex 
projections$projectedreturns
