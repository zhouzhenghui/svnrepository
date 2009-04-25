#this script does phase 1 plus phase 2 modeling
#this script is as automatic as possible, 
#and converges on the best robust model that has stationary residuals
#saving the results in both lagranges and states csv model files

#set your intial working directory (intial analysis folder)
#setwd("Z:/SVNRepository2/SVNRepository/Code/Development/initial_analysis_r_code/")
#setwd("C:/Users/tim/Documents/ClassesSpring09/MastersProject/svnrepository/SVNRepository/Code/Development/initial_analysis_r_code/")

#install/source all the packages used in this project (this will take a minute)
source("../source_functions/source.all.R")
source.all()
install.all.packages()


#set your initial state parameters and variables lists
begin_month=1
begin_year=1998
state = "NV" #our dependent variable 
end_month = 6
end_year = 2008

#Step 1
#eda--determine some rough lag ranges
data = grab.data(state,begin_month,begin_year,end_month,end_year,sreturn=T)
test.data.vector = data$test.data.vector
dates = data$dates
independent.variables=data$independent.variables
test.and.plot = dataframe.eda(test.data.vector, state, dates, basicTS = F, comparisonTS = T, allTS = F, CCF = F, scatter = F, ACF = F, PACF = F)
test.and.plot$adf.test

#Step 2
#bic-stepwise regression - refine to get initial lag ranges
do_bic_lars_regressions(state,test.data.vector,independent.variables)


#Step 3
#Phase 1-robust regression - set automatic or sequential
robust.lm=do_robust_regression(state,test.data.vector,dates,independent.variables,automatic=T,plot=T)
robust.lm
resids = robust.lm$resids
phase1fits=robust.lm$robust.lm$fit

#Step 4
#Phase 2-vol modeling 
vol.model = do_vol_modeling(state,test.data.vector,phase1fits,resids,dates,automatic=T,plot=T)
vol.model

#Step 5
#Curve fitting of explanatory variables
new.curves=curvefit(test.data.vector,plot=T)

#Step 6
#Get forecasts, check forecasts, store, and plot