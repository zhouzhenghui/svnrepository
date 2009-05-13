states.to.use = c("CA", "OH", "SD", "FL", "TX", "KS", "VT", "ID", "MI", "AZ","MS", "KY","AL", "WV","ND","NJ", "GA","MA","NY","VT")
states.to.use =sort(states.to.use)

for(i in 1:length(states.to.use)){
#set your initial state parameters and variables lists
begin_month=1
state = states.to.use[i]
begin_year=get_start_year(state)
end_month = 6
end_year = 2008

#Step 1
#eda--determine some rough lag ranges
data = grab.data(state,begin_month,begin_year,end_month,end_year,sreturn=T)
test.data.vector = data$test.data.vector
test.data.vector = as.data.frame(test.data.vector)
dates = data$dates

robust.lm=do_robust_regression(state,test.data.vector,dates,independent.variables,automatic=T,plot=F, compute.anova = T)
resids = robust.lm$resids
phase1fits=robust.lm$robust.lm$fit
driftmodel = robust.lm$robust.lm
anova = robust.lm$anova
vol.model = do_vol_modeling(state,test.data.vector,phase1fits,resids,dates,automatic=F,plot=F,useGPH=F)
volmodel = vol.model$model

sink("aggregate_data.txt",append=T)
print(state)
print(summary(driftmodel))
print(anova)



#Step 5
#Curve fitting of explanatory variables
begin_year=1998
data = grab.data(state,begin_month,begin_year,end_month,end_year,sreturn=F)
test.data.vector2 = data$test.data.vector
test.data.vector2 = as.data.frame(test.data.vector2)
dates = data$dates

new.curves=curvefit(test.data.vector2,state,plot=F,begin_year,begin_month,end_year,end_month+5)
shifted.multivar.test.data.vector2 = shift.df.multi(new.curves,state,independent.variables)
driftmodel = robust.lm$robust.lm
volmodel = vol.model$model
begin_year=1998
data = grab.data(state,begin_month,begin_year,end_month,end_year,sreturn=T)
test.data.vector = data$test.data.vector
test.data.vector = as.data.frame(test.data.vector)
dates = data$dates
projections = forecaster(driftmodel,resids,volmodel,independent.variables,test.data.vector,new.curves,plot=T,plotindex=T, plot.ses =F)
index_forecasts = projections$projectedindex
for(k in 1:6){
dev.off()
}
index_forecasts = index_forecasts[c((length(index_forecasts)-60):length(index_forecasts))]
oneyrchange = (index_forecasts[12]-index_forecasts[1])/index_forecasts[1]
twoyrchange = (index_forecasts[24]-index_forecasts[12])/index_forecasts[12]
threeyrchange = (index_forecasts[36]-index_forecasts[24])/index_forecasts[24]
fouryrchange = (index_forecasts[48]-index_forecasts[36])/index_forecasts[36]
fiveyrchange = (index_forecasts[60]-index_forecasts[48])/index_forecasts[48]


print("1 to 5 year year on year % changes")
print(oneyrchange)
print(twoyrchange)
print(threeyrchange)
print(fouryrchange)
print(fiveyrchange)

print("")
print("")



sink()


}