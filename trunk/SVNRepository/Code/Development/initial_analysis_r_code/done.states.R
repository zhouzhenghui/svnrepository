states.to.use = c("CA", "OH", "SD", "FL", "TX", "KS", "NV", "VT", "ID", "MI", "AZ","MS", "KY","AL", "WV","ND","NJ", "GA","MA","NY")
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
print(driftmodel)
print(anova)
print("")

sink()


}