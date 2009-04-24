

library("outliers")
library("fArma")
library("fracdiff")
library("Design")
library("dynlm")
library("fGarch")
#data codes
#median_income
#thirty_year_current_coupon
#thirty_year_commitment_rate
#mortgage_originations
#unemployment_rate
#primary_interest_rate
#population_size
#population_growth
#median_house_median_income
#delta_from_median_house_median_income
#foreclosures
#housing_stock
#inventory_turnover
#building_permits
#housing_price_index

##set directory for the initial analysis here, function calls will move relative to this
#setwd("Z:/SVNRepository/Code/Development/initial_analysis_r_code/")
setwd("C:/Users/tim/Documents/ClassesSpring09/MastersProject/SVNRepository/timr_FinEngProj09/Code/Development/initial_analysis_r_code")
setwd("Z:/M.Eng Project/SVNRepository/Code/Development/initial_analysis_r_code/")

setwd("//erevno/Courses/Agamas/Shirley/SVNRepository/Code/Development/initial_analysis_r_code")


source("../source_functions/get.sd.data.R")
source("../source_functions/get.sreturn.R")
source("../source_functions/multi.adf.test.R")
source("../source_functions/multi.adf.test.lags.R")
source("../source_functions/format.dates.R")
source("../source_functions/chop.R")
source("../source_functions/shift.R")
source("../ATS_2008.R")
#group 1
supply_demand_data_code_vector=c(
"median_income",
#"thirty_year_current_coupon",
"thirty_year_commitment_rate",
"mortgage_originations",
"unemployment_rate",
#"primary_interest_rate",
"population_size",
#"population_growth",
#"median_house_median_income",
#"delta_from_median_house_median_income"
"foreclosures",
#"housing_stock",
#"inventory_turnover",
"building_permits"
)


housing_data_code = c("housing_price_index")

begin_month=1
begin_year=1997
state = "NV"
end_month = 6
end_year = 2008

sd_data_vector = NULL
state_data = as.data.frame(get.sd.data(housing_data_code,state,begin_month,begin_year,end_month,end_year)$interpolated_data)
dates  = format.dates(as.matrix(as.data.frame(get.sd.data("building_permits",state,begin_month,begin_year,end_month,end_year))$data.Date))


names(state_data) = state
names(state_data) = state

i=6
for (i in 1:length(supply_demand_data_code_vector)){
	if(length(sd_data_vector)==0){
		sd_data_vector_temp = as.numeric(as.matrix(get.sd.data(supply_demand_data_code_vector[i],state,begin_month,begin_year,end_month,end_year)$interpolated_data))
		#sd_data_vector_temp = sd_data_vector_temp[dim(sd_data_vector_temp)[2]]
		sd_data_vector_temp = as.data.frame(sd_data_vector_temp)
		names(sd_data_vector_temp)=supply_demand_data_code_vector[i]
		sd_data_vector = sd_data_vector_temp
	}else{
		sd_data_vector_temp = as.numeric(as.matrix(get.sd.data(supply_demand_data_code_vector[i],state,begin_month,begin_year,end_month,end_year)$interpolated_data))
		#sd_data_vector_temp = sd_data_vector_temp[dim(sd_data_vector_temp)[2]]
		sd_data_vector_temp = as.data.frame(sd_data_vector_temp)

		names(sd_data_vector_temp)=supply_demand_data_code_vector[i]
		sd_data_vector = cbind(sd_data_vector,sd_data_vector_temp)
	}
}

combined_data_vector = cbind(sd_data_vector,state_data)

#add extra states here
extra_states = c("AL","AR","AZ","CA","FL","NV")
for (i in 1:length(extra_states)){
	state = extra_states[i]
	state_data = as.data.frame(get.sd.data(housing_data_code,state,begin_month,begin_year,end_month,end_year)$interpolated_data)
	names(state_data) = state
	combined_data_vector = cbind(combined_data_vector,state_data)
}

#get some extra versions of this matrix
combined_data_vector.sreturn = as.data.frame(sapply(combined_data_vector,"get.sreturn"))
combined_data_vector.diff = as.data.frame(sapply(combined_data_vector,"diff"))
combined_data_vector.doublediff = as.data.frame(sapply(combined_data_vector.sreturn,"diff"))



##END DATA FORMATION

##BEGIN RANDOM TESTS

test.data.vector = combined_data_vector.sreturn
#test.data.vector = shifted.test.vector
adf.tests = as.data.frame(sapply(test.data.vector,"multi.adf.test"))

adf.lags = as.data.frame(sapply(test.data.vector,"multi.adf.test.lags"))
cbind(adf.tests,adf.lags)
names(test.data.vector)
#ACFS
for (i in 1:dim(test.data.vector)[2]){
	dev.new()
	acf(test.data.vector[,i],lag.max= 40,main= paste("ACF of",names(test.data.vector)[i],"Series"))
	dev.new()
	acf(test.data.vector[,i]^2,lag.max= 40,main= paste("ACF of",names(test.data.vector)[i],"Series Squared"))
}
##CCFS
dependent =combined_data_vector.doublediff[,which(match(names(test.data.vector),state)==1)]
dependent =combined_data_vector.sreturn[,which(match(names(test.data.vector),state)==1)]
shift = 20
#shiftedur = unemployment_rate[(shift+1):length(unemployment_rate)]
truncdep = dependent[(shift+1):length(dependent)]

#truncdep = dependent[1:(length(dependent)-shift)]
shiftedur = unemployment_rate[1:(length(unemployment_rate)-shift)]
dev.new()
ccf(unemployment_rate,dependent, lag.max = 40)
cor(unemployment_rate,dependent)

plot(shiftedur,truncdep)
ccf(shiftedur,truncdep, lag.max = 40)
cor(shiftedur,truncdep)
x = as.matrix(unemployment_rate)
y = as.matrix(dependent)

par(mfrow=c(3,2))

for (i in 1:(which(match(names(test.data.vector),state)==1)-1)){
	#dev.new()
	ccf(test.data.vector[,i],dependent,lag.max= 40,main= paste("CCF of",names(test.data.vector)[i],"Series with",state))	
	#dev.new()
	plot(test.data.vector[,i],dependent, main = paste("Scatterplot of",names(test.data.vector)[i],"Series with",state))
}

##SCATTER PLOTS OF LAGGED VARIABLES
#negative shift to move the series backwards in time
shift=2
tempshifted = population_size[seq((shift+1),length(median_income),by=1)]
tempshifted = c(tempshifted,rep(tempshifted[length(tempshifted)],shift))


tempshifted = median_income[seq(1,length(median_income)-(shift),by=1)]
tempshifted = c(rep(tempshifted[1],shift),tempshifted)

tsh.rmoutlier = rm.outlier(tempshifted, fill = TRUE)
dep.rmoutlier = rm.outlier(dependent, fill = TRUE)

plot(tsh.rmoutlier,dep.rmoutlier )
dev.new()
plot(tempshifted,dependent)
plot(population_size,dependent)

#combined_data_vector = as.data.frame(sapply(combined_data_vector,"diff"))
acf(combined_data_vector_sreturn, lag.max =40)

names(combined_data_vector)

combined_data_vector = sapply(combined_data_vector,"log")
adf.test(combined_data_vector$building_permits,alternative = c("stationary"), k=11)
plot(as.ts(combined_data_vector$building_permits))
acf(as.ts(combined_data_vector$building_permits))

data = combined_data_vector$building_permits
acf.vals = acf(combined_data_vector, lag.max = 40, plot = FALSE)
#temp_names =  as.matrix(abbreviate(names(combined_data_vector),minlength=7))
temp_names =  names(combined_data_vector)

#temp_names = as.matrix(as.list(as.matrix(temp_names)))
acf.vals$snames = temp_names
plot(acf.vals)

##ccf plot to determine lagged or not



acf(combined_data_vector)

acf(sd_data_vector)

sd_data_vector_sreturn = sapply(sd_data_vector, "get.sreturn")
state_data_sreturn = sapply(state_data,"get.sreturn")
acf(combined_data_vector)

dev.new()
acf(combined_data_vector_sreturn)

plot(combined_data_vector)
plot(as.data.frame(combined_data_vector_sreturn))


plot(as.ts(combined_data_vector), main = "Monthly (interpolated) Time Series Plots : Begin 1997")
plot(as.ts(combined_data_vector_sreturn))

par(mfrow=c(5,1))
for(i in 6:10){

if(i!=10){
par(mar=c(2, 4, 2, 2) + 0.1)
}
else{
par(mar=c(4, 4, 2, 2) + 0.1)
}
plot(as.ts(combined_data_vector[,i]), ylab="", xaxt="n",main=names(combined_data_vector)[i])
}
tick.coords = seq(from=1,to=length(combined_data_vector[,i]), by = 1)
quarter.coords = seq(from=1,to=length(combined_data_vector[,i]), by = 3)
x.coords = seq(from=1,to=length(combined_data_vector[,i]),by=12)

axis(side=1,at=x.coords,labels = dates[x.coords], las =2,  tick = TRUE, tck=-.14)
axis(side=1,at=quarter.coords,labels = rep("",length(quarter.coords)), las =2, cex = 0.5, tick = TRUE)




names(sd_data_vector)


#individual adf tests
supply_demand_data_code_vector = c(supply_demand_data_code_vector,"housing_price_index")

adf.test.values = NULL
for (i in 1:length(supply_demand_data_code_vector)){
	adf.test.data = as.data.frame(get.sd.data(supply_demand_data_code_vector[i],state,begin_month,begin_year,end_month,end_year)$data)
	adf.test.data = as.matrix(as.numeric(as.matrix(adf.test.data[,dim(adf.test.data)[2]])))
	#adf.test.data = get.sreturn(adf.test.data)
	adf.test.values = c(adf.test.values,adf.test(adf.test.data)$p.value)

}

#END RANDOM TESTS

acf(building_permits)
# BEGIN MULTIPLE LINEAR REGRESSION
attach(combined_data_vector.sreturn)
state = "AZ"

#detach()
predictors = names(combined_data_vector)[1:length(names(combined_data_vector))]
cbind(predictors)

###MAKE SURE TO ADD CHANGE THIS IF NECESSARY
#predictor_numbers = c(1,3,4,5)
#predictor_numbers = c(2,4,6)
predictor_numbers = seq(1,7,by=1)
#predictor_numbers = seq(1:length(predictors))

#reset this every time
predictor_code = NULL

#predictors for order 1
for (j in predictor_numbers){
	if (j==predictor_numbers[length(predictor_numbers)]){
		predictor_code = paste(predictor_code,predictors[j],sep="") 
	}else{
		predictor_code = paste(predictor_code,predictors[j]," + ",sep="") 
	}	
	
}

#predictors for order 2
predictor_code = paste(predictor_code," + ", sep="")
for (j in predictor_numbers){
	if (j==predictor_numbers[length(predictor_numbers)]){
		predictor_code = paste(predictor_code,"I(",predictors[j],"^2)",sep="") 
	}else{
		predictor_code = paste(predictor_code,"I(",predictors[j],"^2)"," + ",sep="") 
	}	
	
}

#predictors for cross pairs
predictor_code = paste(predictor_code," + ", sep="")
special_mapping = seq(1,length(predictor_numbers),by=1)
for (j in 1:(length(special_mapping)-1)){
    for (k in (j+1):length(special_mapping)){
	if (k==length(special_mapping) && j==length(special_mapping)-1){
		predictor_code = paste(predictor_code,"I(",predictors[predictor_numbers[j]],"*",predictors[predictor_numbers[k]],")",sep="") 
	}else{
		predictor_code = paste(predictor_code,"I(",predictors[predictor_numbers[j]],"*",predictors[predictor_numbers[k]],")"," + ",sep="") 
	}
     }	
	
}
#test.data.vector = shifted.test.vector
test.data.vector=combined_data_vector.sreturn

form = as.formula(paste(state, " ~ ", predictor_code))
#combined_data_vector.sreturn$AZ  = dependent
fit <- lm(form, data=test.data.vector)
summary(fit) # show results
# Other useful functions
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit)
names(fit)
vif(fit)
#sum(anova(fit)[seq(1,5),2])/sum(anova(fit)[seq(1,6),2]) # anova table
vcov(fit) # covariance matrix for model parameters
influence(fit) # regression diagnostics 

names(summary(fit))
#tvalues
summary(fit)$coefficients[,3]
names(summary(fit)$coefficients[,3])[which(abs(summary(fit)$coefficients[,3])>=1.96)]


# diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)

plot(as.ts(AZ), lty = 2,col = "blue")
lines(as.ts(fitted(fit)), col = "black")


multi.adf.test(as.matrix(residuals(fit)))
multi.adf.test.lags(as.matrix(residuals(fit)))


dev.new()

plot(as.ts(residuals(fit)))
acf(as.ts(residuals(fit)), lag.max = 40, main = "ACF of Residuals")
acf(as.ts(residuals(fit)^2), lag.max = 40, main = "ACF of Residuals Squared")
pacf(as.ts(residuals(fit)), lag.max = 40, main = "PACF of Residuals")
pacf(as.ts(residuals(fit)^2), lag.max = 40, main = "PACF of Residuals Squared")

multi.adf.test(as.ts(residuals(fit)))
multi.adf.test.lags(as.ts(residuals(fit)))
Box.Ljung.test(as.ts(residuals(fit)),12,9)

residuals = as.matrix(residuals(fit))

##qq plot
qqnorm(residuals ,main="Normal Plot of Residuals", datax=TRUE)
qqline(residuals, datax=TRUE)

var(residuals)
##PHASE 2

#d.est = fdGPH(as.ts(residuals(fit)))$d
#d.est = fdSperio(as.ts(residuals(fit)))$d
d.est = whittleFit(residuals , order = c(0, 0), subseries = 1, method = c("farma"),trace = TRUE, spec = FALSE, title = NULL, description = NULL)@hurst$H  - 0.5
		hurst = whittleFit(residuals , order = c(0, 0), subseries = 1, method = c("farma"),trace = F, spec = FALSE, title = NULL, description = NULL)
		d.hat = hurst@hurst$H  - 0.5
		d.hat.sd = sqrt(.CetaARIMA(hurst@hurst$H,0, 0)/length(hurst@data$x))

residuals.fracdiff = diffseries(residuals,d.est)
acf(residuals.fracdiff,lag.max=40,main="ACF of Fractionally Differenced Simple Returns",sub=paste("d=",round(d.est,2),sep=""))
acf(residuals.fracdiff^2,lag.max=40,main="ACF of Squared Fractionally Differenced Simple Returns",sub=paste("d=",round(d.est,2),sep=""), cex.main = 0.8)
##qq plot
qqnorm(residuals.fracdiff ,main="Normal Plot of Residuals", datax=TRUE)
qqline(residuals.fracdiff, datax=TRUE)
#ARMA fitting to fractionally difference residuals, plus dropping parameters with fixed params
fit2 = arima(residuals.fracdiff,order = c(3,0,0))
fit2 = arima(residuals.fracdiff,order = c(3,0,0), fixed = c(0, 0, NA, 0))
acf(fit2$residuals,main="PACF of Residuals")

acf(fit2$residuals^2,main="ACF of Residuals Squared")
Box.Ljung.test(fit2$residuals,12,9)
Box.Ljung.test(fit2$residuals^2,12,9)

qqnorm(as.matrix(fit2$residuals) ,main="Normal Plot of Residuals", datax=TRUE)
qqline(as.matrix(fit2$residuals), datax=TRUE)

residuals2 = fit2$residuals

model.temp = garchFit(formula = ~arma(4,0)+garch(3,1), data=residuals2 , cond.dist = c("QMLE"), include.mean=TRUE, trace=FALSE);
#standardized residuals here
sresi=model.temp@residuals/model.temp@sigma.t
acf(sresi,lag.max=40,main="PACF of ARMA(4,0)+GARCH(3,0) Standardized Residuals")

acf(sresi^2,lag.max=40,main="ACF of ARMA(4,0)+GARCH(3,0) Squared Standardized Residuals")


qqnorm(sresi ,main="Normal Plot of Residuals", datax=TRUE)
qqline(sresi, datax=TRUE)



Box.Ljung.test(sresi,12,11)
Box.Ljung.test(sresi^2,12,8)

length(current.state.data.sreturn)
##end fractional garch modeling



# compare models
fit1 <- lm(y ~ x1 + x2 + x3 + x4, data=combined_data_vector)
fit2 <- lm(y ~ x1 + x2)
anova(fit1, fit2) 


# K-fold cross-validation
library(DAAG)
cv.lm(df=combined_data_vector, fit, m=6) # 3 fold cross-validation

# Assessing R2 shrinkage using 10-Fold Cross-Validation

fit <- lm(y~x1+x2+x3,data=combined_data_vector)

library(bootstrap)
# define functions
theta.fit <- function(x,y){lsfit(x,y)}
theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}

# matrix of predictors
X <- as.matrix(combined_data_vector[c("x1","x2","x3")])
# vector of predicted values
y <- as.matrix(combined_data_vector[c("y")])

results <- crossval(X,y,theta.fit,theta.predict,ngroup=10)
cor(y, fit$fitted.values)**2 # raw R2
cor(y,results$cv.fit)**2 # cross-validated R2 


### Stepwise Regression
library(MASS)
temp_fit <- lm(form,data=test.data.vector)
summary(temp_fit)
fit<-stepAIC(temp_fit, scope=form, trace=1)
summary(fit)
fit$anova$AIC[length(fit$anova$AIC)] #returns the AIC of the particular fitted model
###

# All Subsets Regression
library(leaps)
attach(test.data.vector)

test.data.vector

fit<-regsubsets(test.data.vector[,1:7],test.data.vector[,8])
summary(fit)
names(fit)

#leaps<-regsubsets(y~x1+x2+x3+x4,data=combined_data_vector,nbest=10)
# view results
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps,scale="r2")
# plot statistic by subset size
library(car)
subsets(leaps, statistic="rsq") 

