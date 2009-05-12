source("../source_functions/multi.adf.test.R")
source("../source_functions/multi.adf.test.lags.R")
source("../source_functions/pretty.ts.graph.R")
source("../source_functions/custom.overplot.R")

"dataframe.eda" = function(test.data.vector, state, dates, basicTS = T, comparisonTS = T, allTS = T, CCF = T, scatter = T, ACF = T, PACF = T, same.scale=FALSE, singleplot=FALSE) {
	##Basic Time Series Plots	
	if (basicTS){
	for (i in 1:dim(test.data.vector)[2]){
			
			title = paste("TS Plot of",state,names(test.data.vector)[i])
			pretty.ts.graph(test.data.vector[,i],title,dates)
		}
	}

	

	#all time series plot	
	if (comparisonTS){
		for (i in 1:dim(test.data.vector)[2]){
			#dev.new()
			subset.vector = c(state,names(test.data.vector)[i])
			new.test.data.vector = subset(test.data.vector, select =subset.vector)
			custom.overplot(new.test.data.vector, state, dates,same.scale)
		}
	}

	#all time series plot	
	if (allTS){	
			
			custom.overplot(test.data.vector, state, dates,same.scale)		
	}

	##Stationarity Tests
	adf.test = NA
	adf.lags = NA
	#adf.tests = as.data.frame(sapply(test.data.vector,"multi.adf.test"))
	#adf.lags = as.data.frame(sapply(test.data.vector,"multi.adf.test.lags"))
	#return.adf.vector = cbind(adf.tests,adf.lags)
		#Check (P)ACFS
		for (i in 1:dim(test.data.vector)[2]){
			if(ACF){
				dev.new()
				title = paste("ACF Plot of",state,ifelse(state==names(test.data.vector)[i],"HPI",names(test.data.vector)[i]),"Series")
				acf(test.data.vector[,i],lag.max= 40,main=title)
				dev.new()
				title = paste("ACF Plot of",state,ifelse(state==names(test.data.vector)[i],"HPI",names(test.data.vector)[i]),"Series Squared")
				acf(test.data.vector[,i]^2,lag.max= 40,main=title)
			}
			if(PACF){
				dev.new()
				title = paste("PACF Plot of",state,ifelse(state==names(test.data.vector)[i],"HPI",names(test.data.vector)[i]),"Series")
				pacf(test.data.vector[,i],lag.max= 40,main=title)
				dev.new()
				title = paste("PACF Plot of",state,ifelse(state==names(test.data.vector)[i],"HPI",names(test.data.vector)[i]),"Series Squared")
				pacf(test.data.vector[,i]^2,lag.max= 40,main=title)
			}
		}

		

	##CCFS

	dependent =test.data.vector[,which(match(names(test.data.vector),state)==1)]
	
	
		for (i in 1:dim(test.data.vector)[2]){
			if (CCF){
				dev.new()
				ccf(test.data.vector[,i],dependent,lag.max= 40,main= paste("CCF of",names(test.data.vector)[i],"Series with",state))	
			}
			if (scatter){
				dev.new()
				plot(test.data.vector[,i],dependent, main = paste("Scatterplot of",names(test.data.vector)[i],"Series with",state))
				lines(lowess(test.data.vector[,i],dependent), col="red")
				abline(lm(dependent~test.data.vector[,i]),col="black")
				print(summary(rlm(dependent~test.data.vector[,i])))
			}	
		}
	



	#structure(list(adf.test=return.adf.vector))

}