source("../source_functions/get.orig.data.R")
source("../source_functions/standardize.R")
library("outliers")
"smoother"=function(test.data.vector){

iterations =0
grubbs = TRUE
grubbsdone = ifelse(grubbs,TRUE,FALSE)
use.returns = TRUE
new.total.dataframe = NULL
total_size =dim(test.data.vector)[1]
#splitsize=18
splitsize=total_size
split = floor(total_size/splitsize)
for(counter in 1:3){
#while(grubbsdone==TRUE){
#change this later
for (i in 1:(split)){
	rowstart = (i-1)*splitsize + 1
	rowend = ifelse(i==(split+1),total_size ,i*splitsize)
	
	if(iterations == 0 && grubbs == TRUE){
			temp.dataframe = test.data.vector[seq(rowstart,rowend),]
	}else{
			temp.dataframe  = new.total.dataframe
	}
	
	
	if(use.returns){
		#plot(as.ts(simple.return.temp.dataframe))
		simple.return.temp.dataframe = sapply(temp.dataframe,"get.sreturn")
		simple.return.temp.dataframe = as.data.frame(simple.return.temp.dataframe)
		#perform the grubbs test on this chunk, and find which value this corresponds to
		grubbs.outliers.mask = as.data.frame(t(as.matrix(sapply(simple.return.temp.dataframe,"grubbs.test")[3,])))<0.01
		#remove outliers for each column
		smoothed.simple.return.temp.dataframe=simple.return.temp.dataframe
		for (q in 1:(dim(simple.return.temp.dataframe)[2]-1)){	#skip the HPI
			if(grubbs.outliers.mask[1,q]){
				smoothed.simple.return.temp.dataframe[,q] = rm.outlier(smoothed.simple.return.temp.dataframe[,q],fill=T)
			}
		}
		#look at the maximum difference between the two, and use only 1 value
		replacements = sapply(abs(smoothed.simple.return.temp.dataframe-simple.return.temp.dataframe),"which.max")

		#now replace the original values using the new simple returns
		new.temp.dataframe = temp.dataframe	
		for(p in 1:(dim(new.temp.dataframe)[2]-1)){
			new.temp.dataframe[as.numeric(replacements[p])+1,p]=NA
		}
	}else{
		standardized.temp.dataframe = standardize(temp.dataframe)
		smoothed.temp.dataframe = rm.outlier(standardized.temp.dataframe,fill=T)
		replacements = sapply(abs(standardized.temp.dataframe-smoothed.temp.dataframe),"which.max")
		new.temp.dataframe = temp.dataframe	
		for(k in 1:(dim(new.temp.dataframe)[2])){
			new.temp.dataframe[as.numeric(replacements[k]),k]=NA
		}

	}
	
	if (i==1){
		new.total.dataframe = new.temp.dataframe	
	}else{
		new.total.dataframe = rbind(new.total.dataframe,new.temp.dataframe)
	}
}
new.total.dataframe = na.omit(new.total.dataframe, method="ie")
grubbsdone.temp = TRUE
	for(y in 1:(dim(grubbs.outliers.mask)[2])){	
		grubbsdone.temp = grubbsdone.temp && !as.matrix(grubbs.outliers.mask)[y]
	}
grubbsdone = !grubbsdone.temp
#print(grubbsdone)
iterations = iterations + 1
}
new.total.dataframe
}

