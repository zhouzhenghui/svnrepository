"get.lagvectors" = function(state){
	table = read.csv("../../../Data/Supply Demand/Data/lagranges.csv")
	#get the correct row
	new.table = table[which(table$State==state),2:dim(table)[2]]
	names = names(new.table)
	new.table = as.matrix(new.table)
	lagrange.table = NULL
	#i =3
	for (i in 1:dim(new.table)[2]){
		#see if this is a variable to omit entirely
		if(length(grep("n",as.character(new.table[,i])))>0){
			lagvector.temp=NA
		}else{
			#see if this is a cell with individual lags, or a range
			lag.trial =  as.numeric(unlist(strsplit(as.character(new.table[,i]), "#")))[1]
			if (!is.na(lag.trial)){
				#these are individual lags
				lagvector.temp = NULL
				lag.individuals =  as.numeric(unlist(strsplit(as.character(new.table[,i]), "#")))
				for (k in 1:length(lag.individuals)){
					lag.individual =  as.numeric(unlist(strsplit(as.character(new.table[,i]), "#")))[k]
					lagvector.temp = c(lagvector.temp,lag.individual)
				}
			

			}else{
				#this must be a lag range
				lag.lower =  as.numeric(unlist(strsplit(as.character(new.table[,i]), ",")))[1]
				lag.upper =  as.numeric(unlist(strsplit(as.character(new.table[,i]), ",")))[2]
				lagvector.temp=seq(lag.lower,lag.upper,by=1)
			
			}			
		}
			if (is.null(lagrange.table)){
					lagrange.table =  as.data.frame(lagvector.temp)				
			}else{			
				if(dim(lagrange.table)[1]>length(lagvector.temp)){
					#na-pad the lagvector
					nas = rep(NA,dim(lagrange.table)[1]-length(lagvector.temp))
					lagvector.temp = c(lagvector.temp,nas)
				}
				if(dim(lagrange.table)[1]<length(lagvector.temp)){
					#na-pad the lagrange.table
					nas =rep(NA,dim(lagrange.table)[2])	

					for(j in 1:(length(lagvector.temp)-dim(lagrange.table)[1])){
						lagrange.table = rbind(lagrange.table,nas,deparse.level=0)

					}
				}
					lagrange.table = cbind(lagrange.table,lagvector.temp)
			}
	}
	names(lagrange.table)=names
	lagrange.table
}
