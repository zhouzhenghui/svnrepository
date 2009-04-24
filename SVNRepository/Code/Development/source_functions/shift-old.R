source("../source_functions/chop.R")

"shifter" = function(olddataframe,dependent,independents,lagvector){
# - means shift backwards in time (is a lagging indicator)
# + means shift forwards in time (is a leading indicator)

#start here
dependent.data = olddataframe[,which(names(olddataframe)==dependent)]
new.dependent.data = NULL

independent.data = olddataframe[,na.omit(match(independents,names(olddataframe)))]
new.independent.data = NULL
#get lagged vectors
lagging.indices = which(lagvector < 0)
leading.indices = which(lagvector > 0)
neither.indices = which(lagvector == 0)

#if no lagging variables skip step 1
if(length(leading.indices)==0){
new.dependent.data = dependent.data
new.independent.data = independent.data
}else{
max.leading = max(lagvector[leading.indices])

#trim dependent variable the first time
	temp.data = chop(dependent.data, max.leading, "beginning")	
	new.dependent.data  = as.data.frame(cbind(new.dependent.data , temp.data))
	names(new.dependent.data)[1]= dependent



#trim the neither indices
for (i in neither.indices){
	temp.data = chop(independent.data[,i],max.leading,"beginning")
	new.independent.data  = as.data.frame(cbind(new.independent.data , temp.data))
	names(new.independent.data)[dim(new.independent.data)[2]]= names(independent.data)[i]
}



#trim the lagging indices
for (i in lagging.indices){
	temp.data = chop(independent.data[,i],max.leading,"beginning")
	new.independent.data  = as.data.frame(cbind(new.independent.data , temp.data))
	names(new.independent.data)[dim(new.independent.data)[2]]= names(independent.data)[i]
}
#adjust the leading
for (i in leading.indices){
	temp.data = chop(independent.data[,i],max.leading-lagvector[i],"beginning")
	temp.data = chop(temp.data,lagvector[i],"end")	
	new.independent.data  = as.data.frame(cbind(new.independent.data , temp.data))
	names(new.independent.data)[dim(new.independent.data)[2]]= names(independent.data)[i]
}

}
if(length(lagging.indices)==0){
#if no lagging variables skip step 2
}else{
#STEP 2
#adjust for the lagging again in the dependent
max.lagging = max(abs(lagvector[lagging.indices]))

#trim dependent variable the second time
	temp.data = chop(as.matrix(new.dependent.data), max.lagging , "end")	
	new.dependent.data = NULL
	new.dependent.data  = as.data.frame(cbind(new.dependent.data , temp.data))
	names(new.dependent.data)[1]= dependent

independent.data = new.independent.data
new.independent.data = NULL


#trim the neither indices  2nd time
for (i in neither.indices){
	temp.data.name = independents[i]
	temp.data = independent.data[,which(names(independent.data)==temp.data.name)]
	temp.data =	chop(temp.data,max.lagging,"end")
	new.independent.data  = as.data.frame(cbind(new.independent.data , temp.data))
	names(new.independent.data)[dim(new.independent.data)[2]]= independents[i]

}

#trim the leading indices 
for (i in leading.indices){
	temp.data.name = independents[i]
	temp.data = independent.data[,which(names(independent.data)==temp.data.name)]
	temp.data =	chop(temp.data,max.lagging,"end")
	new.independent.data  = as.data.frame(cbind(new.independent.data , temp.data))
	names(new.independent.data)[dim(new.independent.data)[2]]= independents[i]

}

#trim the lagging indices 
for (i in lagging.indices){
	temp.data.name = independents[i]
	temp.data = independent.data[,which(names(independent.data)==temp.data.name)]
	temp.data =	chop(temp.data,abs(lagvector[i]),"beginning")
	temp.data =	chop(temp.data,max.lagging - abs(lagvector[i]),"end")
	new.independent.data  = as.data.frame(cbind(new.independent.data , temp.data))
	names(new.independent.data)[dim(new.independent.data)[2]]= independents[i]

}
}
cbind(new.independent.data,new.dependent.data)



}