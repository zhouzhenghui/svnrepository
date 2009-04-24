"dategen"=function(start_month,start_year,end_month,end_year){
date.vector = NULL

year_range = start_year
month_range = seq(start_month,12,by=1)
for (i in year_range){
	for (j in month_range){
		temp.date = paste(j,"/",substr(as.character(i),3,4),sep="")
		date.vector = c(date.vector, temp.date)
	}
}



year_range = seq(start_year+1,end_year-1,by=1)
month_range = seq(1,12,by=1)
for (i in year_range){
	for (j in month_range){
		temp.date = paste(j,"/",substr(as.character(i),3,4),sep="")
		date.vector = c(date.vector, temp.date)
	}
}
month_range = seq(1,end_month,by=1)
for (j in month_range){
		temp.date = paste(j,"/",substr(as.character(end_year),3,4),sep="")
		date.vector = c(date.vector, temp.date)
	}
	date.vector
}