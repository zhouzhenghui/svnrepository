"format.dates" = function(dates){

dates.split = strsplit(dates,"/")
month = NULL
year  = NULL
for (i in 1:length(dates)){
	month = c(month,dates.split[[i]][1])
	year = c(year,substr(dates.split[[i]][3],1,4))
}
paste(month,"/",year,sep="")
}