"source.all" = function(){
	

	source.files = read.csv("../../../Data/Supply Demand/Data/source_files.csv", header=FALSE)
	source.files = as.matrix(source.files)
	source("../ATS_2008.R")

	for (source.file in source.files){
		if(substr(source.file,nchar(source.file),nchar(source.file))=="R"){
			source(paste("../source_functions/",source.file,sep=""))
		}
	}
}
