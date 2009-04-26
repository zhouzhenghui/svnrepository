"install.all.packages"=function(install=T){
	table = read.csv("../../../Data/Supply Demand/Data/packages.csv", header=FALSE)
	table=as.matrix(table)
	for (package.name in table){
		if(install){
			install.packages(package.name, lib=.libPaths())
		}
		library(as.character(package.name), character.only=TRUE)
	}


}