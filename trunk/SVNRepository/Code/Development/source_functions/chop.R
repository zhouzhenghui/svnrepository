"chop" = function(vector, chopamount, location = "beginning"){

if (location == "beginning"){
	vectortemp = vector[(chopamount+1):length(vector)]
}else{
	vectortemp = vector[1:(length(vector)-chopamount)]

}
	vectortemp
}
