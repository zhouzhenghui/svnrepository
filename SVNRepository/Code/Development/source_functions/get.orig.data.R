"get.orig.data" = function(start.value,sreturns){
	#(o2-o1)/o1= r1
	#r1*o1+o1 = o2
	#r2*o2+o2=o3...
	orig.data = c(start.value)
	for (i in 1:length(sreturns)){
		orig.data=c(orig.data,orig.data[i]*sreturns[i]+orig.data[i])
	}
	orig.data
}