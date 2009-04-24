for (i in 1:length(data)){
		normality.test(

if (i==1){
			
			plot(parameter.vector.concatenated,col=i,xaxt="n", xlab = "", main = paste("Parameter estimates for",selector))
			at.val = axTicks(1, axp = c(1,n,n-1), usr = NULL, log = NULL)


			axis(1,labels = x.axis.lab, at = at.val, cex.axis = 0.75, las = 2)
		}
		else
		{
			points(parameter.vector.concatenated, col=i)
		}
