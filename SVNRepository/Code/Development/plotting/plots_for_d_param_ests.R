library(Hmisc)
library(clim.pact)
names = seq(1:2)
text.names = c("AL","AK")

param= c(0.55,0.56)
param2=c(0.6,0.7)
se = c(0.14,0.16)

#get all three means in a row
stacked.d.hats = NULL
stacked.d.ses = NULL
stacked.D.hats = NULL
stacked.D.ses = NULL
stacked.d.stars = NULL
stacked.d.stars.ses = NULL
stacked.D.stars = NULL
stacked.D.stars.ses = NULL
for (j in 1:3){
dim(as.matrix(d.hat.frame))

	for (i in c(1,3,5)){
			stacked.d.hats = c(stacked.d.hats,as.numeric(as.matrix(d.hat.frame)[j,i]))
	}
	for (k in c(2,4,6)){
			stacked.d.ses = c(stacked.d.ses,as.numeric(as.matrix(d.hat.frame)[j,k]))
	}

	for (i in c(1,3,5)){
			stacked.d.stars = c(stacked.d.stars,as.numeric(as.matrix(d.star.frame)[j,i]))
	}
	for (k in c(2,4,6)){
			stacked.d.stars.ses = c(stacked.d.stars.ses,as.numeric(as.matrix(d.star.frame)[j,k]))
	}

	for (i in c(1,3,5)){
			stacked.D.hats = c(stacked.D.hats,as.numeric(as.matrix(D.hat.frame)[j,i]))
	}
	for (k in c(2,4,6)){
			stacked.D.ses = c(stacked.D.ses,as.numeric(as.matrix(D.hat.frame)[j,k]))
	}
	
	for (i in c(1,3,5)){
			stacked.D.stars = c(stacked.D.stars,as.numeric(as.matrix(D.star.frame)[j,i]))
	}
	for (k in c(2,4,6)){
			stacked.D.stars.ses = c(stacked.D.stars.ses,as.numeric(as.matrix(D.star.frame)[j,k]))
	}

}
sink("dhatframe.txt")
d.hat.frame#=read.csv("C:/Users/tim/Documents/ClassesSpring09/MastersProject/SVNRepository/timr_FinEngProj09/Data/Test Results (data)/D-test-with-unit-roots/dhatframe.txt")
sink()

sink("dstarframe.txt")
d.star.frame#=read.csv("C:/Users/tim/Documents/ClassesSpring09/MastersProject/SVNRepository/timr_FinEngProj09/Data/Test Results (data)/D-test-with-unit-roots/dstarframe.txt")

sink()

sink("D-hatframe.txt")
D.hat.frame#=read.csv("C:/Users/tim/Documents/ClassesSpring09/MastersProject/SVNRepository/timr_FinEngProj09/Data/Test Results (data)/D-test-with-unit-roots/D-hatframe.txt")

sink()

sink("D-starframe.txt")
D.star.frame#=read.csv("C:/Users/tim/Documents/ClassesSpring09/MastersProject/SVNRepository/timr_FinEngProj09/Data/Test Results (data)/D-test-with-unit-roots/D-starframe.txt")

sink()

read.table("D-starframe.txt")

##do some cleanup replacements on the stars
stacked.d.stars[which(stacked.d.stars==0)]=1000
stacked.d.stars.ses[which(stacked.d.stars.ses==0)]=0
stacked.D.stars[which(stacked.D.stars==0)]=1000
stacked.D.stars.ses[which(stacked.D.stars.ses==0)]=0

st1 = paste(state.names[1:3],"(gph)")
st2 = paste(state.names[1:3],"(sp)")
st3 = paste(state.names[1:3], "(wh)")
state.labels = sort(c(st1,st2,st3))
state.numbers = seq(1:length(state.labels))

errbar(state.numbers,stacked.d.hats,stacked.d.hats+stacked.d.ses,stacked.d.hats-stacked.d.ses,xaxt="n",xlab = "",ylab="", col=1, lty = 1,lwd=2, pch = 1, ylim = c(-1,1),cap = 0.1)
par(new=T)
errbar(state.numbers,stacked.d.stars,stacked.d.stars+stacked.d.stars.ses,stacked.d.stars-stacked.d.stars.ses,xaxt="n",ylab="d-estimate",xlab="",main="Fractional Difference Parameter Estimate", col="blue", lty = 2, pch = 2,lwd=3, ylim = c(-1,1),cap = 0.1)

axis(side=1,at=state.numbers ,labels = state.labels, las=3, cex.axis = 1)
abline(h=c(0,0.5),col="red",lty=2)
abline(v=state.numbers,col="lightgrey",lwd=0.6,lty=3)
abline(v=state.numbers[which(mod(state.numbers,3) == 0)]+0.5,col="grey",lwd=0.9,lty=2)

dev.new()

errbar(state.numbers,stacked.D.hats,stacked.D.hats+stacked.D.ses,stacked.D.hats-stacked.D.ses,xaxt="n",xlab = "",ylab="", col=1, lty = 1,lwd=2, pch = 1, ylim = c(-1,1),cap = 0.1)
par(new=T)
errbar(state.numbers,stacked.D.stars,stacked.D.stars+stacked.D.stars.ses,stacked.D.stars-stacked.D.stars.ses,xaxt="n",ylab="D-estimate",xlab="",main="Fractional Difference Parameter Estimate", col="blue", lty = 2, pch = 2,lwd=3, ylim = c(-1,1),cap = 0.1)

axis(side=1,at=state.numbers ,labels = state.labels, las=3, cex.axis = 1)
abline(h=c(0,0.5),col="red",lty=2)
abline(v=state.numbers,col="lightgrey",lwd=0.6,lty=3)
abline(v=state.numbers[which(mod(state.numbers,3) == 0)]+0.5, col="grey",lwd=0.9,lty=2)

