## Box-Cox power transformation code 
## and comparison of before and after transformation
## using graphical and statistical techniques
## 
## Created Jan 20 2009
## Yasin & Felix

setwd("Z:/MEng Project/Box Cox")
setwd("Z:/SVNRepository/Data/Nile")
setwd("Z:/SVNRepository")


library(car)
library(MASS)

source("../source_functions/return_box_cox.R")

all.data = read.table("../../../Data/States/all.txt")
all.data.state = all.data[,1]
states = read.csv("../../../Data/States/states.csv")
state.names = as.matrix(states$State)


all.data=read.table("all.txt")
all.data.state = all.data[,1]
states=read.csv("states.csv")
state.names = as.matrix(states$State)


###--Matrix row is [state name, box cox lambda, lambda standard error]

box.cox.values = NULL
sreturn.constant.added = NULL   	#vector containing the constant that must be added to each state to make data > 0

for (i in 1:length(as.matrix(state.names))){
	current.state.name = as.matrix(state.names)[i]
	current.state.data = subset(all.data,all.data[,1]==current.state.name)[,4]
	n = length(current.state.data)
	current.state.data.sreturn = (current.state.data[2:n]-current.state.data[1:n-1])/current.state.data[1:n-1]
	current.state.data.sreturn.shifted = current.state.data.sreturn + abs(min(current.state.data.sreturn)) + 0.001

	sreturn.constant.added = rbind(sreturn.constant.added,append(state.names[i],abs(min(current.state.data.sreturn)) + 0.001))	

	box.cox.values = rbind(box.cox.values,append(state.names[i],c(return.box.cox(current.state.data.sreturn.shifted))))
}

sreturn.constant.added
box.cox.values


###????
sink("box cox output.txt", append=TRUE);
print(box.cox.values);
sink();

### testing NE
NE=read.table("NE.txt")[,4]
NE.sreturn = (NE[2:length(NE)]-NE[1:(length(NE)-1)])/NE[1:(length(NE)-1)]
NE.sreturn.shifted = NE.sreturn + abs(min(NE.sreturn)) + 0.001
NE.sreturn.big = NE.sreturn.shifted + 5
NE.diff = diff(NE)
NE.diff.shifted = NE.diff + abs(min(NE.diff)) + 0.001

box.cox.powers(NE)
#box.cox.powers(NE.sreturn)
box.cox.powers(NE.sreturn.shifted)
box.cox.powers(NE.sreturn.big)

library(nortest)
library(fBasics)
shapiroTest(box.cox(NE.sreturn.shifted,box.cox.powers(NE.sreturn.shifted)$lambda))
shapiroTest(box.cox(NE.sreturn.big,box.cox.powers(NE.sreturn.big)$lambda))

##Graphical normality tests for NE testing
par(mfrow=c(2,4))
qqnorm(NE.sreturn.shifted, main="NE simple return shifted")
qqline(NE.sreturn.shifted)
qqnorm(box.cox(NE.sreturn.shifted,box.cox.powers(NE.sreturn.shifted)$lambda), main="power transformed data (shifted)")
qqline(box.cox(NE.sreturn.shifted,box.cox.powers(NE.sreturn.shifted)$lambda))

qqnorm(NE.sreturn.big, main="NE simple return big")
qqline(NE.sreturn.big)
qqnorm(box.cox(NE.sreturn.big,box.cox.powers(NE.sreturn.big)$lambda), main="power transformed data (big)")
qqline(box.cox(NE.sreturn.big,box.cox.powers(NE.sreturn.big)$lambda))


plot(density(NE.sreturn), xlab = "Original NE simple returns data", main = "")
x <- seq(min(NE.sreturn),max(NE.sreturn),length=100)
hx <- dnorm(x,mean(NE.sreturn),sd(NE.sreturn))
lines(x,hx, lty = 2, col = 2)

plot(density(box.cox(NE.sreturn.shifted,box.cox.powers(NE.sreturn.shifted)$lambda)), xlab = "Power transformed NE simple returns", main = "")
x <- seq(min(box.cox(NE.sreturn.shifted,box.cox.powers(NE.sreturn.shifted)$lambda)),max(box.cox(NE.sreturn.shifted,box.cox.powers(NE.sreturn.shifted)$lambda)),length=100)
hx <- dnorm(x,mean(box.cox(NE.sreturn.shifted,box.cox.powers(NE.sreturn.shifted)$lambda)),sd(box.cox(NE.sreturn.shifted,box.cox.powers(NE.sreturn.shifted)$lambda)))
lines(x,hx, lty = 2, col = 2)

plot(density(NE.diff), xlab = "NE first difference data", main = "")
x <- seq(min(NE.diff),max(NE.diff),length=100)
hx <- dnorm(x,mean(NE.diff),sd(NE.diff))
lines(x,hx, lty = 2, col = 2)

plot(density(box.cox(NE.diff.shifted,box.cox.powers(NE.diff.shifted)$lambda)), xlab = "Power transformed NE first differenced", main = "")
x <- seq(min(box.cox(NE.diff.shifted,box.cox.powers(NE.diff.shifted)$lambda)),max(box.cox(NE.diff.shifted,box.cox.powers(NE.diff.shifted)$lambda)),length=100)
hx <- dnorm(x,mean(box.cox(NE.diff.shifted,box.cox.powers(NE.diff.shifted)$lambda)),sd(box.cox(NE.diff.shifted,box.cox.powers(NE.diff.shifted)$lambda)))
lines(x,hx, lty = 2, col = 2)

##---MI plots----####
MI=read.table("MI.txt")[,4]
MI.sreturn = (MI[2:length(NE)]-MI[1:(length(MI)-1)])/MI[1:(length(MI)-1)]
MI.sreturn.shifted = MI.sreturn + abs(min(MI.sreturn)) + 0.001
MI.sreturn.big = MI.sreturn.shifted + 5
MI.diff = diff(MI)

box.cox.powers(MI)
#box.cox.powers(MI.sreturn)
box.cox.powers(MI.sreturn.shifted)
box.cox.powers(MI.sreturn.big)

library(nortest)
library(fBasics)
shapiroTest(box.cox(MI.sreturn.shifted,box.cox.powers(NE.sreturn.shifted)$lambda))
shapiroTest(box.cox(MI.sreturn.big,box.cox.powers(NE.sreturn.big)$lambda))

plot(density(MI.sreturn), xlab = "Original MI simple returns data", main = "")
x <- seq(min(MI.sreturn),max(MI.sreturn),length=100)
hx <- dnorm(x,mean(MI.sreturn),sd(MI.sreturn))
lines(x,hx, lty = 2, col = 2)

plot(density(box.cox(MI.sreturn.shifted,box.cox.powers(MI.sreturn.shifted)$lambda)), xlab = "Power transformed MI simple returns", main = "")
x <- seq(min(box.cox(MI.sreturn.shifted,box.cox.powers(MI.sreturn.shifted)$lambda)),max(box.cox(MI.sreturn.shifted,box.cox.powers(MI.sreturn.shifted)$lambda)),length=100)
hx <- dnorm(x,mean(box.cox(MI.sreturn.shifted,box.cox.powers(MI.sreturn.shifted)$lambda)),sd(box.cox(MI.sreturn.shifted,box.cox.powers(MI.sreturn.shifted)$lambda)))
lines(x,hx, lty = 2, col = 2)

plot(density(MI.diff), xlab = "MI first difference data", main = "")
x <- seq(min(MI.diff),max(MI.diff),length=100)
hx <- dnorm(x,mean(MI.diff),sd(MI.diff))
lines(x,hx, lty = 2, col = 2)


return.box.cox(nile)

box.cox.powers(nile.sreturn)

nilelog=box.cox(nile,0)

x= box.cox.powers(nile)$lambda
y= box.cox.powers(nile)$stderr


##--Check if box.cox function took the log of the data
nilelog=log(nile)
length(which(nilelog==log(nile)))





###-Statistical normality tests
ksnormTest(nile)
ksnormTest(nilelog)
shapiroTest(nile)
shapiroTest(nilelog)
jarqueberaTest(nile)
jarqueberaTest(nilelog)
ad=adTest(nile)
ad
ad2=adTest(nilelog)
ad2
dagoTest(nile)
dagoTest(nilelog)
lillieTest(nile)
lillieTest(nilelog)

