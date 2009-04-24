## Box-Cox power transformation code 
## and comparison of before and after transformation
## using graphical and statistical techniques
## 
## Created Jan 20 2009
## Vikas, Tommy


setwd("Z:/SVNRepository/Data/Nile")

library(car)
library(MASS)

nile=read.table("nile.dat")[,1]

box.cox.powers(nile)
nilelog=box.cox(nile,0)

##--Check if box.cox function took the log of the data
nilelog=log(nile)
length(which(nilelog==log(nile)))


###--Graphical normality tests
par(mfrow=c(2,2))
qqnorm(nile, main="Original data")
qqline(nile)
qqnorm(nilelog, main="Log transformed data")
qqline(nilelog)

plot(density(nile), xlab = "Original Nile data", main = "")
x <- seq(min(nile),max(nile),length=100)
hx <- dnorm(x,mean(nile),sd(nile))
lines(x,hx, lty = 2, col = 2)

plot(density(nilelog), xlab = "log(nile)", main = "")
x <- seq(min(nilelog),max(nilelog),length=100)
hx <- dnorm(x,mean(nilelog),sd(nilelog))
lines(x,hx, lty = 2, col = 2)


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

