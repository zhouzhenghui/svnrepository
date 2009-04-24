library(Rmetrics)
library(fArma)
library(fracdiff)
library(fBasics)
library(tseries)
library(MASS)
library(fGarch)
library(nortest)

## --Simulate & Test Normal Data--#

results.shapiro = rep(0, 1000)
results.jb = rep(0, 1000)
results.ad = rep(0, 1000) 
results.cvm = rep(0, 1000)
results.ks = rep(0, 1000)

for(i in 1:1000){
	sim.data = rnorm(662, mean = 0, sd = 3)
	results.shapiro[i] = shapiroTest(sim.data)@test$p.value
	results.jb[i] = as.numeric(jarqueberaTest(sim.data)@test$p.value)
	results.ad[i] = adTest(sim.data)@test$p.value
	results.cvm[i] = cvm.test(sim.data)$p.value
	results.ks[i] = ks.test(sim.data, "pnorm", mean(sim.data), sd(sim.data), alternative = c("two.sided"),exact = TRUE)$p.value
}

sort.shapiro = sort(results.shapiro) 
sort.jb = sort(results.jb) 
sort.ad = sort(results.ad) 
sort.cvm = sort(results.cvm) 
sort.ks = sort(results.ks)

## --KS 2.5%, 50%, 97.5%
sort.ks[25]
sort.ks[500]
sort.ks[975]

## --Shapiro 2.5%, 50%, 97.5%
sort.shapiro[25]
sort.shapiro[500]
sort.shapiro[975]

## --JB 2.5%, 50%, 97.5%
sort.jb[25]
sort.jb[500]
sort.jb[975]

## --AD 2.5%, 50%, 97.5%
sort.ad[25]
sort.ad[500]
sort.ad[975]

## --CVM 2.5%, 50%, 97.5%
sort.cvm[25]
sort.cvm[500]
sort.cvm[975]



as.numeric(ksnormTest(sim.data)@test$p.value[1])
ks.test(sim.data, sim.data2, alternative = c("two.sided"),exact = NULL)
?ksnormTest