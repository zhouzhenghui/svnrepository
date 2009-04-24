library(urca)
data(ecb)
m3.real <- ecb[,"m3"]/ecb[,"gdp.defl"]
gdp.real <- ecb[,"gdp.nom"]/ecb[,"gdp.defl"]
rl <- ecb[,"rl"]
ecb.data <- cbind(m3.real, gdp.real, rl)
m3d.po <- ca.po(ecb.data, type="Pz")
summary(m3d.po)
plot(as.ts(ecb.data[,c(2,3)]))

ecdet = c("none", "const", "trend")
johansen.test = ca.jo(ecb.data[,c(1,2)], type = c("trace"), ecdet = c("const"), K = 2, spec=c("transitory"), season = NULL, dumvar = NULL)

johansen.test@cval 
johansen.test@teststat