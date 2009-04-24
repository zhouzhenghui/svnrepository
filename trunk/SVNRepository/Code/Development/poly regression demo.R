data_code ="unemployment_rate";
data_code ="building_permits";
data.current.state = as.numeric(as.matrix((data$data))[,4])
Y = data.current.state
length(Y)
X = c(1:200)

model <- lm(Y ~ X + I(X^2)+ I(X^3) + I(X^4)+I(X^5) +I(X^6))
summary(model)

int=model$coefficients[1]
coeff1 = model$coefficients[2]
coeff2 = model$coefficients[3]
coeff3 = model$coefficients[4]
coeff4 = model$coefficients[5]
coeff5 = model$coefficients[6]
coeff6 = model$coefficients[7]

plot(Y,type="l")
plot(int+coeff1*X + coeff2*X^2 + coeff3*X^3 +coeff4*X^4 + coeff5*X^5 + coeff6*X^6,type="l")
X=c(1:200)

#################################

library(cobs)
model = conreg(X[seq(length(X)-24,length(X))],Y[seq(length(X)-24,length(X))], convex = TRUE)
?conreg
plot(model$x,model$yf)
?predict

xx <- seq(144,250)
yx <- predict(model, xx)


