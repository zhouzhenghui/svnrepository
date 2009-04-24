x = rnorm(100,0,1)
alpha = 5
beta = 0.2

y = alpha + (beta * x )

acf(x)
acf(y)

x.sreturn = get.sreturn(x)
y.sreturn = get.sreturn(y)
acf(x.sreturn) 
acf(y.sreturn)
ccf(x.sreturn ,y.sreturn)


z = x[-1]
z = c(z,z[length(z)])
z.sreturn = get.sreturn(z)


plot(x,y)
plot(z,y)

ccf(x,y)
#z lags y
ccf(z,y)



plot(x.sreturn,y.sreturn)

plot(z.sreturn,y.sreturn)
ccf(z.sreturn,y.sreturn)
ccf(x.sreturn,y.sreturn)


