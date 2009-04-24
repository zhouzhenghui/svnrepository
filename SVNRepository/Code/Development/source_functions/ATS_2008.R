# Version 2008.11.21
# David S. Matteson
# Wrapper for several common R TS functions 
# (plus some new ones) for use with 
# ORIE 5550 Fall 2008

##############################################################################
"print.data.frame" = function (x, ..., digits = NULL, quote = FALSE,
     right = TRUE, srn = FALSE)
{
     if (length(x) == 0) {
         cat("NULL data frame with", length(row.names(x)), "rows\n")
     }
     else if (length(row.names(x)) == 0) {
         print.default(names(x), quote = FALSE)
         cat("<0 rows> (or 0-length row.names)\n")
     }
     else {
         if (!is.null(digits)) {
             op <- options(digits = digits)
             on.exit(options(op))
         }
         rowlab <- if (srn)
             rep("", nrow(x))
         else row.names(x)
         prmatrix(format(x), rowlab = rowlab, ..., quote = quote,
             right = right)
     }
     invisible(x)
}
##############################################################################
"ACF.PACF" = function(data, lag.max = 12, round = 2, acf = TRUE, pacf = TRUE, cols = 1){
    lag.max = floor(lag.max)
    col.length = ceiling(lag.max/cols) #
    ACF = acf(data, plot=F, lag.max = lag.max)$acf[-1]
    PACF = pacf(data, plot=F, lag.max = lag.max)$acf
    Lag = 1:lag.max
    temp = round( cbind(Lag, ACF, PACF), 2)
#    temp = if (cols == 2) {rbind(temp[1:col.length,], temp[(col.length+1):lag.max,])}
#    temp = if (cols == 3) {rbind(temp[1:col.length,], temp[(col.length+1):(2*col.length),], temp[(2*col.length+1):lag.max,])}
    temp = as.data.frame(temp) 
    if(!acf)  {print(temp[,-2], srn = T)}        
    if(!pacf) {print(temp[,-3], srn = T)}        
    if(acf & pacf)      {print(temp, srn = T)}        
}
##############################################################################
"Box.Ljung.test" = function(data, lag = 12, adj.DF = NULL){
    DF = ifelse(is.null(adj.DF), lag, adj.DF)
    temp = Box.test(data, lag=lag, type = 'Ljung')
    
    STATISTIC <- temp$statistic
    adj.p.value = 1-pchisq(STATISTIC , DF)
    if(is.null(adj.DF)){
		PVAL <- temp$p.value
    }
    else{
	  PVAL <- adj.p.value 
    }
	
	METHOD <- "Box-Ljung test"    
	PARAMETER <- DF
	names(PVAL) <- "P-Val"
	names(STATISTIC) <- "X-squared"
   	names(PARAMETER) <- "df"
    	structure(list(statistic = STATISTIC, parameter = PARAMETER, 
        p.value = PVAL, method = METHOD, data.name = temp$data.name), 
        class = "htest")

}

##############################################################################
"ACF.test" = function(data, lag){
    N = length(data)
    ACF = acf(data, plot=F, lag.max = lag)$acf[(lag+1)]
    Statistic = sqrt(N) * ACF 
    p.value = round( 2*(1-pnorm(Statistic)) , 3)
    temp = as.data.frame(cbind( round(Statistic,2), p.value, N ) )
    names(temp) = c("Statistic", "P-value", "T")
    print(temp, srn = T)
}
"PACF.test" = function(data, lag){
    N = length(data)
    PACF = pacf(data, plot=F, lag.max = lag)$acf[lag]
    Statistic = sqrt(N) * PACF 
    p.value = round( 2*(1-pnorm(Statistic)) , 3)
    temp = as.data.frame(cbind( round(Statistic,2), p.value, N ) )
    names(temp) = c("Statistic", "P-value", "T")
    print(temp, srn = T)
}
##############################################################################
"AR.AIC" = function(data, lag.max){
    lag.max = floor(abs(lag.max))
    AIC = numeric(lag.max)
    for(i in 1:lag.max){
        temp = arima(data, order = c(i,0,0))
        AIC[i] = temp$aic
    }
    AIC
}
"AR.BIC" = function(data, lag.max){
    lag.max = floor(abs(lag.max))
    BIC = numeric(lag.max)
    N = length(data); lnN = log(N)
    for(i in 1:lag.max){
        temp = arima(data, order = c(i,0,0))
        d = i + 2
        BIC[i] = temp$aic - 2*d + d*lnN
    }
    BIC
}
##############################################################################
"eacf" = function (z,ar.max=7,ma.max=13) 
{
	LAG1 = function(z,LAG=1){ c(rep(NA,LAG),z[1:(length(z)-LAG)]) }
	reupm = function(m1,nrow,ncol){
		k=ncol-1
		m2=NULL
		for (i in 1:k){
			i1=i+1
			work=LAG1(m1[,i])
			work[1]=-1
			temp=m1[,i1]-work*m1[i1,i1]/m1[i,i]
			temp[i1]=0
			m2=cbind(m2,temp)
		}
		m2
	}
	ceascf = function(m,cov1,nar,ncol,count,ncov,z,zm){
		result=0*seq(1,nar+1)
		result[1]=cov1[ncov+count]
		for (i in 1:nar) {
			temp=cbind(z[-(1:i)],zm[-(1:i),1:i])%*%c(1,-m[1:i,i])
			result[i+1]=acf(temp,plot=F,lag.max=count)$acf[count+1]
		}
		result
	}

	ar.max=ar.max+1
	ma.max=ma.max+1
	nar=ar.max-1
	nma=ma.max
	ncov=nar+nma+2
	nrow=nar+nma+1
	ncol=nrow-1
	z=z-mean(z)
	zm=NULL
	for(i in 1:nar) zm=cbind(zm,LAG1(z,LAG=i))
	cov1=acf(z,lag.max=ncov,plot=F)$acf
	cov1=c(rev(cov1[-1]),cov1)
	ncov=ncov+1
	m1=matrix(0,ncol=ncol,nrow=nrow)
	for(i in 1:ncol) m1[1:i,i] = ar.ols(z,order.max=i,aic=F,demean=F,intercept=F)$ar
	eacfm=NULL
	for (i in 1:nma) {
		m2=reupm(m1=m1,nrow=nrow,ncol=ncol)
		ncol=ncol-1
		eacfm=cbind(eacfm, ceascf(m2,cov1,nar,ncol,i,ncov,z,zm))
		m1=m2
	}
	work=1:(nar+1)
	work=length(z)-work+1
	symbol=NULL
	for (i in 1:nma) {
		work=work-1
		symbol=cbind(symbol,ifelse(abs(eacfm[,i])>2/sqrt(work), 'X','-'))
	}
	rownames(symbol)=0:(ar.max-1)
	colnames(symbol)=0:(ma.max-1)
	rownames(eacfm)=0:(ar.max-1)
	colnames(eacfm)=0:(ma.max-1)
	cat('AR/MA\n')
	print(symbol, quote = FALSE)
	invisible(list(eacf=eacfm,ar.max=ar.max,ma.ma=ma.max,symbol=symbol))
}

##############################################################################
"garchOxFit" <-
function (formula.mean = ~arma(0, 0), formula.var = ~garch(1, 1), 
    series = x, cond.dist = c("gaussian", "t", "ged", "skewed-t"), 
    include.mean = TRUE, include.var=TRUE, truncation = 100, 
    trace = TRUE, title = NULL, arch.in.mean=0) 
{
    fit = list()
    fit$x = series
##    include.var = TRUE
##    include.var=FALSE   (ATS 2008, This command can be used to estimate
##     volatility model without the constant term,e.g. RiskMetric models.
##      RST(4/27/2008)
## As an alternative, I moved "include.var" command into the argument.
## 
    fit$csts = c(include.mean, include.var)
    distris = 0:3
    names(distris) = c("gaussian", "t", "ged", "skewed-t")
    distri = distris[cond.dist[1]]
    fit$cond.dist = cond.dist[1]
    if (missing(formula.mean)) {
        fit$formula.mean = ~arma(0, 0)
        fit$arma.orders = c(0, 0)
    }
    else {
        fit$arma.orders = as.numeric(strsplit(strsplit(strsplit(as.character(formula.mean), 
            "\\(")[[2]][2], "\\)")[[1]], ",")[[1]])
    }
    arfima = FALSE
    fit$arfima = as.integer(arfima)
    if (missing(formula.var)) {
        fit$formula.var = ~garch(1, 1)
        fit$garch.orders = c(1, 1)
    }
    else {
        fit$garch.orders = as.numeric(strsplit(strsplit(strsplit(as.character(formula.var), 
            "\\(")[[2]][2], "\\)")[[1]], ",")[[1]])
    }
#    arch.in.mean = 0
    fit$arch.in.mean = arch.in.mean
    models = 1:11
    names(models) = c("garch", "egarch", "gjr", "aparch", "igarch", 
        "figarch.bbm", "figarch.chung", "fiegarch", "fiaparch.bbm", 
        "fiaparch.chung", "hygarch")
    selected = strsplit(as.character(formula.var), "\\(")[[2]][1]
    fit$model = models[selected]
    nt = length(series)
    ident = paste(selected, as.character(floor(runif(1) * 10000)), 
        sep = "")
    parameters = c(csts = fit$csts, distri = distri, arma = fit$arma.orders, 
        arfima = fit$arfima, garch = fit$garch.orders, model = fit$model, 
        inmean = fit$arch.in.mean, trunc = truncation, nt = nt)
    write(x = parameters, file = "OxParameter.txt")
    write(x = "X", file = "OxSeries.csv", ncolumns = 1)
    write(x=series, file = "OxSeries.csv", ncolumns = 1, append = TRUE)
    command = "C:\\Ox\\bin\\oxl.exe C:\\Ox\\lib\\GarchOxModelling.ox"
    fit$ox = system(command, show.output.on.console = trace, 
        invisible = TRUE)
    fit$model = selected
    fit$call = match.call()
    fit$residuals = scan("OxResiduals.csv", skip = 1, quiet = TRUE)
    fit$condvars = scan("OxCondVars.csv", skip = 1, quiet = TRUE)
    fit$coef = matrix(scan("OxParameters.csv", skip = 1, quiet = TRUE), 
        byrow = TRUE, ncol = 3)
    fit$title = title
    if (is.null(title)) 
        fit$title = "GARCH Ox Modelling"
    fit$description = as.character(date())
    class(fit) = "garchOx"
    fit
}

##############################################################################
#### ATS 2008   (2008.09.30) created mainly to estimate EGARCH models in R.
"egarchOxFit" <-
function (formula.mean = ~arma(0, 0), formula.var = ~garch(1, 1), 
    series = x, cond.dist = c("gaussian", "t", "ged", "skewed-t"), 
    include.mean = TRUE, include.var=TRUE, truncation = 100, 
    trace = TRUE, title = NULL, arch.in.mean=0) 
{
    fit = list()
    fit$x = series
#    include.var = TRUE
    fit$csts = c(include.mean, include.var)
    distris = 0:3
    names(distris) = c("gaussian", "t", "ged", "skewed-t")
    distri = distris[cond.dist[1]]
    fit$cond.dist = cond.dist[1]
    if (missing(formula.mean)) {
        fit$formula.mean = ~arma(0, 0)
        fit$arma.orders = c(0, 0)
    }
    else {
        fit$arma.orders = as.numeric(strsplit(strsplit(strsplit(as.character(formula.mean), 
            "\\(")[[2]][2], "\\)")[[1]], ",")[[1]])
    }
    arfima = FALSE
    fit$arfima = as.integer(arfima)
    if (missing(formula.var)) {
        fit$formula.var = ~garch(1, 1)
        fit$garch.orders = c(1, 1)
    }
    else {
        fit$garch.orders = as.numeric(strsplit(strsplit(strsplit(as.character(formula.var), 
            "\\(")[[2]][2], "\\)")[[1]], ",")[[1]])
    }
#    arch.in.mean = 0
    fit$arch.in.mean = arch.in.mean
    models = 1:11
    names(models) = c("garch", "egarch", "gjr", "aparch", "igarch", 
        "figarch.bbm", "figarch.chung", "fiegarch", "fiaparch.bbm", 
        "fiaparch.chung", "hygarch")
    selected = strsplit(as.character(formula.var), "\\(")[[2]][1]
    fit$model = models[selected]
    nt = length(series)
    ident = paste(selected, as.character(floor(runif(1) * 10000)), 
        sep = "")
    parameters = c(csts = fit$csts, distri = distri, arma = fit$arma.orders, 
        arfima = fit$arfima, garch = fit$garch.orders, model = fit$model, 
        inmean = fit$arch.in.mean, trunc = truncation, nt = nt)
    write(x = parameters, file = "OxParameter.txt")
    write(x = "X", file = "OxSeries.csv", ncolumns = 1)
    write(x=series, file = "OxSeries.csv", ncolumns = 1, append = TRUE)
    command = "C:\\Ox\\bin\\oxl.exe C:\\Ox\\lib\\GarchOxModelling_w.ox"
    fit$ox = system(command, show.output.on.console = trace, 
        invisible = TRUE)
    fit$model = selected
    fit$call = match.call()
    fit$residuals = scan("OxResiduals.csv", skip = 1, quiet = TRUE)
    fit$condvars = scan("OxCondVars.csv", skip = 1, quiet = TRUE)
    fit$coef = matrix(scan("OxParameters.csv", skip = 1, quiet = TRUE), 
        byrow = TRUE, ncol = 3)
    fit$title = title
    if (is.null(title)) 
        fit$title = "GARCH Ox Modelling"
    fit$description = as.character(date())
    class(fit) = "garchOx"
    fit
}


##############################################################################
"forecast" = function(m1,rt,orig,h,xre=NULL){
# m1: is a model object
# orig: is the forecast origin
# rt: the time series
# xre: the independent variables
# h: forecast horizon
#
# Requires the library fSeries or FinTS (use Rmetrics)
regor=c(m1$arma[1],m1$arma[6],m1$arma[2])
seaor=list(order=c(m1$arma[3],m1$arma[7],m1$arma[4]),perioa=m1$arma[5])
T=length(rt)
if(orig > T)orig=T
x=rt[1:orig]
mm=arima(x,order=regor,seasonal=seaor,xreg=xre)
fore=predict(mm,h)
pred=fore$pred
se=fore$se
print(pred)
print(se)
forecast<-list(origin=orig,pred=pred,se=se)
}

##############################################################################
"backtest" = function(m1,rt,orig,h,xre=NULL,fixed=NULL){
# m1: is a model object
# orig: is the forecast origin
# rt: the time series
# xre: the independent variables
# h: forecast horizon
# fixed: parameter constriant
#
# Requires the library fSeries or FinTS (use Rmetrics)
regor=c(m1$arma[1],m1$arma[6],m1$arma[2])
seaor=list(order=c(m1$arma[3],m1$arma[7],m1$arma[4]),perioa=m1$arma[5])
T=length(rt)
if(orig > T)orig=T
if(h < 1) h=1
rmse=rep(0,h)
nori=T-orig
err=matrix(0,nori,h)
jlast=T-1
for (n in orig:jlast){
jcnt=n-orig+1
x=rt[1:n]
mm=arima(x,order=regor,seasonal=seaor,xreg=xre,fixed=fixed)
if (is.null(xre))
nx=NULL else nx=xre[(n+1):(n=h)]
fore=predict(mm,h,newxreg=nx)
pred=fore$pred
obsd=rt[(n+1):(n+h)]
err[jcnt,]=obsd-pred
}
for (i in 1:h){
iend=nori-i+1
tmp=err[1:iend,i]
rmse[i]=sqrt(sum(tmp^2)/iend)
}
print("RMSE of out-of-sample forecasts")
print(rmse)
backtest<-list(origin=orig,error=err,rmse=rmse)
}

##############################################################################
"mvwindow" <- function(rt,win=30){
# rt: return series
# win: window size.
T=length(rt)
vol=rep(0,T)
if (win < (T+1)){
for (i in win:T){
ist=i-win+1
x=rt[ist:i]
v=var(x)
vol[i]=v
}
}
mvwindow<-list(volatility=vol)
}

##############################################################################
"rev.rows" <- function(x){
 nc = dim(x)[2]
 for (j in 1:nc){
   x[,j] = rev(x[,j])
 }
 x
}
##############################################################################
"ohlc" = function(op,hi,lo,cl,f=0.729,nsplit=0,split=0){
# Compute volatility based on daily open, high, low and closing prices.
# f is the fraction of the day that trading is closed.
# 
# If nsplit > 0, then split contains the information of (time,ratio) 
# of the splits.
#
n=length(op)
nm1=n-1
#print(split)
s0 = diff(cl)^2
s1 = (op[2:n]-cl[1:nm1])^2/(2*f) + (cl[2:n]-op[2:n])^2/(2*(1-f))
s2 = 0.3607*(hi[2:n]-lo[2:n])^2
s3 = 0.17*(op[2:n]-cl[1:nm1])^2/f + 0.83*(hi[2:n]-lo[2:n])^2/((1-f)*4*log(2))
s5= 0.5*(hi[2:n]-lo[2:n])^2-0.386*(cl[2:n]-op[2:n])^2
s6= 0.12*(op[2:n]-cl[1:nm1])^2/f + 0.88 *s5/(1-f)
# take care of stock split
if (nsplit > 0){
if(nsplit==1){
spp=matrix(split,1,2)
}
for (i in 1:nsplit){
idx=spp[i,1]
ratio=spp[i,2]
s0[idx]=(cl[idx+1]-cl[idx]*ratio)^2
s1[idx]= (op[idx+1]-cl[idx]*ratio)^2/(2*f)+(cl[idx+1]-op[idx+1])^2/(2*(1-f))
s3[idx]=0.17*(op[idx+1]-cl[idx]*ratio)^2/f + 0.83*(hi[idx+1]-lo[idx+1])^2/((1-f)*4*log(2))
s6[idx]= 0.12*(op[idx+1]-cl[idx]*ratio)^2/f + 0.88 *s5[idx]/(1-f)
}
}
ohlc = list(s0sq=s0,s1sq=s1,s2sq=s2,s3sq=s3,s5sq=s5,s6sq=s6)
}

##############################################################################
"yz" = function(op,hi,lo,cl,window=63,nsplit=0,split=0){
# Compute the volatility based on Yang and Zhang (2000) method.
n=length(op)
nm1=n-1
ot=log(op[2:n])-log(cl[1:nm1])
ut=log(hi[2:n])-log(op[2:n])
dt=log(lo[2:n])-log(op[2:n])
ct=log(lo[2:n])-log(op[2:n])
if (nsplit > 0){
 if(nsplit==1){
  split=matrix(split,1,2)
}
 for (i in 1:nsplit){
 idx=split[i,1]
 ratio=split[i,2]
 ot[idx]=log(op[idx+1])-log(cl[idx]*ratio)
}
}
syz2=rep(0,nm1)
k=0.34/(1.34+(window+1)/(window-1))
for (i in window:nm1){
ist=i-window+1
s02=var(ot[ist:i])
sc2=var(ct[ist:i])
srs2=mean(ut[ist:i]*(ut[ist:i]-ct[ist:i])+dt[ist:i]*(dt[ist:i]-ct[ist:i]))
syz2[i]=s02+k*sc2+(1-k)*srs2
}
yz = list(yzsq = syz2)
}

##############################################################################
"backnnet" <- function(x,y,size,orig,linout=TRUE,skip=TRUE,maxit=1000){
# x: input (the number of columns is the number of input nodes.
# y: output variable
# orig: is the forecast origin
# h: (forecast horizon fixed at 1 for now)
# size: the number of nodes in the hidden layer.
# It requires the "nnet" library within "Rmetrics"
T=length(y)
if(orig > T)orig=T
nori=T-orig
err=rep(0,nori)
jlast=T-1
#
for (n in orig:jlast){
xx=x[1:n,]
yy=y[1:n]
mm=nnet(xx,yy,size=size,linout=linout,skip=skip,maxit=maxit,
        trace=F,decay=1e-2,reltol=1e-7,abstol=1e-7,range=1.0)
xp=x[n,]
fore=predict(mm,xp)
jcnt=n-orig+1
err[jcnt]=y[(n+1)]-fore
}
rmse=sqrt(sum(err^2)/(T-orig-1))
print("RMSE of out-of-sample forecasts")
print(rmse)
backnnet<-list(origin=orig,error=err,rmse=rmse)
}

##############################################################################
"mq" = function(x,lag, df.adj = 0){
# Compute multivariate Ljung-Box test statistics
#
x = as.matrix(x)
nr = dim(x)[1]
nc = dim(x)[2]
g0 = var(x)
ginv = solve(g0)
qm = 0.0
df = 0
out = as.data.frame(matrix(0,lag,3))
names(out) = c("m", "Q(m)", "p-value")
for (i in 1:lag){
  x1 = x[(i+1):nr,]
  x2 = x[1:(nr-i),]
  g = cov(x1,x2)
  g = g*(nr-i-1)/(nr-1)
  h = t(g)%*%ginv%*%g%*%ginv
  qm = qm+nr*nr*sum(diag(h))/(nr-i)
  df = df+nc*nc
  pv = 1-pchisq(qm,df-df.adj)
#  print(c(i,qm,pv))
out[i,] = c(i,round(qm,2),round(pv,3))
}
print(out)
}

##############################################################################
"coint" = function(x,lags=0,trend=1){
# Perform Johansen's co-integration test.
# trend = 0 means no constant
#       = 1 means with a constant
#       = 2 means with time trend.
#
T=nrow(x)
k=ncol(x)
y=as.matrix(x)
# setup the differenced data
dy=cbind(rep(0,k),t(diff(y)))
dy=t(dy)
#for (i in 1:k){
#dy[,i]=c(0,diff(y[,i]))
#}
ist = lags+1
ut=dy[(ist+1):T,]
vt=y[ist:(T-1),]
# Run regression to remove the impact of stationary part or trend.
nobe=T-ist
idx=0
if( (trend > 0) || (lags > 0)){
idim=trend + lags*k
##print(c(idim,nobe))
bx=matrix(0,nobe,idim)
if(trend >0){
bx[,1]=rep(1,nobe)
idx=1
if(trend > 1){
idx=2
bx[,2]=c(1:nobe)
}
}
if(lags > 0){
for (j in 1:lags){
bx[,(idx+1):(idx+k)]=dy[(ist-j+1):(T-j),]
idx=idx+k
}
}
xpx=t(bx)%*%bx
##print(xpx)
xpxinv=solve(xpx)
y1=dy[(ist+1):T,]
xpy=t(bx)%*%y1
b1=xpxinv%*%xpy
ut=y1-bx%*%b1
zm1=y[ist:(T-1),]
xpy=t(bx)%*%zm1
b2=xpxinv%*%xpy
vt=zm1-bx%*%b2
}
# Perform eigenvalues &eigenvector analysis
S00=t(ut)%*%ut/(T-lags)
S01=t(ut)%*%vt/(T-lags)
S11=t(vt)%*%vt/(T-lags)
print("S00-mtx")
print(S00)
print("S01-mtx")
print(S01)
print("S11-mtx")
print(S11)
S00inv=solve(S00)
S11inv=solve(S11)
A=S11inv%*%t(S01)
A1=S00inv%*%S01
AA=A%*%A1
print("The U-mtx")
print(AA)
mm=eigen(AA)
ev=mm$values
#print(ev)
evto=mm$vectors
lktr=rep(0,k)
lkmx=rep(0,k)
for (j in 1:k){
lkmx[j]=-nobe*log(1-ev[j])
}
#print(lkmx)
lktr[k]=lkmx[k]
for (j in 1:(k-1)){
lktr[(k-j)]=lktr[(k-j+1)]+lkmx[(k-j)]
}
#print(lktr)
rnk=c(1:k)-1
resu=cbind(rnk,ev,lktr,lkmx)
print("Rank, Eigenvalue, trace, & max-eig")
print(resu,digits=3)
#print(paste("selected order: aic =",aicor))
coint<-list(trace=lktr,maxeig=lkmx,eigvalue=ev,eigvector=evto)

}

##############################################################################

# EWMA estimators

"nllik.ewma" = function(lambda, innov){
  clambda = 1-lambda
  Sigma.hat = var(innov)
  invSigma.hat = chol2inv(chol(Sigma.hat)) # invSigma.hat = solve(Sigma.hat)
  detSigma.hat = det(Sigma.hat)
  llik = -0.5*log(detSigma.hat) - 0.5*crossprod(innov[1,],invSigma.hat)%*%innov[1,]
  llik = llik -0.5*log(detSigma.hat) - 0.5*crossprod(innov[2,],invSigma.hat)%*%innov[2,]
  n = dim(innov)[1]
  for(i in 3:n){
    atm1 = innov[(i-1),]
    at = innov[i,]
    denom = 1 - lambda^(i-1)
#    Sigma.hat = clambda * tcrossprod(atm1) + lambda * Sigma.hat #approx
    Sigma.hat = (clambda/denom) * tcrossprod(atm1) + (lambda*(1-lambda^(i-2))/denom) * Sigma.hat #exact 
    invSigma.hat = chol2inv(chol(Sigma.hat)) # invSigma.hat = solve(Sigma.hat)
    detSigma.hat = det(Sigma.hat)
    llik = llik - 0.5*(log(detSigma.hat) + crossprod(at,invSigma.hat)%*%at)
  }
  nllik = -llik; nllik
}

"est.ewma" = function(lambda.0, innov){
  out = nlminb(lambda.0, nllik.ewma, lower = 0.001, upper = 0.999, innov = innov)
  lambda.hat = out$par; lambda.hat
}

"sigma.ewma" = function(lambda, innov){
  clambda = 1-lambda
  Sigma.hat = var(innov)
  n = dim(innov)[1]
  d = dim(innov)[2]
  Sigma.t = array(0, c(d,d,n))
  Sigma.t[,,1:2] = Sigma.hat
  for(i in 3:n){
    atm1 = innov[(i-1),]
    at = innov[i,]
    denom = 1 - lambda^(i-1)
#    Sigma.hat = clambda * tcrossprod(atm1) + lambda * Sigma.hat #approx
    Sigma.t[,,i] = (clambda/denom) * tcrossprod(atm1) + (lambda*(1-lambda^(i-2))/denom) * Sigma.t[,,(i-1)] #exact 
   }
 Sigma.t
}

"matrix.sqrt" <- function(A)
{
  sva <- svd(A)
  if (min(sva$d)>=0)
     Asqrt <- t(sva$v %*% (t(sva$u) * sqrt(sva$d)))
  else
     stop("Matrix square root is not defined")
  return(Asqrt)
}

##############################################################################