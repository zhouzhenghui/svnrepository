current.state.name
data = current.state.data.sreturn
fdGPH(data)
sqrt(pi^2/6)/sqrt(134)*1.96
x = data
  x <- as.numeric(na.fail(as.ts(x)))
	bandw.exp = 0.5
    if (any(is.na(x))) 
        stop("NAs in x")
    if (NCOL(x) > 1) 
        stop("only implemented for univariate time series")
    n <- length(x)
    g <- trunc(n^bandw.exp)
    j <- 1:g
    kk <- 1:(n - 1)
    w <- 2 * pi * j/n
    mx <- mean(x)
    var.x <- sum((x - mx)^2)/n
    cov.x <- numeric(n - 1)
    for (k in kk) cov.x[k] <- sum((x[1:(n - k)] - mx) * (x[(1 + 
        k):n] - mx))/n
    periodogram <- numeric(g)
    for (i in 1:g) periodogram[i] <- var.x + 2 * sum(cov.x * 
        cos(w[i] * kk))
    pos <- j[periodogram > 0]
    y.reg <- log(periodogram[pos]/(2 * pi))
    x.reg <- 2 * log(2 * sin(w[pos]/2))
    fit <- lm(y.reg ~ x.reg)
    d.GPH <- coef(fit)[2]
    names(d.GPH) <- NULL
    x.r2 <- sum((x.reg - mean(x.reg))^2)
    var.d <- pi^2/(6 * x.r2)
    var.reg <- sum(resid(fit)^2)/((g - 1) * x.r2)
    list(d = -d.GPH, sd.as = sqrt(var.d), sd.reg = sqrt(var.reg))
