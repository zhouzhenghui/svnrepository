"fadf.test" = function (x, alternative = c("stationary", "explosive"), k = trunc((length(x) - 1)^(1/3)), dparam= 0) 
{
    if (NCOL(x) > 1) 
        stop("x is not a vector or univariate time series")
    if (any(is.na(x))) 
        stop("NAs in x")
    if (k < 0) 
        stop("k negative")
    alternative <- match.arg(alternative)
    DNAME <- deparse(substitute(x))
    k <- k + 1
    y <- diff(x)
    n <- length(y)
    z <- embed(y, k)
    yt <- z[, 1]
    xt1 <- diffseries(x[k:n],dparam) #TIM changed this line
    tt <- k:n
    if (k > 1) {
        yt1 <- z[, 2:k]

        res <-  res <- lm(yt ~ xt1 + 1 + yt1)
    }
    else res <- lm(yt ~ xt1 + 1)

    res.sum <- summary(res)
    STAT <- res.sum$coefficients[2, 1]/res.sum$coefficients[2, 2]
   
    PARAMETER <- k - 1
    METHOD <- "Augmented Dickey-Fuller Test"
    names(STAT) <- "Dickey-Fuller"
    names(PARAMETER) <- "Lag order"
    structure(list(statistic = STAT, parameter = PARAMETER, alternative = alternative, 
        method = METHOD, data.name = DNAME), 
        class = "htest")
}