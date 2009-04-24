"fadf.test" = function (x, lags = 1, type = c("nc", "c", "ct"), title = NULL,  description = NULL, dparam = 0) 
{
    CALL = match.call()
    test = list()
    DNAME = deparse(substitute(x))
    test$data.name = DNAME
    if (class(x) == "timeSeries") 
        x = series(x)
    x = as.vector(x)
    if (lags < 0) 
        stop("Lags are negative")
    doprint = FALSE
    type = type[1]
    lags = lags + 1
    y = diff(x)
    n = length(y)
    z = embed(y, lags)
    y.diff = z[, 1]
    y.lag.1 = diffseries(x[lags:n],dparam)
    tt = lags:n
    if (lags > 1) {
        y.diff.lag = z[, 2:lags]
        if (type == "nc") {
            res = lm(y.diff ~ y.lag.1 - 1 + y.diff.lag)
        }
        if (type == "c") {
            res = lm(y.diff ~ y.lag.1 + 1 + y.diff.lag)
        }
        if (type == "ct") {
            res = lm(y.diff ~ y.lag.1 + 1 + tt + y.diff.lag)
        }
    }
    else {
        if (type == "nc") {
            res = lm(y.diff ~ y.lag.1 - 1)
        }
        if (type == "c") {
            res = lm(y.diff ~ y.lag.1 + 1)
        }
        if (type == "ct") {
            res = lm(y.diff ~ y.lag.1 + 1 + tt)
        }
    }
    res.sum = summary(res)
    if (doprint) 
        print(res.sum)
    if (type == "nc") 
        coefNum = 1
    else coefNum = 2
    STAT = res.sum$coefficients[coefNum, 1]/res.sum$coefficients[coefNum, 
        2]
    names(STAT) = "Dickey-Fuller"
    test$statistic = STAT
    PVAL = 0 #insert new pval later here
    names(PVAL) = ""
    test$p.value = PVAL 
    PARAMETER = lags - 1
    names(PARAMETER) = "Lag Order"
    test$parameter = PARAMETER
    if (is.null(title)) 
        title = "Augmented Dickey-Fuller Test"
    if (is.null(description)) 
        description = date()
    test$lm = res
    new("fHTEST", call = CALL, data = list(x = x), test = test, 
        title = as.character(title), description = .description())
}
#####end new test
