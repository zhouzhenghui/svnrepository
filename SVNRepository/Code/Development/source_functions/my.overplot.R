"myoverplot"=function (formula,dates = NULL, data = parent.frame(),state, same.scale = FALSE, 
    xlab, ylab, xlim, ylim, min.y, max.y, log = "", panel = "panel.overplot", 
    subset, plot = TRUE, groups, main, f = 2/3, ...) 
{
    if (length(formula) != 3 || length(formula[[3]]) != 3 || 
        formula[[3]][[1]] != as.name("|")) 
        stop("Formula must be of the form y ~ x1 | x2")
    if (!missing(subset)) {
        flag <- eval(substitute(subset), envir = data)
        data <- data[flag, ]
    }
    cond <- eval(formula[[3]][[3]], envir = data, parent.frame())
    x <- eval(formula[[3]][[2]], envir = data, parent.frame())
    y <- eval(formula[[2]], envir = data, parent.frame())
    y.all.min <- min(y, na.rm = TRUE)
    y.all.max <- max(y, na.rm = TRUE)
    x.all.min <- min(x, na.rm = TRUE)
    x.all.max <- max(x, na.rm = TRUE)
    if (y.all.min == y.all.max) 
        browser()
    if (length(cond) == 0) {
        cond <- list(as.factor(rep(1, length(x))))
    }
    if (!is.factor(cond)) {
        cond <- factor(cond)
    }
    mycall <- match.call(expand.dots = FALSE)
    mycall$panel <- mycall$plot <- mycall$groups <- mycall$same.scale <- NULL
    mycall$min.y <- mycall$max.y <- NULL
    mycall$data <- data
    mycall$formula[3] <- formula[[3]][2]
    if (is.character(panel)) 
        panel <- as.name(panel)
    else panel <- deparse(substitute(panel))
    mycall[[1]] <- as.name(panel)
    if (same.scale) {
        if (missing(ylim)) 
            mycall$ylim <- range(y[y > 0], na.rm = TRUE)
    }
    if (missing(xlim)) 
        if (log %in% c("x", "xy")) 
            mycall$xlim <- range(x[x > 0], na.rm = TRUE)
        else mycall$xlim <- range(x, na.rm = TRUE)
    tmp <- na.omit(data.frame(x, y, cond))
    leveln <- sapply(split(tmp, tmp$cond), nrow)
    if (missing(groups)) 
        #groups <- names(leveln)[which(names(leveln)!=state)]
	   groups <- names(leveln)[which(names(leveln)!=state)]
	   groups <- c(state,groups)	
    ngroups <- length(groups)
    if (!missing(min.y) && length(min.y == 1)) 
        min.y <- rep(min.y, length = ngroups)
    if (!missing(max.y) && length(max.y == 1)) 
        max.y <- rep(max.y, length = ngroups)
    oldpar <- par()["mar"]
    on.exit(par(oldpar))
	
	if(length(groups)>2){
	    par(mar = par("mar") + c(0, ngroups * 2.5, 0, 0))
	}else{
	    par(mar = par("mar") + c(0, ngroups/2 * 2.5, 0, ngroups/2 * 2.5))

	}
    i <- 1
    for (level in groups) {
        if (i > 1) {
            par(new = TRUE)
        }
        mycall$subset <- (cond == level)
        mycall$ylab <- ""
        mycall$xlab <- ""
        mycall$xaxt = "n"
        mycall$yaxt = "n"
        mycall$pch = "."
        mycall$col = i
        mycall$lty = 1
        tmp.y <- y[mycall$subset & cond == level]
        min.tmp.y <- min(tmp.y, na.rm = TRUE)
        max.tmp.y <- max(tmp.y, na.rm = TRUE)
        if (!missing(min.y) || !missing(max.y)) {
            if (!missing(min.y) && !missing(max.y)) {
                mycall$ylim <- c(min.y[i], max.y[i])
            }
            else if (missing(min.y) && !missing(max.y)) {
                if (same.scale) 
                  mycall$ylim <- c(y.all.min, max.y[i])
                else mycall$ylim <- c(min.tmp.y, max.y[i])
            }
            else {
                if (same.scale) 
                  mycall$ylim <- c(min.y[i], y.all.max)
                else mycall$ylim <- c(min.y[i], max.tmp.y)
            }
        }
        if (plot) {
            status <- try(eval(mycall, parent.frame()))
            if ("try-error" %in% class(status)) 
                break
        }
        usr <- par("usr")
		if(length(groups)<=2 && i<=2){
			sidetemp = ifelse(i==1,2,4)
 		        axis(side = sidetemp , line = 2.5 * (1 - 1), lty = i, col = i, lwd = 2)
		}else{
			axis(side = 2, line = 2.5 * (i - 1), lty = i, col = i, lwd = 2)

		}
	  

		if(length(groups)<=2 && i<=2){
			if(i==1){
	 		        #title(ylab = level, line = 2.5 * (i - 1))
				  mtext(level, side=2, line = 2.5 * (i + 1 - 1))
	
			}else{
				mtext(level, side=4, line = 2.5 * (i - 1))
			}
		}else{
			title(ylab = level, line = 2.5 * (i - 1))

		}
 	
       
        if (i == 1) {
	            x.coords = seq(from=1,to=length(dates),by=12)
			tick.coords = seq(from=1,to=length(dates), by = 1)
			quarter.coords = seq(from=1,to=length(dates), by = 3)
			axis(side=1,at=x.coords,labels = dates[x.coords], las =2,  tick = TRUE, tck=-.04)
			axis(side=1,at=quarter.coords,labels = rep("",length(quarter.coords)), las =2, cex = 0.5, tick = TRUE)
		}
        i <- i + 1
    }
    if (missing(xlab)) 
        xlab <- as.character(formula[[3]][[2]])
    if (missing(ylab)) 
        ylab <- as.character(formula[[2]])
    if (missing(main)) 
        main <- paste("plot of ", xlab, "vs.", ylab, "by", as.character(formula[[3]][[3]]))
    if (i > 1) {
        title(main = main, xlab = xlab)
        title(ylab = ylab, line = 2.5 * (i - 1))
    }
    par(oldpar)
    invisible(split(data.frame(x, y), cond))
}
