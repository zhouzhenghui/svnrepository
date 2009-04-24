###this function wraps arima, and estimates a more complete model by
###dropping insignifican coefficients and performing box tests
###returns a custom structure
##Tim


"complete.arma.model.with.stats" = function(data,ord = c(0,0,0), fix = rep(NA,sum(ord)+1),alpha = 0.05, box.params = c(12,8,8)){
	#start with initial model here
	model.fit = arima(data,order = ord, fixed = fix)
	coeffs = model.fit$coef

	
	t.stat.vector = NULL
	#figure out the t-statistics
	crit.intersect = 0;
	all.params.dropped = FALSE
	dropped.params = NULL	
	counter_adjust = 0
	crit.intersect.vec = NULL
	while(all.params.dropped == FALSE){
		
		t.stat.vector = NULL
		counter_adjust = 0
		for (counter in 1:length(coeffs)){
			if (length(crit.intersect.vec) == 0){
				t.stat.vector= c(t.stat.vector,model.fit$coef[counter]/sqrt(model.fit$var.coef)[counter-counter_adjust,counter-counter_adjust])		

			}else{
				if (counter == crit.intersect.vec[min(length(crit.intersect.vec),counter_adjust+1)]){
					t.stat.vector = c(t.stat.vector,1.01*qnorm(alpha/2))
					counter_adjust = counter_adjust + 1

				}else{
					t.stat.vector= c(t.stat.vector,model.fit$coef[counter]/sqrt(model.fit$var.coef)[counter-counter_adjust,counter-counter_adjust])		
				}	
			}
		}	
		coeffs = model.fit$coef
		se.vector = coeffs/t.stat.vector
		names(t.stat.vector)=names(coeffs)

		#re-estimate here with dropped parameters
		if(length(dropped.params)==0){
			dropped.params = rep(NA,length(coeffs))
		}
		crit1 = which(abs(t.stat.vector)<=abs(qnorm(alpha/2)))
		crit2 = which.min(abs(t.stat.vector))
		crit.intersect = intersect(crit1,crit2)
		crit.intersect.vec = c(crit.intersect.vec,crit.intersect)
		if (length(crit.intersect) == 0){
			all.params.dropped = TRUE
		}else{
			
			dropped.params[crit.intersect]=0
			if (length(which(dropped.params == 0))==length(dropped.params)){
				all.params.dropped = TRUE
			}else{
				model.fit= arima(data,order= ord, fixed = dropped.params)
				coeffs = model.fit$coef
				all.params.dropped = FALSE
			}
		}	
	
	}
	#check the roots of the ma-part (if there are any)
		ma.coeffs = coeffs[which(substr(names(coeffs),1,2)=="ma")]	
		if (length(ma.coeffs)>0){
			roots.norm = abs(polyroot(c(1,-ma.coeffs[1:length(ma.coeffs)])));
			gt1.roots = length(subset(roots.norm,roots.norm>1));
			num.roots = length(roots.norm);
			is.invertible = (gt1.roots==num.roots);
		}else{
			is.invertible = TRUE;
		}
	#perform box tests to determine mean/volatility equation appropriateness
		resids = model.fit$residuals
		#DONT CHANGE THIS STRUCTURE
		lag.to.use = box.params[1]
		mean.adj.DF = box.params[2]
		vol.adj.DF = box.params[3]

		mean.check.pval = Box.Ljung.test(resids,lag = lag.to.use,adj.DF = mean.adj.DF)$p.value;
		mean.check.stat = Box.Ljung.test(resids,lag = lag.to.use,adj.DF = mean.adj.DF)$statistic;

		vol.check.pval = Box.Ljung.test(resids^2,lag = lag.to.use,adj.DF = vol.adj.DF)$p.value;
		vol.check.stat = Box.Ljung.test(resids^2,lag = lag.to.use,adj.DF = vol.adj.DF)$statistic;

		has.resid.correlation=(mean.check.pval >= alpha)
		has.arch.effect = (vol.check.pval <= alpha)

	#output structure
	COEF <- coeffs
	STDERR <- se.vector
	TSTAT <- t.stat.vector
	FITMODEL <- model.fit$coef
	INVERTIBLE <- is.invertible
	MEANCHECK <- has.resid.correlation
	VOLCHECK <- has.arch.effect
	MEANPVAL <- mean.check.pval
	MEANSTAT <- mean.check.stat
	MEANDOF <- mean.adj.DF
	VOLPVAL <- vol.check.pval
	VOLSTAT <- vol.check.stat
	VOLDOF <- vol.adj.DF	
	SIGMA2 <- model.fit$sigma2
	MODEL <- model.fit

	names(COEF) <- names(coeffs)
	names(STDERR) <- names(coeffs)
   	names(TSTAT) <- names(coeffs)
	names(FITMODEL) <- names(coeffs)
	names(INVERTIBLE) <- "Invertible Model?"
	names(MEANCHECK) <- "Mean Equation Adequate?"
	names(VOLCHECK) <- "ARCH Effect?"
	
	names(MEANPVAL) <- "Resid Box Test P-Val"
	names(MEANSTAT) <- "Resid Box Test Stat"
	names(MEANDOF) <- "Resid Box Test DOF Used"
	names(VOLPVAL) <- "Resid^2 Box Test P-Val"
	names(VOLSTAT) <- "Resid^2 Box Test Stat"
	names(VOLDOF) <- "Resid^2 Box Test DOF Used"	
	names(SIGMA2) <- "Sigma^2 of residual series"
	names(MODEL) <- "Final model"

    	structure(list(coef = COEF, se = STDERR, 
      t.stat = TSTAT, fitmodel = FITMODEL, invertible = INVERTIBLE , mean.good=MEANCHECK,
	vol.good = VOLCHECK, Box.mean.p.value = MEANPVAL, Box.mean.stat = MEANSTAT,
	Box.mean.dof.adj=MEANDOF,Box.vol.pval=VOLPVAL,Box.vol.stat = VOLSTAT,Box.vol.dof.adj=VOLDOF,
	sigma2 = SIGMA2))
}