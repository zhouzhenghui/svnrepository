###function that returns simulation results for fractional difference (d) parameter estimates
###estimation methods can be "GPH","Sperio","WhittleFARIMA","WhittleFGN"

##optional arma order is available for joint fitting of an arma model and simulation (d estimtes comes from ARIMA(p,d,q)) 
##no.of.sims is the number of MC simulations used to get parameter estimates
##doplot plots a histogram of the d-parameter estimates
#dhat is the original parameter estimate
#dhat
#dstar are the simulated d-values
#dstar.quantiles are the simulated quantiles based on alpha (100-alpha% CI quantiles)
#Dstar are the simulated d-values for the fractionally differenced process
#Dstar.quantiles are the same, but for Dstar (not dstar)


"fractional.diff.est.sim" = function(state.name="FL", data = c(1,0,1,0,1),arma.order = c(0,0),n.to.use = length(data), 
						no.of.sims=1000, include.model = FALSE, method = "WhittleFARIMA", 
						alpha = 0.05, sim = TRUE, doplot=TRUE,resample=TRUE, burn.in=200)

 {
	###choose method here####
	if (method == "GPH"){
		d  = fdGPH(data);
		d.hat = d$d;
		d.hat.sd = d$sd.as
		D = fdGPH(diffseries(data,d.hat))
		D.hat = D$d
		D.hat.sd = D$sd.as
	}

	if (method == "Sperio"){
		d = fdSperio(data);
		d.hat = d$d;
		d.hat.sd = d$sd.as
		D = fdSperio(diffseries(data,d.hat))
		D.hat = D$d
		D.hat.sd = D$sd.as
	}

	if (method == "WhittleFARIMA"){
		hurst = whittleFit(data, order = c(0, 0), subseries = 1, method = c("farma"),trace = F, spec = FALSE, title = NULL, description = NULL)
		d.hat = hurst@hurst$H  - 0.5
		d.hat.sd = sqrt(.CetaARIMA(hurst@hurst$H,arma.order[1], arma.order[2])/length(hurst@data$x))
		Hurst = whittleFit(diffseries(data,d.hat), order = c(arma.order[1], arma.order[2]), subseries = 1, method = c("farma"),trace = T, spec = FALSE, title = NULL, description = NULL)
		D.hat = Hurst@hurst$H  - 0.5
		D.hat.sd = sqrt(.CetaARIMA(Hurst@hurst$H,arma.order[1], arma.order[2])/length(Hurst@data$x))

	}

	if (method == "WhittleFGN"){
		hurst = whittleFit(data, order = c(arma.order[1], arma.order[2]), subseries = 1, method = c("fgn"),trace = TRUE, spec = FALSE, title = NULL, description = NULL)
		d.hat = hurst@hurst$H  - 0.5
		d.hat.sd = sqrt(.CetaFGN(hurst@hurst$H)/length(hurst@data$x))
		#using farma instead of fgn because of errors with optimimzer
		Hurst = whittleFit(diffseries(data,d.hat), order = c(arma.order[1], arma.order[2]), subseries = 1, method = c("farma"),trace = F, spec = FALSE, title = NULL, description = NULL)
		D.hat = Hurst@hurst$H  - 0.5
		D.hat.sd = sqrt(.CetaARIMA(Hurst@hurst$H,arma.order[1], arma.order[2])/length(hurst@data$x))
		
	} 
	
		
	data.diffseries = diffseries(data,d.hat);
	
	if (length(which(arma.order==c(0,0))) > 0){
		ma.coeffs = 0;
		ar.coeffs = 0;
		resid = data.diffseries
	}else{
		current.state.arfima = arima(data.diffseries, order = arma.order);
		ma.coeffs = as.numeric(current.state.arfima$coef[which(substr(names(current.state.arfima$coef),1,2)=="ma")]);
		ar.coeffs = as.numeric(current.state.arfima$coef[which(substr(names(current.state.arfima$coef),1,2)=="ar")]);
		resid = current.state.arfimia$residuals
	}
	

	d.star.vec=0;
	D.star.vec=0;
	d.star.mean = 0;
	D.star.mean = 0;
	d.star.sd =0;
	D.star.sd = 0;
	d.star.median = 0;
	D.star.median = 0;

	
	#don't try to run if d not in -1/2 to 1/2
if(sim == TRUE){
	if(abs(d.hat)>0.5){
		print(paste("error for simulating",state.name,"d out of range, d=",d.hat))
	}else{

		##MC sim for n=sample length
		
		for (k in 1:no.of.sims){
			if (resample==TRUE){
				if(method == "WhittleFGN"){
					sim.data =as.numeric(as.matrix(fgnSim(n = n.to.use, H = d.hat+0.5,method = c("beran"))))
				}else{
					sim.data =as.numeric(as.matrix(armaSim(model = list(ar = ar.coeffs, ma = ma.coeffs, d = d.hat), n =n.to.use, n.start = burn.in,start.innov = sample(resid, n.to.use+burn.in, replace = TRUE, prob = NULL))));			
				}
			}else{
				if(method == "WhittleFGN"){
					sim.data = as.numeric(as.matrix(fgnSim(n = n.to.use, H = d.hat+0.5,method = c("beran"))))
				}else{
					sim.data = as.numeric(as.matrix(armaSim(model = list(ar = ar.coeffs, ma = ma.coeffs, d = d.hat), n =n.to.use, n.start = burn.in,rand.gen = rnorm)));			
				}
			}
			##Simulate for appropriate method
			if (method == "GPH"){
				d.star  = fdGPH(sim.data)$d;
				sim.data.star = diffseries(data,d.star);
				D.star = fdGPH(sim.data.star)$d;
			}

			if (method == "Sperio"){
				d.star = fdSperio(sim.data)$d;
				sim.data.star = diffseries(data,d.star);
				D.star = fdSperio(sim.data.star)$d;
			}	

			if (method == "WhittleFARIMA"){
				hurst = whittleFit(sim.data, order = c(arma.order[1], arma.order[2]), subseries = 1, method = c("farma"),trace = FALSE, spec = FALSE, title = NULL, description = NULL);
				d.star = hurst@hurst$H  - 0.5;
				sim.data.star = diffseries(data,d.star);
				hurst.star = whittleFit(sim.data.star, order = c(arma.order[1], arma.order[2]), subseries = 1, method = c("farma"),trace = F, spec = FALSE, title = NULL, description = NULL);
				D.star = hurst.star@hurst$H  - 0.5;
			}	

			if (method == "WhittleFGN"){
				hurst = whittleFit(sim.data, order = c(arma.order[1], arma.order[2]), subseries = 1, method = c("fgn"),trace = FALSE, spec = FALSE, title = NULL, description = NULL);
				d.star = hurst@hurst$H  - 0.5;
				sim.data.star = diffseries(data,d.star);
				hurst.star = whittleFit(sim.data.star, order = c(arma.order[1], arma.order[2]), subseries = 1, method = c("farma"),trace = FALSE, spec = FALSE, title = NULL, description = NULL);
				D.star = hurst.star@hurst$H  - 0.5;
			} 
			d.star.vec = c(d.star.vec,d.star);
			D.star.vec = c(D.star.vec,D.star);

		}
		d.star.mean = mean(d.star.vec);
		D.star.mean = mean(D.star.vec);
		d.star.sd = sd(d.star.vec);
		D.star.sd = sd(D.star.vec);
		d.star.median = median(d.star.vec);
		D.star.median = median(D.star.vec);

	}
}
	if(include.model){
		model = paste(state.name,"-ARFIMA(",arma.order[1],",",round(d.hat,2),",",arma.order[3],")",sep="")	;
	}else{
		model = state.name;
	}
	
	d.star.quantiles = as.numeric(quantile(d.star.vec, probs = c(alpha/2,1-alpha/2)))
	D.star.quantiles = as.numeric(quantile(D.star.vec, probs = c(alpha/2,1-alpha/2)))
	
	d.star.sdbars = as.numeric(c(d.star.mean-d.star.sd,d.star.mean+d.star.sd))
	D.star.sdbars = as.numeric(c(D.star.mean-D.star.sd,D.star.mean+D.star.sd))

	d.hat.sdbars = as.numeric(c(d.hat-d.hat.sd,d.hat+d.hat.sd))
	D.hat.sdbars = as.numeric(c(D.hat-D.hat.sd,D.hat+D.hat.sd))

	###PLOTS####
	
	
	if(doplot==TRUE && sim==TRUE){
		for (j in 1:2){			
			if(j==1){
				d.star.temp.vec = d.star.vec
				d.star.temp.mean = d.star.mean
				d.star.temp.quantiles = d.star.quantiles
				d.star.temp.sdbars = d.star.sdbars
				d.hat.temp = d.hat				
				d.star.temp.median = d.star.median
				d.hat.temp.sdbars = d.hat.sdbars
			
			}else{
				d.star.temp.vec = D.star.vec
				d.star.temp.mean = D.star.mean
				d.star.temp.quantiles = D.star.quantiles
				d.star.temp.sdbars = D.star.sdbars
				d.hat.temp = D.hat				
				d.star.temp.median = D.star.median
				d.hat.temp.sdbars = D.hat.sdbars
			}	
			
			x = seq(min(d.star.temp.vec),max(d.star.temp.vec),length=100)
			#axis(side = 2,at = pretty(hx,n = 10))
			hx = dnorm(x,mean(d.star.temp.vec),sd(d.star.temp.vec))
			plot(density(d.star.temp.vec), xlab = paste(ifelse(j==1,"d*","D*"),"estimate"), main = paste("MC Simulation using method",method,"for n=",n.to.use),sub=paste(no.of.sims,"simulations,", ifelse(resample,"innovations resampled with replacement.","innovations from N(0,1).")),cex.main = 0.7, cex.axis = 0.5,cex.sub = 0.8, las=3, xaxt="n", yaxt = "n",mar = c(5, 4, 4, 4))
			lines(x,hx, lty = 2, col = "blue")
			abline(v=d.star.temp.mean,col = "blue", lty=1)
			abline(v=d.star.temp.median,col = "blue", lty=4)


			abline(v=d.star.temp.quantiles,col = "blue", lty=3)
			abline(v=d.star.temp.sdbars,col = "blue", lty=2)
			

			abline(v=d.hat.temp,col = "black", lty=1)
			abline(v=d.hat.temp.sdbars,col = "black", lty=3)
			
			axis.vals = sort(c(d.star.temp.mean,d.star.temp.quantiles,d.star.temp.sdbars, d.star.temp.median))
			
			hat.axis.vals = sort(c(d.hat.temp,d.hat.temp.sdbars))
			hat.axis.labels = rep("",length(hat.axis.vals))
			hat.axis.labels[which(hat.axis.vals==d.hat.temp)]=paste("dhat",round(d.hat.temp,2));
			hat.axis.labels[which(hat.axis.vals==d.hat.temp.sdbars[1])]=paste("dhat-SD",round(d.hat.temp.sdbars[1],2));
			hat.axis.labels[which(hat.axis.vals==d.hat.temp.sdbars[2])]=paste("dhat+SD",round(d.hat.temp.sdbars[2],2));
			
			axis(side = 1, at= hat.axis.vals, labels = hat.axis.labels, col.axis = "black",cex.axis = 0.7, las = 3)
			xaxis.labels = rep("",length(axis.vals))
			xaxis.labels[which(axis.vals==d.star.temp.mean)]=paste("d*=",round(d.star.temp.mean,2));
			xaxis.labels[which(axis.vals==d.star.temp.quantiles[1])]=paste("d*ci",round(d.star.temp.quantiles[1],2))
			xaxis.labels[which(axis.vals==d.star.temp.quantiles[2])]=paste("d*ci",round(d.star.temp.quantiles[2],2))
			xaxis.labels[which(axis.vals==d.star.temp.sdbars[1])]=paste("d*-SD",round(d.star.temp.sdbars[1],2))
			xaxis.labels[which(axis.vals==d.star.temp.sdbars[2])]=paste("d*+SD",round(d.star.temp.sdbars[2],2))
			#xaxis.labels[which(axis.vals==d.star.temp.median)]=paste("d*-median",round(d.star.temp.median,2))
			
			axis(side=1, at = axis.vals, labels = xaxis.labels, las = 3, cex.axis = 0.7, col.axis = "blue")
			if (j==1){
				dev.new()
				par(new = T)	
			}
		}
	}
    	structure(list(d.hat.param = d.hat, d.hat.se = d.hat.sd, D.hat.param = D.hat, D.hat.se = D.hat.sd, d.star.mean.param = d.star.mean, d.star.se = d.star.sd,
			D.star.mean.param = D.star.mean, D.star.se = D.star.sd))#, d.star.vec = d.star.vec,  D.star.vec = D.star.vec))



}