
# a few utilities we need below
source('../Godoy/response.effect.from.pars.R')
source('../Godoy/polar.transform.R')

for(which.treatment in c('Open','Shade')){

	# read in the wainwright data for the specified treatment and assign some common variables
	source('prep.data.R')

	# scrape the shit out of the output files
	best.fits <- lapply(
		seq.int(length(targets)),
		function(d){
			saved.fits <- list.files('../../results/Wainwright/', pattern=paste0(which.treatment,'.optim.D',d,'[.]'))
			aics <- unlist(sapply(
				saved.fits,
				function(x){
					load(paste0('../../results/Wainwright/',x))
					assign("y", eval(parse(text = paste0(which.treatment,".optim.lowD"))))
					aa <- y[[1]]$aic
					ifelse(is.null(aa),NA,aa)
				}
			))
		}
	)

	# troll through the lists and find the lowest at each dimension and the best overall
	best.aics <- unlist(lapply(best.fits, min, na.rm=TRUE))

	# find the best high D model as a point of comparison for the weights
	bestest <- names(which.min(best.fits[[length(best.aics)]]))
	load(paste0('../../results/Wainwright/', bestest))
	assign("y", eval(parse(text = paste0(which.treatment,".optim.lowD"))))
	fargus.best <- response.effect.from.pars(
		y[[1]]$par,
		targets,
		competitors,
		dimensions=length(best.aics),
		godoy=FALSE
	)

	# find the best overall model
	best.val <- min(best.aics)
	delta.aics <- best.aics - best.val
	best.d <- min(which(delta.aics <= 2))
	bestest <- which.min(best.fits[[best.d]])
	bestest <- names(best.fits[[best.d]][bestest])

	# reload the best overall model and save the interpretable parameters to a separate unique file
	load(paste0('../../results/Wainwright/', bestest))
	assign("y", eval(parse(text = paste0(which.treatment,".optim.lowD"))))
	Wainwright.best <- response.effect.from.pars(
		y[[1]]$par,
		targets,
		competitors,
		dimensions=best.d,
		godoy=FALSE
	)
	save(Wainwright.best,
		file=paste0("../../results/Wainwright/wainwright.",which.treatment,".best.Rdata"),
		ascii = TRUE
	)

	# write out weights for alternative to the AIC figure
	write.table(
		cbind(
			(fargus.best$weights),
			(fargus.best$weights**2) / sum(fargus.best$weights**2),
			cumsum((fargus.best$weights**2) / sum(fargus.best$weights**2))
		),
		file=paste0("../../results/Wainwright/wainwright.",which.treatment,".SVD-rsquared.csv"),
		quote=FALSE,
		col.names=FALSE,
		sep=" ",
		row.names=FALSE
	)

	# fit the classic models as a point of comparison for the AIC figures
	source('../Mayfield/model.comparison.R')

	# write out a table of the AICs
	Wainwright.AICs <- c(gamma.fit.1$aic, best.aics)
	Wainwright.AICs <- cbind(seq.int(length(Wainwright.AICs))-1, Wainwright.AICs)
	write.table(
		Wainwright.AICs,
		paste0("../../results/Wainwright/wainwright.",which.treatment,".AICs.csv"),
		quote=FALSE,
		col.names=FALSE,
		sep=" ",
		row.names=FALSE
	)

	# compare deviance of lowest AIC models to deviance of "null" model
	best.per.d <- sapply(best.fits, function(x) names(x)[which.min(x)])
	best.deviances <- sapply(
		best.per.d,
		function(x){
			load(paste0('../../results/Wainwright/', x))
			assign("y", eval(parse(text = paste0(which.treatment,".optim.lowD"))))
			return(y[[1]]$value)
		}
	)
	write.table(
		cbind(
			best.deviances,
			1 - best.deviances/gamma.fit.1$deviance
		),
		file=paste0("../../results/Wainwright/wainwright.",which.treatment,".pseudo-rsquared.csv"),
		quote=FALSE,
		col.names=FALSE,
		sep=" ",
		row.names=FALSE
	)

	# write out the alpha coefficients for comparison plots elsewhere
	source('../Mayfield/get.alphas.from.model.R')
	coef.summary <- summary(gamma.fit.3)$coef
	alphas.mean <- get.alphas.from.model(coef.summary[,"Estimate"],targets,competitors)
	alphas.se <- get.alphas.from.model(coef.summary[,"Std. Error"],targets,competitors)

	alphas <- as.data.frame.table(alphas.mean)
	colnames(alphas) <- c("row","col","alpha")
	alphas[,"alphas.se$se"] <- NA #alphas.se[,3] #DEBUG unscaling of SE by lambda is incorrect
	write.table(
		alphas,
		paste0("../../results/Wainwright/wainwright.",which.treatment,".alphas.orig.csv"),
		quote=FALSE
	)
}
