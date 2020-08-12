
# a few utilities we need below
source('../Godoy/response.effect.from.pars.R')
source('../Godoy/polar.transform.R')

for(which.treatment in c('Open','Shade')){

	# read in the mayfield data for the specified treatment and assign some common variables
	source('prep.data.R')

	# scrape the shit out of the output files
	best.fits <- lapply(
		seq.int(length(targets)),
		function(d){
			saved.fits <- list.files('../../results/Mayfield/', pattern=paste0(which.treatment,'.optim.D',d))
			bics <- unlist(sapply(
				saved.fits,
				function(x){
					load(paste0('../../results/Mayfield/',x))
					assign("y", eval(parse(text = paste0(which.treatment,".optim.lowD"))))
					aa <- y[[1]]$aic
					bic <- aa - 2*(length(y[[1]]$par)+1) + (length(y[[1]]$par)+1)*log(length(y[[1]]$y))
					ifelse(is.null(bic),NA,bic)
				}
			))
		}
	)

	# troll through the lists and find the lowest at each dimension and the best overall
	best.bics <- unlist(lapply(best.fits, min, na.rm=TRUE))
	
	# find the best high D model as a point of comparison for the weights
	bestest <- names(which.min(best.fits[[length(best.bics)]]))
	load(paste0('../../results/Mayfield/', bestest))
	assign("y", eval(parse(text = paste0(which.treatment,".optim.lowD"))))
	fargus.best <- response.effect.from.pars(
		y[[1]]$par,
		targets,
		competitors,
		dimensions=length(best.bics),
		godoy=FALSE
	)

	# search for the best overall model
	best.val <- min(best.bics)
	delta.bics <- best.bics - best.val
	best.d <- min(which(delta.bics <= 2))
	bestest <- which.min(best.fits[[best.d]])
	bestest <- names(best.fits[[best.d]][bestest])

	# reload the best overall model and save the interpretable parameters to a separate unique file
	load(paste0('../../results/Mayfield/', bestest))
	assign("y", eval(parse(text = paste0(which.treatment,".optim.lowD"))))
	Mayfield.best <- response.effect.from.pars(
		y[[1]]$par,
		targets,
		competitors,
		dimensions=best.d,
		godoy=FALSE
	)
	save(Mayfield.best,
		file=paste0("../../results/Mayfield/mayfield.",which.treatment,".best.BIC.Rdata"),
		ascii = TRUE
	)

	# write out weights for alternative to the BIC figure
	write.table(
		cbind(
			sqrt(fargus.best$weights),
			sqrt(fargus.best$weights) / sum(sqrt(fargus.best$weights)),
			cumsum(sqrt(fargus.best$weights) / sum(sqrt(fargus.best$weights)))
		),
		file=paste0("../../results/Mayfield/mayfield.",which.treatment,".full.weights.BIC.csv"),
		quote=FALSE,
		col.names=FALSE,
		sep=" ",
		row.names=FALSE
	)

	# write out a pseudo-rsquared table for use in the paper
	write.table(
		cbind(
			cumsum(sqrt(Mayfield.best$weights) / sum(sqrt(fargus.best$weights))),
			sqrt(Mayfield.best$weights) / sum(sqrt(fargus.best$weights))
		),
		file=paste0("../../results/Mayfield/mayfield.",which.treatment,".pseudo-rsquared.BIC.csv"),
		quote=FALSE,
		col.names=FALSE,
		sep=" ",
		row.names=FALSE
	)	

	# fit the classic models as a point of comparison for the BIC figures
	source('../Mayfield/model.comparison.R')

	# write out a table of the BICs
	Mayfield.BICs <- c(BIC(gamma.fit.1), best.bics)
	Mayfield.BICs <- cbind(seq.int(length(Mayfield.BICs))-1, Mayfield.BICs)
	write.table(
		Mayfield.BICs,
		paste0("../../results/Mayfield/mayfield.",which.treatment,".BICs.csv"),
		quote=FALSE,
		col.names=FALSE,
		sep=" ",
		row.names=FALSE
	)
}
