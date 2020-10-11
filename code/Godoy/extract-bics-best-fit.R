
# a few utilities we need below
source('./response.effect.from.pars.R')
source('./polar.transform.R')

for(which.treatment in c('C','T')){

	# read in the mayfield data for the specified treatment and assign some common variables
	source('prep.data.R')

	# scrape the shit out of the output files
	best.fits <- lapply(
		seq.int(length(targets)),
		function(d){
			saved.fits <- list.files('../../results/Godoy/', pattern=paste0(which.treatment,'.optim.D',d))
			bics <- unlist(sapply(
				saved.fits,
				function(x){
					q <- try(load(paste0('../../results/Godoy/',x)))
					if(inherits(q, "try-error")){
						message(x)
						return(NA)
					}else{
						assign("y", eval(parse(text = paste0(which.treatment,".optim.lowD"))))
						aa <- y[[1]]$aic
						bic <- aa - 2*(length(y[[1]]$par)+1) + (length(y[[1]]$par)+1)*log(length(y[[1]]$y))
						ifelse(is.null(bic),NA,bic)
					}
				}
			))
		}
	)

	# troll through the lists and find the lowest at each dimension and the best overall
	best.bics <- unlist(lapply(best.fits, min, na.rm=TRUE))
	
	# find the best high D model as a point of comparison for the weights
	bestest <- names(which.min(best.fits[[length(best.bics)]]))
	load(paste0('../../results/Godoy/', bestest))
	assign("y", eval(parse(text = paste0(which.treatment,".optim.lowD"))))
	fargus.best <- response.effect.from.pars(
		y[[1]]$par,
		targets,
		competitors,
		dimensions=length(best.bics),
		godoy=TRUE
	)

	# search for the best overall model
	best.val <- min(best.bics)
	delta.bics <- best.bics - best.val
	best.d <- min(which(delta.bics <= 2))
	bestest <- which.min(best.fits[[best.d]])
	bestest <- names(best.fits[[best.d]][bestest])

	# reload the best overall model and save the interpretable parameters to a separate unique file
	load(paste0('../../results/Godoy/', bestest))
	assign("y", eval(parse(text = paste0(which.treatment,".optim.lowD"))))
	Godoy.best <- response.effect.from.pars(
		y[[1]]$par,
		targets,
		competitors,
		dimensions=best.d,
		godoy=TRUE
	)
	save(Godoy.best,
		file=paste0("../../results/Godoy/godoy.",which.treatment,".best.BIC.Rdata"),
		ascii = TRUE
	)

	# write out weights for alternative to the BIC figure
	write.table(
		cbind(
			(fargus.best$weights),
			(fargus.best$weights**2) / sum(fargus.best$weights**2),
			cumsum((fargus.best$weights**2) / sum(fargus.best$weights**2))
		),
		file=paste0("../../results/Godoy/godoy.",which.treatment,".pseudo-rsquared.BIC.csv"),
		quote=FALSE,
		col.names=FALSE,
		sep=" ",
		row.names=FALSE
	)

	# fit the classic models as a point of comparison for the AIC figures
	source('./model.comparison.R')

	# write out a table of the BICs
	Godoy.BICs <- c(BIC(gamma.fit.1), best.bics)
	Godoy.BICs <- cbind(seq.int(length(Godoy.BICs))-1, Godoy.BICs)
	write.table(
		Godoy.BICs,
		paste0("../../results/Godoy/godoy.",which.treatment,".BICs.csv"),
		quote=FALSE,
		col.names=FALSE,
		sep=" ",
		row.names=FALSE
	)
}
