
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
			aics <- unlist(sapply(
				saved.fits,
				function(x){
					q <- try(load(paste0('../../results/Godoy/',x)))
					if(inherits(q, "try-error")){
						message(x)
						return(NA)
					}else{
						assign("y", eval(parse(text = paste0(which.treatment,".optim.lowD"))))
						aa <- y[[1]]$aic
						ifelse(is.null(aa),NA,aa)
					}
				}
			))
		}
	)

	# troll through the lists and find the lowest at each dimension and the best overall
	best.aics <- unlist(lapply(best.fits, min, na.rm=TRUE))
	
	# find the best high D model as a point of comparison for the weights
	bestest <- names(which.min(best.fits[[length(best.aics)]]))
	load(paste0('../../results/Godoy/', bestest))
	assign("y", eval(parse(text = paste0(which.treatment,".optim.lowD"))))
	fargus.best <- response.effect.from.pars(
		y[[1]]$par,
		targets,
		competitors,
		dimensions=length(best.aics),
		godoy=TRUE
	)

	# search for the best overall model
	best.val <- min(best.aics)
	delta.aics <- best.aics - best.val
	best.d <- min(which(delta.aics <= 2))
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
		file=paste0("../../results/Godoy/godoy.",which.treatment,".best.Rdata"),
		ascii = TRUE
	)

	# write out weights for alternative to the AIC figure
	write.table(
		cbind(
			sqrt(fargus.best$weights),
			sqrt(fargus.best$weights) / sum(sqrt(fargus.best$weights)),
			cumsum(sqrt(fargus.best$weights) / sum(sqrt(fargus.best$weights)))
		),
		file=paste0("../../results/Godoy/godoy.",which.treatment,".full.weights.csv"),
		quote=FALSE,
		col.names=FALSE,
		sep=" ",
		row.names=FALSE
	)

	# write out a pseudo-rsquared table for use in the paper
	write.table(
		cbind(
			cumsum(sqrt(Godoy.best$weights) / sum(sqrt(fargus.best$weights))),
			sqrt(Godoy.best$weights) / sum(sqrt(fargus.best$weights))
		),
		file=paste0("../../results/Godoy/godoy.",which.treatment,".pseudo-rsquared.csv"),
		quote=FALSE,
		col.names=FALSE,
		sep=" ",
		row.names=FALSE
	)

	# fit the classic models as a point of comparison for the AIC figures
	source('./model.comparison.R')

	# write out a table of the AICs
	Godoy.AICs <- c(gamma.fit.1$aic, best.aics)
	Godoy.AICs <- cbind(seq.int(length(Godoy.AICs))-1, Godoy.AICs)
	write.table(
		Godoy.AICs,
		paste0("../../results/Godoy/godoy.",which.treatment,".AICs.csv"),
		quote=FALSE,
		col.names=FALSE,
		sep=" ",
		row.names=FALSE
	)
}
