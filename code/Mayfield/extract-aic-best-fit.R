
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
			aics <- unlist(sapply(
				saved.fits,
				function(x){
					load(paste0('../../results/Mayfield/',x))
					assign("y", eval(parse(text = paste0(which.treatment,".optim.lowD"))))
					aa <- y[[1]]$aic
					ifelse(is.null(aa),NA,aa)
				}
			))
		}
	)

	# troll through the lists and find the lowest at each dimension and the best overall
	best.aics <- unlist(lapply(best.fits, min, na.rm=TRUE))
	best.d <- which.min(best.aics)
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
		file=paste0("../../results/Mayfield/mayfield.",which.treatment,".best.Rdata"),
		ascii = TRUE
	)

	# fit the classic models as a point of comparison for the AIC figures
	source('../Mayfield/model.comparison.R')

	# write out a table of the AICs
	Mayfield.AICs <- c(gamma.fit.0$aic, best.aics)
	Mayfield.AICs <- cbind(seq.int(length(Mayfield.AICs))-1, Mayfield.AICs)
	write.table(
		Mayfield.AICs,
		paste0("../../results/Mayfield/mayfield.",which.treatment,".AICs.csv"),
		quote=FALSE,
		col.names=FALSE,
		sep=" ",
		row.names=FALSE
	)
}
