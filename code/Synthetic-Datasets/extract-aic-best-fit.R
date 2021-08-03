
# a few utilities we need below
source('../Utils/polar.transform.R')
source('../Utils/response.effect.from.pars.R')

# which fake data set are we analyzing?
for(which.dataset in seq(25)){

	# read in the synthetic data and assign some common variables
	source('prep.data.R')

	# convenience variable to know where the results files should be located
	resultsdir <- paste0('../../results/Synthetic-Datasets/Synthetic-Dataset-',which.dataset,'/')

	# scrape the shit out of the output files
	best.fits <- lapply(
		seq.int(length(targets)),
		function(d){
			saved.fits <- list.files(
				resultsdir,
				pattern=paste0('Synthetic-Dataset-',which.dataset,'.optim.D',d)
			)
			aics <- unlist(sapply(
				saved.fits,
				function(x){
					load(paste0(resultsdir,x))
					aa <- Synthetic.optim.lowD[[as.character(d)]]$aic
					ifelse(is.null(aa),NA,aa)
				}
			))
		}
	)

	# find the best high D model as a point of comparison for the weights
	best.aics <- unlist(lapply(best.fits, min, na.rm=TRUE))
	bestest <- names(which.min(best.fits[[length(best.aics)]]))
	load(paste0(resultsdir, bestest))
	assign("y", eval(parse(text = paste0("Synthetic.optim.lowD"))))
	fargus.best <- response.effect.from.pars(
		y[[1]]$par,
		targets,
		competitors,
		dimensions=length(best.aics)
	)

	# write out weights for alternative to the AIC figure
	write.table(
		cbind(
			(fargus.best$weights),
			(fargus.best$weights**2) / sum(fargus.best$weights**2),
			cumsum((fargus.best$weights**2) / sum(fargus.best$weights**2))
		),
		file=paste0("../../results/Synthetic-Datasets/Synthetic-Dataset-",which.dataset,".pseudo-rsquared.csv"),
		quote=FALSE,
		col.names=FALSE,
		sep=" ",
		row.names=FALSE
	)

	# troll through the lists and find the lowest at each dimension and the best overall
	best.aics <- unlist(lapply(best.fits, min, na.rm=TRUE))
	best.val <- min(best.aics)
	delta.aics <- best.aics - best.val
	best.d <- min(which(delta.aics <= 2))
	bestest <- which.min(best.fits[[best.d]])
	bestest <- names(best.fits[[best.d]][bestest])

	# reload the best overall model and save the interpretable parameters to a separate unique file
	load(paste0(resultsdir, bestest))
	Synthetic.best <- response.effect.from.pars(
		Synthetic.optim.lowD[[as.character(best.d)]]$par,
		targets,
		competitors,
		dimensions=best.d
	)
	save(Synthetic.best,
		file=paste0("../../results/Synthetic-Datasets/Synthetic-Dataset-",which.dataset,".best.Rdata"),
		ascii = TRUE
	)

	# fit the alpha-centric models as a point of comparison for the AIC figures
	source('./model.comparison.R')

	# write out a table of the AICs
	Synthetic.AICs <- c(gamma.fit.1$aic, best.aics)
	Synthetic.AICs <- cbind(seq.int(length(Synthetic.AICs))-1, Synthetic.AICs)
	write.table(
		Synthetic.AICs,
		paste0("../../results/Synthetic-Datasets/Synthetic-Dataset-",which.dataset,".AICs.csv"),
		quote=FALSE,
		col.names=FALSE,
		sep=" ",
		row.names=FALSE
	)

}
