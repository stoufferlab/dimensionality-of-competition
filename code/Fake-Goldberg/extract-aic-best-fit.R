
# a few utilities we need below
source('../Godoy/response.effect.from.pars.R')
source('../Godoy/polar.transform.R')

# which fake data set are we analyzing?
for(which.fake.data in seq(25)){

	# read in the goldberg data and assign some common variables
	source('prep.data.R')

	# convenience variable to know where the results files should be located
	resultsdir <- paste0('../../results/Fake-Goldberg/Fake-Goldberg-',which.fake.data,'/')

	# scrape the shit out of the output files
	best.fits <- lapply(
		seq.int(length(targets)),
		function(d){
			saved.fits <- list.files(
				resultsdir,
				pattern=paste0('Fake-Goldberg-',which.fake.data,'.optim.D',d)
			)
			aics <- unlist(sapply(
				saved.fits,
				function(x){
					load(paste0(resultsdir,x))
					aa <- Goldberg.optim.lowD[[as.character(d)]]$aic
					ifelse(is.null(aa),NA,aa)
				}
			))
		}
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
	Goldberg.best <- response.effect.from.pars(
		Goldberg.optim.lowD[[as.character(best.d)]]$par,
		targets,
		competitors,
		dimensions=best.d,
		godoy=FALSE
	)
	save(Goldberg.best,
		file=paste0("../../results/Fake-Goldberg/Fake-Goldberg-",which.fake.data,".best.Rdata"),
		ascii = TRUE
	)

	# fit the classic models as a point of comparison for the AIC figures
	source('../Mayfield/model.comparison.R')

	# write out a table of the AICs
	Goldberg.AICs <- c(gamma.fit.1$aic, best.aics)
	Goldberg.AICs <- cbind(seq.int(length(Goldberg.AICs))-1, Goldberg.AICs)
	write.table(
		Goldberg.AICs,
		paste0("../../results/Fake-Goldberg/fake-goldberg-",which.fake.data,".AICs.csv"),
		quote=FALSE,
		col.names=FALSE,
		sep=" ",
		row.names=FALSE
	)

}
