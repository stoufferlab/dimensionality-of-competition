
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
			bics <- unlist(sapply(
				saved.fits,
				function(x){
					load(paste0(resultsdir,x))
					aa <- Goldberg.optim.lowD[[as.character(d)]]$aic
					bic <- aa - 2*(length(Goldberg.optim.lowD[[as.character(d)]]$par)+1) + (length(Goldberg.optim.lowD[[as.character(d)]]$par)+1)*log(length(Goldberg.optim.lowD[[as.character(d)]]$y))
					ifelse(is.null(bic),NA,bic)
				}
			))
		}
	)

	# troll through the lists and find the lowest at each dimension and the best overall
	best.bics <- unlist(lapply(best.fits, min, na.rm=TRUE))
	best.val <- min(best.bics)
	delta.bics <- best.bics - best.val
	best.d <- min(which(delta.bics <= 2))
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
		file=paste0("../../results/Fake-Goldberg/Fake-Goldberg-",which.fake.data,".best.BIC.Rdata"),
		ascii = TRUE
	)

	# fit the classic models as a point of comparison for the BIC figures
	source('../Mayfield/model.comparison.R')

	# write out a table of the BICs
	Goldberg.BICs <- c(gamma.fit.1$bic, best.bics)
	Goldberg.BICs <- cbind(seq.int(length(Goldberg.BICs))-1, Goldberg.BICs)
	write.table(
		Goldberg.BICs,
		paste0("../../results/Fake-Goldberg/fake-goldberg-",which.fake.data,".BICs.csv"),
		quote=FALSE,
		col.names=FALSE,
		sep=" ",
		row.names=FALSE
	)

}
