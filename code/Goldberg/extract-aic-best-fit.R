
# a few utilities we need below
source('../Godoy/response.effect.from.pars.R')
source('../Godoy/polar.transform.R')

# read in the goldberg data and assign some common variables
source('prep.data.R')

# scrape the shit out of the output files
best.fits <- lapply(
	seq.int(length(targets)),
	function(d){
		saved.fits <- list.files('../../results/Goldberg/', pattern=paste0('Goldberg.optim.D',d))
		aics <- unlist(sapply(
			saved.fits,
			function(x){
				load(paste0('../../results/Goldberg/',x))
				aa <- Goldberg.optim.lowD[[as.character(d)]]$aic
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
load(paste0('../../results/Goldberg/', bestest))
Goldberg.best <- response.effect.from.pars(
	Goldberg.optim.lowD[[as.character(best.d)]]$par,
	targets,
	competitors,
	dimensions=best.d,
	godoy=FALSE
)
save(Goldberg.best,
	file=paste0("../../results/Goldberg/Goldberg.best.Rdata"),
	ascii = TRUE
)

# fit the classic models as a point of comparison for the AIC figures
source('../Mayfield/model.comparison.R')

# write out a table of the AICs
Goldberg.AICs <- c(gamma.fit.0$aic, best.aics)
Goldberg.AICs <- cbind(seq.int(length(Goldberg.AICs))-1, Goldberg.AICs)
write.table(
	Goldberg.AICs,
	"../../results/Goldberg/goldberg.AICs.csv",
	quote=FALSE,
	col.names=FALSE,
	sep=" ",
	row.names=FALSE
)
