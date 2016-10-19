
library(competition)

dummy <- function(hampa){
	competitors <- colnames(hampa)
	competitors <- competitors[which(!competitors %in% c('date','code','plot','treatment','indiv','background','neighbours_number','unviable_seeds','seeds','focal'))]
	competitors <- competitors[which(colSums(hampa[,competitors])!=0)]

	optim.lowD <- list()
	for(t in levels(hampa$treatment)[1]){
		optim.lowD[[t]] <- list()

		hampa.local <- subset(hampa, treatment==t)
		focals.local <- levels(droplevels(hampa.local$focal))
		competitors.local <- competitors[which(colSums(hampa.local[,competitors])>0)]

		# solve the zero- and one-dimensional problems first
		dimensions <- 0
		tmp <- optimal.traits(hampa.local, dimensions, focals.local, competitors.local, method='sbplx', maxeval=1E6)
		# tmp <- optimal.traits(hampa.local, dimensions, focals.local, competitors.local, start=tmp$par, method='cobyla', maxeval=10000)
		optim.lowD[[t]][[as.character(dimensions)]] <- tmp

		dimensions <- 1
		while(TRUE){
			tmp <- optimal.traits(hampa.local, dimensions, focals.local, competitors.local, method='sbplx', maxeval=1E6)
			if(is.finite(tmp$value)) break
		}
		optim.lowD[[t]][[as.character(dimensions)]] <- tmp

		# now use the lower-dimensional solutions as starting points for the more complex ones
		for(dimensions in seq.int(2,min(length(focals.local),length(competitors.local)))){
			start <- change.dimensions(optim.lowD[[t]][[as.character(dimensions-1)]]$par, focals.local, competitors.local, dimensions-1, dimensions)

			while(TRUE){
				tmp <- optimal.traits(hampa.local, dimensions, focals.local, competitors.local, start=start, method='sbplx', maxeval=1E6)
				if(is.finite(tmp$value)) break
			}
			optim.lowD[[t]][[as.character(dimensions)]] <- tmp
		}

	}

	return(optim.lowD)
}

optim.lowD <- dummy(hampa)
rm(dummy)