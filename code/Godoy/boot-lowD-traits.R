
library(competition)

dummy <- function(hampa, optim.lowD){
	competitors <- colnames(hampa)
	competitors <- competitors[which(!competitors %in% c('date','code','plot','treatment','indiv','background','neighbours_number','unviable_seeds','seeds','focal'))]
	competitors <- competitors[which(colSums(hampa[,competitors])!=0)]

	boot.lowD <- list()
	for(t in names(optim.lowD)){
		boot.lowD[[t]] <- list()

		hampa.local <- subset(hampa, treatment==t)
		focals.local <- levels(droplevels(hampa.local$focal))
		competitors.local <- competitors[which(colSums(hampa.local[,competitors])>0)]

		for(dimensions in as.integer(names(optim.lowD[[t]]))){
			boot.lowD[[t]][[as.character(dimensions)]] <- list()
			# boot.lowD[[t]][[as.character(dimensions)]][[1]] <- optim.lowD[[t]][[as.character(dimensions)]]
			for(czech in seq.int(1,100)){
				while(TRUE){
					hampa.local2 <- hampa.local[sample(rownames(hampa.local),nrow(hampa.local),replace=TRUE),]
					tmp <- optimal.traits(hampa.local2, dimensions, focals.local, competitors.local, start=optim.lowD[[t]][[as.character(dimensions)]]$par, method='sbplx', maxeval=1E6)
					if(is.finite(tmp$value)) break
				}
				# tmp <- optimal.traits(hampa.local, dimensions, focals.local, competitors.local, start=tmp$par, method='cobyla', maxeval=10000)
				boot.lowD[[t]][[as.character(dimensions)]][[czech]] <- tmp
			}
		}

	}

	return(boot.lowD)
}

boot.lowD <- dummy(hampa, optim.lowD)
rm(dummy)