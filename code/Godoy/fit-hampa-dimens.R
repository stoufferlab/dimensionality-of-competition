
# load in house competition library
library(competition)

# read in Oscar's data
datadir <- "../../data/Godoy/"
hampa <- read.csv(paste0(datadir, "hampa_neigbours_survey.csv"), row.names=1)

# remove the extra columns at the end
hampa <- hampa[,which(!grepl("X",colnames(hampa)))]

# rename target as focal
hampa$focal <- hampa$target
hampa$target <- NULL

# pretend fruits are seeds
hampa$seeds <- hampa$fruits
hampa$fruits <- NULL

# make hampa look like my standardized competition data format
# this first means separating the different competitors into separate columns which include their neighbor abundance
competitors <- as.character(levels(hampa$background))
hampa[,competitors] <- 0
for(i in seq.int(1,nrow(hampa))){
	if(!is.na(hampa[i,'background'])){
		hampa[i,as.character(hampa[i,"background"])] <- hampa[i, "neighbours_number"]
	}
}

# the data should now be primed for analysis; woohoo!

optim.lowD <- list()
for(t in levels(hampa$treatment)){
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
	# tmp <- optimal.traits(hampa.local, dimensions, focals.local, competitors.local, start=tmp$par, method='cobyla', maxeval=10000)
	optim.lowD[[t]][[as.character(dimensions)]] <- tmp

	# now use the lower-dimensional solutions as starting points for the more complex ones
	for(dimensions in seq.int(2,min(length(focals.local),length(competitors.local)))){
		start <- change.dimensions(optim.lowD[[t]][[as.character(dimensions-1)]]$par, focals.local, competitors.local, dimensions-1, dimensions)

		while(TRUE){
			tmp <- optimal.traits(hampa.local, dimensions, focals.local, competitors.local, start=start, method='sbplx', maxeval=1E6)
			if(is.finite(tmp$value)) break
		}

		# tmp <- optimal.traits(hampa.local, dimensions, focals.local, competitors.local, start=tmp$par, method='cobyla', maxeval=10000)
		optim.lowD[[t]][[as.character(dimensions)]] <- tmp
	}

}
