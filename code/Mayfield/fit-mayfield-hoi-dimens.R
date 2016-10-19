
# load in house competition library
library(competition)

# read in Margie's HOI data
load('../../data/Mayfield-HOI/mayfield.Rdata')

# rename the response variable
mayfield$seeds <- mayfield$total.seed
mayfield$total.seed <- NULL

# identify competitors species
competitors <- colnames(mayfield)
competitors <- competitors[which(!competitors %in% c('focal','seeds','site','quadrat'))]
competitors <- competitors[which(colSums(mayfield[,competitors])!=0)]

# identify focal species
mayfield$focal <- droplevels(mayfield$focal)
focals <- as.character(levels(droplevels(mayfield$focal)))

# get rid of columns that bore me
# mayfield <- mayfield[,c('seeds','focal',competitors,'datasource','plotID','block','light')]

###############################################################################
###############################################################################
### Attempt things just by optimizing the traits
###############################################################################
###############################################################################

optim.lowD <- list()
for(t in levels(mayfield$site)){
	optim.lowD[[t]] <- list()

	# let's try just one treatment for simplicity in testing regime
	mayfield.local <- subset(mayfield, site==t)
	mayfield.local$focal <- droplevels(mayfield.local$focal)
	focals.local <- levels(droplevels(mayfield.local$focal))
	competitors.local <- competitors[which(colSums(mayfield.local[,competitors])>0)]

	# solve the zero- and one-dimensional problems first
	dimensions <- 0
	tmp <- optimal.traits(mayfield.local, dimensions, focals.local, competitors.local, method='sbplx', maxeval=1E6)
	# tmp <- optimal.traits(mayfield.local, dimensions, focals.local, competitors.local, start=tmp$par, method='cobyla', maxeval=10000)
	optim.lowD[[t]][[as.character(dimensions)]] <- tmp

	dimensions <- 1
	while(TRUE){
		tmp <- optimal.traits(mayfield.local, dimensions, focals.local, competitors.local, method='sbplx', maxeval=1E6)
		if(is.finite(tmp$value)) break
	}
	# tmp <- optimal.traits(mayfield.local, dimensions, focals.local, competitors.local, start=tmp$par, method='cobyla', maxeval=10000)
	optim.lowD[[t]][[as.character(dimensions)]] <- tmp

	# now use the lower-dimensional solutions as starting points for the more complex ones
	for(dimensions in seq.int(2,min(length(focals.local),length(competitors.local)))){
		start <- change.dimensions(optim.lowD[[t]][[as.character(dimensions-1)]]$par, focals.local, competitors.local, dimensions-1, dimensions)

		while(TRUE){
			tmp <- optimal.traits(mayfield.local, dimensions, focals.local, competitors.local, start=start, method='sbplx', maxeval=1E6)
			if(is.finite(tmp$value)) break
		}

		# tmp <- optimal.traits(mayfield.local, dimensions, focals.local, competitors.local, start=tmp$par, method='cobyla', maxeval=10000)
		optim.lowD[[t]][[as.character(dimensions)]] <- tmp
	}

}

save(optim.lowD, file='mayfield-hoi-optim.lowD.Rdata')