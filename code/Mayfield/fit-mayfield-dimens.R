
# load in house competition library
library(competition)

# read in Margie's data
datadir <- "../../data/Mayfield/"
mayfield <- read.csv(paste0(datadir, "nhood data wTF.csv"))

# remove NA and 0 
# TO-DO: ask Margie about zero fecundities
mayfield <- subset(mayfield, !is.na(seeds) & !(seeds==0))

mayfield[which(mayfield$light=="Open"),"light"] <- "open"
mayfield[which(mayfield$light=="Shade"),"light"] <- "shade"

mayfield$light <- droplevels(mayfield$light)

# # turn the NAs for background into a non-existent species SOLO which will help us when using the glm function (or equivalents) for fitting
# levels(mayfield$background) <- c(levels(mayfield$background),"SOLO")
# mayfield$background[which(is.na(mayfield$background))] <- "SOLO"

# identify competitors species
competitors <- colnames(mayfield)
competitors <- competitors[which(!competitors %in% c('datasource','plotID','block','light','focal','seeds','competition'))]
competitors <- competitors[which(colSums(mayfield[,competitors])!=0)]

# identify focal species
focals <- as.character(levels(mayfield$focal))

# get rid of columns that bore me
mayfield <- mayfield[,c('seeds','focal',competitors,'datasource','plotID','block','light')]

###############################################################################
###############################################################################
### Attempt things just by optimizing the traits
###############################################################################
###############################################################################

optim.lowD <- list()
for(t in levels(mayfield$light)){
	optim.lowD[[t]] <- list()

	# let's try just one treatment for simplicity in testing regime
	mayfield.local <- subset(mayfield, light==t)
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
