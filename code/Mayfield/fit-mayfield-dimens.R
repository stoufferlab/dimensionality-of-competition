
# read in Margie's data
datadir <- "../../data/Mayfield/"
mayfield <- read.csv(paste0(datadir, "nhood data wTF.csv"))

# remove NA and 0 
# TO-DO: ask Margie about zero fecundities
mayfield <- subset(mayfield, !is.na(seeds) & !(seeds==0))

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
source('dev.fun.R')
source('get.alphas.from.model.R')
source('glm.coefs.from.traits.R')
source('change.dimensions.R')
source('optimal.traits.R')

optim.lowD <- list()
for(t in levels(mayfield$light)){
	optim.lowD[[t]] <- list()

	# let's try just one treatment for simplicity in testing regime
	mayfield.local <- subset(mayfield, (light=="Shade" | light == "shade"))
	focals.local <- levels(droplevels(mayfield.local$focal))
	competitors.local <- competitors[which(colSums(mayfield.local[,competitors])>0)]

	dimensions <- 1
	optim.lowD[[t]][[dimensions]] <- optimal.traits(mayfield.local, dimensions, focals.local, competitors.local, ftol_rel=1E-8, maxeval=5E6)

	for(dimensions in seq.int(2,10)){
		start <- change.dimensions(optim.lowD[[t]][[dimensions-1]]$par, focals, competitors, dimensions-1, dimensions)
		optim.lowD[[t]][[dimensions]] <- optimal.traits(mayfield.local, dimensions, focals.local, competitors.local, start=start, ftol_rel=1E-8, maxeval=5E6)
	}

}
