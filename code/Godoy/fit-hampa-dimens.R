
source('../Mayfield/dev.fun.R')
source('../Mayfield/get.alphas.from.model.R')
source('../Mayfield/glm.coefs.from.traits.R')
source('../Mayfield/change.dimensions.R')
source('../Mayfield/optimal.traits.R')

# read in Oscar's data
datadir <- "../../data/Godoy/"
hampa <- read.csv(paste0(datadir, "hampa_neigbours_survey.csv"), row.names=1)

# remove the extra columns at the end
hampa <- hampa[,which(!grepl("X",colnames(hampa)))]

# rename target as focal
hampa$focal <- hampa$target
hampa$target <- NULL

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

	dimensions <- 1
	optim.lowD[[t]][[dimensions]] <- optimal.traits(hampa.local, dimensions, focals.local, competitors.local, ftol_rel=1E-8, maxeval=5E6)

	for(dimensions in seq.int(2,10)){
		start <- change.dimensions(optim.lowD[[t]][[dimensions-1]]$par, focals, competitors, dimensions-1, dimensions)
		optim.lowD[[t]][[dimensions]] <- optimal.traits(hampa.local, dimensions, focals.local, competitors.local, start=start, ftol_rel=1E-8, maxeval=5E6)
	}

}
