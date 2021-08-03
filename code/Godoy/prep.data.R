
# read in Oscar's data
hampa <- read.csv("../../data/Spain/hampa_neigbours_survey.csv", row.names=1)

# remove the extra columns at the end
hampa <- hampa[,which(!grepl("X",colnames(hampa)))]

# select only the treatment of interest
hampa <- subset(hampa, treatment==which.treatment)

# # turn the NAs for background into a non-existent species SOLO which will help us when using the glm function (or equivalents) for fitting
# levels(hampa$background) <- c(levels(hampa$background),"SOLO")
# hampa$background[which(is.na(hampa$background))] <- "SOLO"

# reshape hampa into a common-format for analysis
fecundity.data <- data.frame(
	data=hampa$date,
	code=hampa$code,
	plot=hampa$plot,
	treatment=hampa$treatment,
	indiv=hampa$indiv,
	target=hampa$target,
	fruits=hampa$fruits
)

# add empty columns for the different neighbors
for(neighbor in levels(hampa$background)){
	fecundity.data[,neighbor] <- 0
}

# populate neighbors with their abundances
for(i in seq.int(nrow(hampa))){
	if(!is.na(hampa$background[i])){
		fecundity.data[i,as.character(hampa$background[i])] <- hampa$neighbours_number[i]
	}
}

# we need a variable called targets for things to work
targets <- levels(fecundity.data$target)

# we need a variable called competitors for things to work
competitors <- levels(hampa$background)

# we need to know what column has the fecundities
fecundity <- "fruits"

# remove unused variables
rm(hampa, i, neighbor)
