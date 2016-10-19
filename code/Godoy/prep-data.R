
# read in Oscar's data
dummy <- function(){
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

	return(hampa)
}

hampa <- dummy()
rm(dummy)
