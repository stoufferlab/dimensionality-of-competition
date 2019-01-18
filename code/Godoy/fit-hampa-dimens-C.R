
# read in Oscar's data
datadir <- "../../data/Godoy/"
hampa <- read.csv(paste0(datadir, "hampa_neigbours_survey.csv"), row.names=1)

# remove the extra columns at the end
hampa <- hampa[,which(!grepl("X",colnames(hampa)))]

# turn the NAs for background into a non-existent species SOLO which will help us when using the glm function (or equivalents) for fitting
levels(hampa$background) <- c(levels(hampa$background),"SOLO")
hampa$background[which(is.na(hampa$background))] <- "SOLO"

# the data should now be primed for analysis; woohoo!

# let's try just one treatment for simplicity in testing regime
hampa <- subset(hampa, treatment=="C")

# run the fitting code
source('fit.machine.R')

# save the fits and write out a table of the AICs
C.optim.lowD <- optim.lowD
save(C.optim.lowD, file="../../results/Godoy/C.optim.lowD.Rdata", ascii=TRUE)

C.AICs <- cbind(0:length(optim.lowD), c(summary(null.fit)$aic, unlist(lapply(optim.lowD, function(x){x$aic}))))
write.table(C.AICs, "../../results/Godoy/godoy.C.AICs.csv", quote=FALSE, col.names=FALSE, sep=" ", row.names=FALSE)
