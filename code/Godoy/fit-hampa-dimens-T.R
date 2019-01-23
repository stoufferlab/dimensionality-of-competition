
# read in Oscar's data
datadir <- "../../data/Godoy/"

# read in the raw data
hampa <- read.csv(paste0(datadir, "hampa_neigbours_survey.csv"), row.names=1)

# remove the extra columns at the end
hampa <- hampa[,which(!grepl("X",colnames(hampa)))]

# turn the NAs for background into a non-existent species SOLO which will help us when using the glm function (or equivalents) for fitting
levels(hampa$background) <- c(levels(hampa$background),"SOLO")
hampa$background[which(is.na(hampa$background))] <- "SOLO"

# the data should now be primed for analysis; woohoo!

# let's try just one treatment for simplicity in testing regime
hampa <- subset(hampa, treatment=="T")

# rename the core data frame
fecundity.data <- hampa

# we need a variable called targets for things to work
targets <- levels(fecundity.data$target)

# we need a variable called competitors for things to work
competitors <- levels(fecundity.data$background)

# we need to know what column has the fecundities
fecundity <- "fruits"

# specify the model family to fit
which.family <- Gamma()

# number of random starts for optimization at each dimension
n.random <- 25

# run the fitting code
source('fit.machine.R')

# save the fits and write out a table of the AICs
T.optim.lowD <- optim.lowD
save(T.optim.lowD, file="../../results/Godoy/T.optim.lowD.Rdata", ascii=TRUE)

# write out an AIC table
T.AICs <- cbind(0:length(optim.lowD), c(summary(null.fit)$aic, unlist(lapply(optim.lowD, function(x){x$aic}))))
write.table(T.AICs, "../../results/Godoy/godoy.T.AICs.csv", quote=FALSE, col.names=FALSE, sep=" ", row.names=FALSE)
