
# read in Margie's data
datadir <- "../../data/Mayfield/"
mayfield <- read.csv(paste0(datadir, "nhood data wTF.csv"))

# relabel "focal" as "target"
cc <- colnames(mayfield)
cc[5] <- "target"
colnames(mayfield) <- cc

# these columns correspond to neighbor densities
competitors <- colnames(mayfield)[8:39]

# remove NA and 0 
# WARNING: ask Margie about zero fecundities
mayfield <- subset(mayfield, !is.na(seeds) & !(seeds==0))

# let's try just one treatment for simplicity in testing regime
mayfield <- subset(mayfield, light=="Shade")

# the data should now be primed for analysis; woohoo!

# run the model comparison code
source('model.comparison.R')

# we need to remove species that don't appear in this treatment when we fit the low-dimensional versions
competitors <- names(which(colSums(mayfield[,competitors])>0))

# we need a variable called targets for things to work
targets <- levels(mayfield$target)

# we need to know what column has the fecundities
fecundity <- "seeds"

# specify the model family to fit
which.family <- Gamma()

# rename the core data frame
fecundity.data <- mayfield

# we get some input from the command line
args <- commandArgs(trailingOnly = TRUE)

# the core dimension for this optimization
which.dimension <- as.integer(args[1])

# which of n random optimizations this is
which.n.random <- as.integer(args[2])

# run the dimensionality fitting code
source('fit.machine.R')

# save the fits and write out a table of the AICs
Shade.optim.lowD <- optim.lowD
# save(Shade.optim.lowD, file="../../results/Mayfield/Shade.optim.lowD.Rdata", ascii=TRUE)
save(Shade.optim.lowD,
	file=paste0("../../results/Mayfield/Shade.optim.D",which.dimension,".r",which.n.random,".Rdata"),
	ascii = TRUE
)

# Shade.AICs <- cbind(0:length(optim.lowD), c(summary(null.fit)$aic, unlist(lapply(optim.lowD, function(x){x$aic}))))
# write.table(Shade.AICs, "../../results/Mayfield/mayfield.Shade.AICs.csv", quote=FALSE, col.names=FALSE, sep=" ", row.names=FALSE)
