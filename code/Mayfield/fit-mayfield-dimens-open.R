
# read in the Mayfield data for the Open treatment
which.treatment <- "Open"
source('prep.data.R')

# specify the model family to fit
which.family <- Gamma()

# we get some input from the command line
args <- commandArgs(trailingOnly = TRUE)

# the core dimension for this optimization
which.dimension <- as.integer(args[1])

# which of n random optimizations this is
which.n.random <- as.integer(args[2])

# run the dimensionality fitting code
source('fit.machine.R')

# save the fits and write out a table of the AICs
Open.optim.lowD <- optim.lowD
save(Open.optim.lowD,
	file=paste0("../../results/Mayfield/Open.optim.D",which.dimension,".r",which.n.random,".Rdata"),
	ascii = TRUE
)
# Open.AICs <- cbind(0:length(optim.lowD), c(summary(null.fit)$aic, unlist(lapply(optim.lowD, function(x){x$aic}))))
# write.table(Open.AICs, "../../results/Mayfield/mayfield.Open.AICs.csv", quote=FALSE, col.names=FALSE, sep=" ", row.names=FALSE)
