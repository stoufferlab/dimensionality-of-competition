
# read in the data for the specified treatment
which.treament <- "T"
source('prep.data.R')

# specify the model family to fit
which.family <- Gamma()

# we get some input from the command line
args <- commandArgs(trailingOnly = TRUE)

# the core dimension for this optimization
which.dimension <- as.integer(args[1])

# which of n random optimizations this is
which.n.random <- as.integer(args[2])

# # run the model comparison code
# source('model.comparison.R')

# run the fitting code
source('fit.machine.R')

# save the fits and write out a table of the AICs
T.optim.lowD <- optim.lowD
save(T.optim.lowD,
	file=paste0("../../results/Godoy/T.optim.D",which.dimension,".r",which.n.random,".Rdata"),
	ascii = TRUE
)

# # write out an AIC table
# T.AICs <- cbind(0:length(optim.lowD), c(summary(null.fit)$aic, unlist(lapply(optim.lowD, function(x){x$aic}))))
# write.table(T.AICs, "../../results/Godoy/godoy.T.AICs.csv", quote=FALSE, col.names=FALSE, sep=" ", row.names=FALSE)
