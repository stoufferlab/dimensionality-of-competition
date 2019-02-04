
# read in the data for the specified treatment
which.treament <- "C"
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
C.optim.lowD <- optim.lowD
save(C.optim.lowD,
	file=paste0("../../results/Godoy/C.optim.D",which.dimension,".r",which.n.random,".Rdata"),
	ascii = TRUE
)

# C.AICs <- cbind(0:length(optim.lowD), c(summary(null.fit)$aic, unlist(lapply(optim.lowD, function(x){x$aic}))))
# write.table(C.AICs, "../../results/Godoy/godoy.C.AICs.csv", quote=FALSE, col.names=FALSE, sep=" ", row.names=FALSE)
