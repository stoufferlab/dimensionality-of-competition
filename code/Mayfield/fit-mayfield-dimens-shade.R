
# read in the Mayfield data for the Open treatment
which.treatment <- "Shade"
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
Shade.optim.lowD <- optim.lowD
save(Shade.optim.lowD,
	file=paste0("../../results/Mayfield/Shade.optim.D",which.dimension,".r",which.n.random,".Rdata"),
	ascii = TRUE
)
