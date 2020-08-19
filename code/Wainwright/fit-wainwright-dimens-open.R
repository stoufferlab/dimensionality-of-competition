
# read in the Wainwright data for the Open treatment
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
source('../Mayfield/fit.machine.R')

# save the fits and write out a table of the AICs
Open.optim.lowD <- optim.lowD
save(Open.optim.lowD,
	file=paste0("../../results/Wainwright/Open.optim.D",which.dimension,".r",which.n.random,".Rdata"),
	ascii = TRUE
)
