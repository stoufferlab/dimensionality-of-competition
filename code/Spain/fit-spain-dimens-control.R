
# read in the data for the specified treatment
which.treatment <- "C"
source('prep.data.R')

# specify the model family to fit
which.family <- poisson(link="inverse")

# we get some input from the command line
args <- commandArgs(trailingOnly = TRUE)

# the core dimension for this optimization
dimensions <- as.integer(args[1])

# which of n random optimizations this is
which.n.random <- as.integer(args[2])

# run the dimensionality fitting code
source('../Utils/fit.machine.R')

# save the fits and write out a table of the AICs
Control.optim.lowD <- optim.lowD
save(Control.optim.lowD,
	file=paste0("../../results/Spain/Control.optim.D",dimensions,".r",which.n.random,".Rdata"),
	ascii = TRUE
)