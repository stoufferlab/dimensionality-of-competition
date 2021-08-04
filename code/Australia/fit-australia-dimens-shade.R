
# read in the Wainwright data for the Shade treatment
which.treatment <- "Shade"
source('prep.data.R')

# specify the model family to fit
which.family <- poisson(link="inverse")

# we get some input from the command line
args <- commandArgs(trailingOnly = TRUE)

# the core dimension for this optimization
dimensions <- as.integer(args[1])

# which of n random optimizations this is
which.n.random <- as.integer(args[2])

# set the random seed for reproducibility
# generated one integer between 1 and 100000 with Random Integer Generator at random.org
# Timestamp: 2021-08-04 21:02:34 UTC
set.seed(78562+which.n.random)

# run the dimensionality fitting code
source('../Utils/fit.machine.R')

# save the fits and write out a table of the AICs
Shade.optim.lowD <- optim.lowD
save(Shade.optim.lowD,
	file=paste0("../../results/Australia/Shade.optim.D",dimensions,".r",which.n.random,".Rdata"),
	ascii = TRUE
)
