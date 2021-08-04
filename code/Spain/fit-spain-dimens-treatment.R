
# read in the data for the specified treatment
which.treatment <- "T"
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
# Timestamp: 2021-08-04 21:05:11 UTC
set.seed(51318+which.n.random)

# run the dimensionality fitting code
source('../Utils/fit.machine.R')

# save the fits and write out a table of the AICs
Treatment.optim.lowD <- optim.lowD
save(Treatment.optim.lowD,
	file=paste0("../../results/Spain/Treatment.optim.D",dimensions,".r",which.n.random,".Rdata"),
	ascii = TRUE
)
