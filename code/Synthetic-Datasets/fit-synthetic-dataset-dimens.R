
# we get some input from the command line
args <- commandArgs(trailingOnly = TRUE)

# the focal synthetic dataset
which.dataset <- as.integer(args[1])

# read in the synthetic dataset
source('prep.data.R')

# specify the model family to fit
which.family <- Gamma(link="inverse")

# the core dimension for this optimization
dimensions <- as.integer(args[2])

# which of n random optimizations this is
which.n.random <- as.integer(args[3])

# run the fitting code
source('../Utils/fit.machine.R')

# save the fits
Synthetic.optim.lowD <- optim.lowD
save(Synthetic.optim.lowD,
	file=paste0("../../results/Synthetic-Datasets/Synthetic-Dataset-",which.dataset,"/Synthetic-Dataset-",which.dataset,".optim.D",dimensions,".r",which.n.random,".Rdata"),
	ascii = TRUE
)
