
# we get some input from the command line
args <- commandArgs(trailingOnly = TRUE)

# the core dimension for this optimization
which.dataset <- as.integer(args[1])

# read in the fake goldberg data
source('prep.data.R')

# specify the model family to fit
which.family <- poisson(link="inverse")

# the core dimension for this optimization
dimensions <- as.integer(args[2])

# which of n random optimizations this is
which.n.random <- as.integer(args[3])

# run the fitting code
source('../Utils/fit.machine.R')

# save the fits
Synthetic.optim.lowD <- optim.lowD
save(Synthetic.optim.lowD,
	file=paste0("../../results/Synthetic-Datasets/Synthetic-Dataset-",which.fake.data,"/Synthetic-Dataset-",which.fake.data,".optim.D",which.dimension,".r",which.n.random,".Rdata"),
	ascii = TRUE
)
