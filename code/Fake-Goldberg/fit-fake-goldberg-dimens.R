
# we get some input from the command line
args <- commandArgs(trailingOnly = TRUE)

# the core dimension for this optimization
which.fake.data <- as.integer(args[1])

# read in the fake goldberg data
source('prep.data.R')

# specify the model family to fit
which.family <- Gamma()

# the core dimension for this optimization
which.dimension <- as.integer(args[2])

# which of n random optimizations this is
which.n.random <- as.integer(args[3])

# run the fitting code
source('../Mayfield/fit.machine.R')

# save the fits
Goldberg.optim.lowD <- optim.lowD
save(Goldberg.optim.lowD,
	file=paste0("../../results/Fake-Goldberg/Fake-Goldberg-",which.fake.data,"/Fake-Goldberg-",which.fake.data,".optim.D",which.dimension,".r",which.n.random,".Rdata"),
	ascii = TRUE
)
