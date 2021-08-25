
library(here)

# read in the Wainwright data for the Shade treatment
which.treatment <- "Shade"
source(here('code/Australia/prep.data.R'))

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

# fit some regression models to use as a starting point for the low dimensional fit
source(here('code/Utils/model.comparison.R'))

# make an educated guess about the parameters prior to optimization
source(here('code/Utils/guess.pars.R'))
source(here('code/Utils/get.alphas.from.model.R'))

# use the weighted average across the regression models to get candidate starting params
par.start <- guess.pars(
	targets,
	competitors,
	m0=inverse.poisson.fit.0,
	m1=inverse.poisson.fit.1,
	m2=inverse.poisson.fit.2,
	m3=inverse.poisson.fit.3,
	sigma=0.001
)

# run the dimensionality fitting code
source(here('code/Utils/fit.machine.R'))

# save the fits and write out a table of the AICs
Shade.optim.lowD <- optim.lowD
save(Shade.optim.lowD,
	file=paste0("../../fits/Australia/Shade.optim.D",dimensions,".r",which.n.random,".Rdata"),
	ascii = TRUE
)
