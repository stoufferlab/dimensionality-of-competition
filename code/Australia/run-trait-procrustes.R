
# conduct procrustes tests between species-species trait matrices

library(here)
source(here('code/Utils/response.effect.from.pars.R'))
source(here('code/Utils/cayley.R'))

# read in the best low-D control fit
all.fits <- read.table('../../results/Australia/australia.Open.fit.summary.csv')
which.fit <- which.min(subset(all.fits, dimensions==1)$AIC)
load(paste0('../../fits/Australia/',rownames(subset(all.fits, dimensions==1)[which.fit,])))
control.traits <- response.effect.from.pars(
	Open.optim.lowD@coef,
	Open.optim.lowD@data$targets,
	Open.optim.lowD@data$competitors, 
	Open.optim.lowD@data$dimensions
)

# read in the best low-D treatment fit
all.fits <- read.table('../../results/Australia/australia.Shade.fit.summary.csv')
which.fit <- which.min(subset(all.fits, dimensions==2)$AIC)
load(paste0('../../fits/Australia/',rownames(subset(all.fits, dimensions==2)[which.fit,])))
treatment.traits <- response.effect.from.pars(
	Shade.optim.lowD@coef,
	Shade.optim.lowD@data$targets,
	Shade.optim.lowD@data$competitors, 
	Shade.optim.lowD@data$dimensions
)

# remove the "Other" effect species
control.traits$effect <- control.traits$effect[rownames(control.traits$effect)!="Other",,drop=FALSE]
treatment.traits$effect <- treatment.traits$effect[rownames(treatment.traits$effect)!="Other",,drop=FALSE]


## all traits
proc.test <- vegan::protest(
	dist(cbind(
		control.traits$response,
		control.traits$effect
	)),
	dist(cbind(
		treatment.traits$response,
		treatment.traits$effect
	))
)
print(proc.test)

## response traits
proc.test.response <- vegan::protest(
	dist(control.traits$response),
	dist(treatment.traits$response)
)
print(proc.test.response)

## all traits
proc.test.effect <- vegan::protest(
	dist(control.traits$effect),
	dist(treatment.traits$effect)
)
print(proc.test.effect)