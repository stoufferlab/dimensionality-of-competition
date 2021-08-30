
# conduct procrustes tests between species-species trait matrices

library(here)
source(here('code/Utils/response.effect.from.pars.R'))
source(here('code/Utils/cayley.R'))

# read in the best low-D control fit
all.fits <- read.table('../../results/Spain/spain.Control.fit.summary.csv')
which.fit <- which.min(subset(all.fits, dimensions==2)$AIC)
load(paste0('../../fits/Spain/',rownames(subset(all.fits, dimensions==2)[which.fit,])))
control.traits <- response.effect.from.pars(
	Control.optim.lowD@coef,
	Control.optim.lowD@data$targets,
	Control.optim.lowD@data$competitors, 
	Control.optim.lowD@data$dimensions
)

# read in the best low-D treatment fit
all.fits <- read.table('../../results/Spain/spain.Treatment.fit.summary.csv')
which.fit <- which.min(subset(all.fits, dimensions==3)$AIC)
load(paste0('../../fits/Spain/',rownames(subset(all.fits, dimensions==3)[which.fit,])))
treatment.traits <- response.effect.from.pars(
	Treatment.optim.lowD@coef,
	Treatment.optim.lowD@data$targets,
	Treatment.optim.lowD@data$competitors, 
	Treatment.optim.lowD@data$dimensions
)

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