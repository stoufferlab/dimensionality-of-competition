
library(here)
source(here('code/Utils/dev.fun.R'))
source(here('code/Utils/glm.coefs.from.traits.R'))
source(here('code/Utils/null.pars.R'))
source(here('code/Utils/response.effect.from.pars.R'))
source(here('code/Utils/cayley.R'))

###############################################################################
###############################################################################
### Attempt things by optimizing the traits
###############################################################################
###############################################################################

source(here('code/Utils/model.comparison.R'))

# Print out the null model deviance as a point of reference
message("Message --> Null Model: Deviance = ",inverse.poisson.fit.0$deviance," / AIC = ", inverse.poisson.fit.0$aic)

# the full competition model whose coefficients get replaced with response-effect versions
model.formula <- as.formula(paste0(fecundity," ~ 0 + target + ",paste0("target:",competitors,collapse=" + ")))
x <- model.matrix(model.formula, fecundity.data)
y <- fecundity.data[,fecundity]

# keep abreast of the situation
message("Message: Optimizing at Dimension = ",dimensions)

# known estimate of lambda + weak interactions & a random hierarchy along each dimension
par.start <- null.pars(
	targets,
	competitors,
	dimensions,
	lambdas=which.family$linkinv(coef(inverse.poisson.fit.0))
)

# calculate the starting deviance before optimization
dev.start <- dev.fun(par.start, dimensions, x, y, family=which.family, targets=targets, competitors=competitors)

# make sure we have viable starting parameter values
while(!is.finite(dev.start)){
	par.start <- null.pars(
		targets,
		competitors,
		dimensions,
		lambdas=which.family$linkinv(coef(inverse.poisson.fit.0))
	)
	dev.start <- dev.fun(par.start, dimensions, x, y, family=which.family, targets=targets, competitors=competitors)
}

# print out starting conditions just for kicks
glm.coefs <- glm.coefs.from.traits(par.start, targets, competitors, dimensions, colnames(x))
mu <- which.family$linkinv(x %*% glm.coefs)
aic <- which.family$aic(y,length(y),mu,rep(1,length(y)),dev.start) + 2*length(par.start)
message("Message: Attempt ",which.n.random," from Deviance = ",dev.start," / AIC = ", aic)

# try to optimize the fit of the data given the parameter vector
optim <- try(
	nloptr::sbplx(
		x0=par.start,
		fn=dev.fun,
		control=list(ftol_rel=1e-8, maxeval=100000),
		# trace=TRUE,
		dimensions=dimensions,
		x=x,
		y=y,
		family=which.family,
		targets=targets,
		competitors=competitors
	)
)

# who'd have thunk it, it worked!
if(!inherits(optim, "try-error")){
	glm.coefs <- glm.coefs.from.traits(optim$par, targets, competitors, dimensions, colnames(x))
	mu <- which.family$linkinv(x %*% glm.coefs)
	aic <- which.family$aic(y,length(y),mu,rep(1,length(y)),optim$value) + 2*length(optim$par)
	message("Message: Attempt ",which.n.random,"   to Deviance = ", optim$value, " / AIC = ", aic)

	optim.lowD <- list(
		dimensions=dimensions,
		value=optim$value,
		aic=aic,
		par=optim$par,
		x=x,
		y=y
	)
}else{
	warning(
		"Warning: Attempt ",
		which.n.random,
		" at Dimension = ",
		dimensions,
		" Failed",
		call. = FALSE,
		immediate. = TRUE
	)

	# save an empty/null output of the optimization
	optim.lowD <- list(
		dimensions=dimensions,
		value=NA,
		aic=NA,
		par=NA
	)
}
