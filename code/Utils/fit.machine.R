
library(here)
source(here('code/Utils/dev.fun.R'))
source(here('code/Utils/glm.coefs.from.traits.R'))
source(here('code/Utils/null.pars.R'))
source(here('code/Utils/optim.bounds.R'))
source(here('code/Utils/polar.transform.R'))
source(here('code/Utils/response.effect.from.pars.R'))

###############################################################################
###############################################################################
### Attempt things by optimizing the traits
###############################################################################
###############################################################################

# get null estimates of the model intercepts as a starting point
null.formula <- as.formula(paste0(fecundity," ~ 0 + target"))
null.fit <- glm(
	null.formula,
	family=which.family,
	data=fecundity.data,
	control=list(maxit=1000)
)

# Print out the null model deviance as a point of reference
message("Message --> Null Model: Deviance = ",null.fit$deviance," / AIC = ", null.fit$aic)

# the full competition model whose coefficients get replaced with response-effect versions
model.formula <- as.formula(paste0(fecundity," ~ 0 + target + ",paste0("target:",competitors,collapse=" + ")))
x <- model.matrix(model.formula, fecundity.data)
y <- fecundity.data[,fecundity]

# container for the optimized fits
optim.lowD<-list()

# keep abreast of the situation
message("Message: Optimizing at Dimension = ",dimensions)

# dummy fit as something to compare to at the end of optimization
if(!as.character(dimensions) %in% names(optim.lowD)){
	optim.lowD[[as.character(dimensions)]] <- list()
	optim.lowD[[as.character(dimensions)]]$value <- Inf
	optim.lowD[[as.character(dimensions)]]$aic <- NA
}

# starting point of the optimization depends on which random iteration this is
if(which.n.random == 1){
	# "null" traits essentially make there be no competitive effect
	par.start <- null.pars(
		targets,
		competitors,
		dimensions,
		random.angles=FALSE,
		lambdas=which.family$linkinv(coef(null.fit))
	)
}else{
	# weak interactions but a random hierarchy within each dimension
	par.start <- null.pars(
		targets,
		competitors,
		dimensions,
		random.angles=TRUE,
		lambdas=which.family$linkinv(coef(null.fit))
	)
}

# check if the parameters actually "work" (this is most useful when changing dimensions)
dev.start <- dev.fun(par.start, dimensions, x, y, family=which.family, targets=targets, competitors=competitors)

# bunk starting conditions
if(is.na(dev.start)){
	message("Message: Aborting due to NA starting conditions for optimization")
}else{
	# print out starting conditions just for kicks
	glm.coefs <- glm.coefs.from.traits(par.start, targets, competitors, dimensions, colnames(x))
	mu <- which.family$linkinv(x %*% glm.coefs)
	aic <- which.family$aic(y,length(y),mu,rep(1,length(y)),dev.start) + 2*length(par.start)
	message("Message --> Attempt ",which.n.random,": From Deviance = ",dev.start," / AIC = ", aic)

	# get the optimizer bounds for this dimensionality
	bounds <- optim.bounds(targets, competitors, dimensions)

	# try to optimize the fit of the data given the parameter vector
	optim <- try(
		nloptr::sbplx(
			x0=par.start,
			fn=dev.fun,
			lower=bounds$lower,
			upper=bounds$upper,
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
		message("Message --> Attempt ",which.n.random,":   To Deviance = ", optim$value, " / AIC = ", aic)

		# this is the best fit at this dimension so far
		if(optim$value < optim.lowD[[as.character(dimensions)]]$value){
			optim.lowD[[as.character(dimensions)]] <- list(
				value=optim$value,
				aic=aic,
				par=optim$par,
				x=x,
				y=y,
				coefs=glm.coefs,
				mu=mu
			)
		}
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
	}
}
