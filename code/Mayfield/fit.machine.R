
source('../Mayfield/dev.fun.R')
source('../Mayfield/glm.coefs.from.traits.R')
source('../Mayfield/change.dimensions.R')
source('../Mayfield/response.effect.from.pars.R')
source('../Godoy/polar.transform.R')

#####################
# given the data set up some null parameters to start with in the optimizer
null.pars <- function(null.fit, targets, competitors, dimensions, random.angles=FALSE){
	# lambdas and weightings are more straightforward
	lambdas <- coef(null.fit) # lambdas
	weights <- rep(log(0.001),dimensions) # weightings
	names(weights) <- paste0("weight",seq.int(dimensions))
	
	# the number of response and effect angles is trickier
	response.dof <- seq.int(length(targets)-1,0)
	if(!random.angles){
		response.angles <- rep(0, sum(response.dof[seq.int(dimensions)]))
	}else{
		response.angles <- unlist(sapply(
			response.dof[seq.int(min(dimensions,length(targets)-1))],
			function(x){
				# the first n-1 are in [-pi/2, pi/2]
				# the last one is in [-pi, pi]
				angles <- c(runif(x-1, -pi/2, pi/2), runif(1, -pi, pi))
				angles
			}
		))
	}
	names(response.angles) <- paste0("response",seq.int(length(response.angles)))

	effect.dof <- seq.int(length(competitors)-1,0)
	if(!random.angles){
		effect.angles <- rep(0, sum(effect.dof[seq.int(dimensions)]))	
	}else{
		effect.angles <- unlist(sapply(
			effect.dof[seq.int(min(dimensions,length(competitors)-1))],
			function(x){
				# the first n-1 are in [-pi/2, pi/2]
				# the last one is in [-pi, pi]
				angles <- c(runif(x-1, -pi/2, pi/2), runif(1, -pi, pi))
				angles
			}
		))
	}	
	names(effect.angles) <- paste0("effect",seq.int(length(effect.angles)))

	par <- c(lambdas, weights, response.angles, effect.angles)
	return(par)
}

# automatically generate the bounds for the optimizer
optim.bounds <- function(targets,competitors,dimensions){
	# lower bounds on parameters
	lower <- c(
		rep(0, length(targets)), # lambdas
		rep(-Inf, dimensions) # weightings
		# rep(0, (length(targets)-1) * dimensions), # response angles
		# rep(0, (length(competitors)-1) * dimensions) # effect angles
	)
	response.dof <- seq.int(length(targets)-1,0)
	response.bounds <- unlist(sapply(
		response.dof[seq.int(min(dimensions,length(targets)-1))],
		function(x){
			# the first n-1 are in [-pi/2, pi/2]
			# the last one is in [-pi, pi]
			angles <- c(rep(-pi/2, x-1), -pi)
			angles
		}
	))
	effect.dof <- seq.int(length(competitors)-1,0)
	effect.bounds <- unlist(sapply(
		effect.dof[seq.int(min(dimensions,length(competitors)-1))],
		function(x){
			# the first n-1 are in [-pi/2, pi/2]
			# the last one is in [-pi, pi]
			angles <- c(rep(-pi/2, x-1), -pi)
			angles
		}
	))

	# stitch all lower bounds together
	lower <- c(lower, response.bounds, effect.bounds)

	# upper bounds on parameters
	# lower bounds on parameters
	upper <- c(
		rep(Inf, length(targets)), # lambdas
		rep(Inf, dimensions) # weightings
		# rep(0, (length(targets)-1) * dimensions), # response angles
		# rep(0, (length(competitors)-1) * dimensions) # effect angles
	)
	response.dof <- seq.int(length(targets)-1,0)
	response.bounds <- unlist(sapply(
		response.dof[seq.int(min(dimensions,length(targets)-1))],
		function(x){
			# the first n-1 are in [-pi/2, pi/2]
			# the last one is in [-pi, pi]
			angles <- c(rep(pi/2, x-1), pi)
			angles
		}
	))
	effect.dof <- seq.int(length(competitors)-1,0)
	effect.bounds <- unlist(sapply(
		effect.dof[seq.int(min(dimensions,length(competitors)-1))],
		function(x){
			# the first n-1 are in [-pi/2, pi/2]
			# the last one is in [-pi, pi]
			angles <- c(rep(pi/2, x-1), pi)
			angles
		}
	))

	# stitch all upper bounds together
	upper <- c(upper, response.bounds, effect.bounds)

	# return both lower and upper bounds
	list(lower=lower, upper=upper)
}

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
	# method=glm.fit3,
	control=list(maxit=1000) #,trace=2)
)

# the full competition model whose coefficients get replaced with response-effect versions
model.formula <- as.formula(paste0(fecundity," ~ 0 + target + ",paste0("target:",competitors,collapse=" + ")))
x <- model.matrix(model.formula, fecundity.data)
y <- fecundity.data[,fecundity]

# container for the optimized fits
optim.lowD<-list()

# now fit things in the lowD world
# start from low dimension to high and fit from null traits
# for(dimensions in seq.int(length(targets))){
	dimensions <- which.dimension

	# keep abreast of the situation
	message("Message: Optimizing at Dimension = ",dimensions)

	# we haven't tried this dimension before so need something to compare everything too
	if(!as.character(dimensions) %in% names(optim.lowD)){
		optim.lowD[[as.character(dimensions)]] <- list()
		optim.lowD[[as.character(dimensions)]]$value <- Inf
	}

	# for(random.starts in seq.int(n.random)){
	random.starts <- which.n.random
	
		if(random.starts == 1){
			# "null" traits essentially make there be no competitive effect
			par.start <- null.pars(null.fit, targets, competitors, dimensions, random.angles=FALSE)
		}else{
			# weak interactions but a random hierarchy within each dimension
			par.start <- null.pars(null.fit, targets, competitors, dimensions, random.angles=TRUE)
		}

		# check if the parameters actually work (this is most useful when changing dimensions)
		dev.start <- dev.fun(par.start, dimensions, x, y, family=which.family, targets=targets, competitors=competitors)

		# bunk starting conditions
		if(is.na(dev.start)){
			message("Message: Aborting due to NA starting conditions for optimization")
		}else{
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
				message("Message: Attempt ",random.starts,": From Deviance = ",dev.start," to Deviance = ", optim$value, " / AIC = ", aic)

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
					random.starts,
					" at Dimension = ",
					dimensions,
					" Failed",
					call. = FALSE,
					immediate. = TRUE
				)
			}
		}
	# }
# }