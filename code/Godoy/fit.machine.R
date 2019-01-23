
#source('glm.fit3.R')
source('dev.fun.R')
source('get.alphas.from.model.R')
source('glm.coefs.from.traits.R')
source('change.dimensions.R')
source('polar.transform.R')

#####################
# given the data set up some null parameters to start with in the optimizer
null.pars <- function(null.fit, targets, competitors, dimensions){
	# lambdas and weightings are more straightforward
	lambdas <- coef(null.fit) # lambdas
	weights <- rep(-10,dimensions) # weightings (in log space)
	names(weights) <- paste0("weight",seq.int(dimensions))
	
	# the number of response and effect angles is trickier
	response.dof <- seq.int(length(targets)-1,0)
	response.angles <- rep(0, sum(response.dof[seq.int(dimensions)]))
	names(response.angles) <- paste0("response",seq.int(length(response.angles)))

	# note extra -1 because of "background"
	effect.dof <- seq.int(length(competitors)-1-1,0)
	effect.angles <- rep(0, sum(effect.dof[seq.int(dimensions)]))
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
		# rep(0, (length(competitors)-2) * dimensions) # effect angles
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
	effect.dof <- seq.int(length(competitors)-1-1,0)
	effect.bounds <- unlist(sapply(
		effect.dof[seq.int(min(dimensions,length(competitors)-1-1))],
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
		# rep(0, (length(competitors)-2) * dimensions) # effect angles
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
	effect.dof <- seq.int(length(competitors)-1-1,0)
	effect.bounds <- unlist(sapply(
		effect.dof[seq.int(min(dimensions,length(competitors)-1-1))],
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

# specify the model family to fit
which.family <- Gamma()

# get null estimates of the model intercepts as a starting point
null.formula <- as.formula("fruits ~ 0 + target")
null.fit <- glm(
	null.formula,
	family=which.family,
	data=hampa,
	# method=glm.fit3,
	control=list(maxit=1000) #,trace=2)
)

model.formula <- as.formula("fruits ~ 0 + target + target:background:neighbours_number")
x <- model.matrix(model.formula, hampa)
y <- hampa$fruits

linkinv <- which.family$linkinv
dev.resids <- which.family$dev.resids
aic <- which.family$aic
targets <- levels(hampa$target)
competitors <- levels(hampa$background)

# container for the optimized fits
optim.lowD<-list()

# now fit things in the lowD world
# start from low dimension to high and fit from null traits
for(dimensions in seq.int(length(targets))){
	# keep abreast of the situation
	message("Message: trying first optimization at dimension ",dimensions)

	# we haven't tried this dimension before so need something to compare everything too
	if(!as.character(dimensions) %in% names(optim.lowD)){
		optim.lowD[[as.character(dimensions)]] <- list()
		optim.lowD[[as.character(dimensions)]]$value <- Inf
	}

	# "null" traits essentially make there be no competitive effect
	par.start <- null.pars(null.fit, targets, competitors, dimensions)
	
	# check if the parameters actually work (this is most useful when changing dimensions)
	dev.start <- dev.fun(par.start, dimensions, x, y, weights, linkinv, dev.resids, targets, competitors)

	# bunk starting conditions
	if(is.na(dev.start)){
		message("Message: aborting due to NA starting conditions for optimization")
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
				linkinv=linkinv,
				dev.resids=dev.resids,
				targets=targets,
				competitors=competitors
			)
		)

		# who'd have thunk it, it worked!
		if(!inherits(optim, "try-error")){
			# this is the best fit at this dimension so far
			if(optim$value < optim.lowD[[as.character(dimensions)]]$value){
				message("Message: Dimension = ", dimensions, " Deviance = ", optim$value)

				glm.coefs <- glm.coefs.from.traits(optim$par, targets, competitors, dimensions, colnames(x))
				mu <- linkinv(x %*% glm.coefs)

				optim.lowD[[as.character(dimensions)]] <- list(
					x=x,
					y=y,
					mu=mu,
					value=optim$value,
					aic=aic(y,length(y),mu,weights,optim$value) + 2*length(optim$par),
					par=optim$par,
					coefs=glm.coefs
				)
			}
		}else{
			warning(
				"Warning: first optimization at dimension",
				dimensions,
				" has failed",
				call. = FALSE,
				immediate. = TRUE
			)
		}
	}
}

# ################
# # optimization strategy two: use d for d-1
# ################

# # use the higher-dimensional traits as starting points for the d-1 versions of the model
# for(dimensions in seq.int(length(targets)-1,1)){

# 	# there are four ways to try an initial guess for parameters to be optimized
# 	message(
# 		"Message: trying to optimize at dimension ",
# 		dimensions,
# 			" from best-fit traits at d+1"
# 	)					

# 	# step down in dimensions
# 	par <- change.dimensions(optim.lowD[[as.character(dimensions+1)]]$par, targets, competitors, dimensions+1, dimensions)

# 	poop <- dev.fun(par, dimensions, x, y, weights, linkinv, dev.resids, targets, competitors)

# 	if(is.na(poop)){
# 		message("Message: aborting due to NA starting conditions for optimization")
# 	}else{

# 		# get the optimizer bounds for this dimensionality
# 		bounds <- optim.bounds(targets, competitors, dimensions)

# 		# try to optimize the fit of the data given the parameter vector
# 		optim <- try(
# 			nloptr::sbplx(
# 				x0=par,
# 				fn=dev.fun,
# 				lower=bounds$lower,
# 				upper=bounds$upper,
# 				control=list(ftol_rel=1e-8, maxeval=100000),
# 				# trace=TRUE,
# 				dimensions=dimensions,
# 				x=x,
# 				y=y,
# 				weights=weights,
# 				linkinv=linkinv,
# 				dev.resids=dev.resids,
# 				targets=targets,
# 				competitors=competitors
# 			)
# 		)

# 		# who'd have thunk it, it worked!
# 		if(!inherits(optim, "try-error")){
# 			message("Dimension = ", dimensions, " Deviance = ", optim$value)

# 			if(optim$value < optim.lowD[[as.character(dimensions)]]$value){
# 				message("Dimension = ", dimensions, " Deviance = ", optim$value)

# 				glm.coefs <- glm.coefs.from.traits(optim$par, targets, competitors, dimensions, colnames(x))
# 				mu <- linkinv(x %*% glm.coefs)

# 				optim.lowD[[as.character(dimensions)]] <- list(
# 					x=x,
# 					y=y,
# 					mu=mu,
# 					value=optim$value,
# 					aic=aic(y,length(y),mu,weights,optim$value) + 2*length(optim$par),
# 					par=optim$par,
# 					coefs=glm.coefs
# 					# alphas=get.alphas.from.model(glm.coefs, targets, competitors)
# 				)
# 			}
# 		}else{
# 			warning(
# 				"optimization from best-fit traits at d+1 failed",
# 				paste0(" at dimension ", dimensions),
# 				call. = FALSE,
# 				immediate. = TRUE
# 			)
# 		}
		
# 	}
# }

# for(dimensions in seq.int(length(targets))){

# 	# there are four ways to try an initial guess for parameters to be optimized
# 	message(
# 		"Message: trying to optimize at dimension ",
# 		dimensions,
# 			" from best-fit traits at d"
# 	)					

# 	par <- optim.lowD[[as.character(dimensions)]]$par
# 	poop <- dev.fun(par, dimensions, x, y, weights, linkinv, dev.resids, targets, competitors)

# 	if(is.na(poop)){
# 		message("Message: aborting due to NA starting conditions for optimization")
# 	}else{

# 		# get the optimizer bounds for this dimensionality
# 		bounds <- optim.bounds(targets, competitors, dimensions)

# 		# try to optimize the fit of the data given the parameter vector
# 		optim <- try(
# 			nloptr::sbplx(
# 				x0=par,
# 				fn=dev.fun,
# 				lower=bounds$lower,
# 				upper=bounds$upper,
# 				control=list(ftol_rel=1e-8, maxeval=100000),
# 				# trace=TRUE,
# 				dimensions=dimensions,
# 				x=x,
# 				y=y,
# 				weights=weights,
# 				linkinv=linkinv,
# 				dev.resids=dev.resids,
# 				targets=targets,
# 				competitors=competitors
# 			)
# 		)

# 		# who'd have thunk it, it worked!
# 		if(!inherits(optim, "try-error")){
# 			message("Dimension = ", dimensions, " Deviance = ", optim$value)

# 			if(optim$value < optim.lowD[[as.character(dimensions)]]$value){
# 				message("Dimension = ", dimensions, " Deviance = ", optim$value)

# 				glm.coefs <- glm.coefs.from.traits(optim$par, targets, competitors, dimensions, colnames(x))
# 				mu <- linkinv(x %*% glm.coefs)

# 				optim.lowD[[as.character(dimensions)]] <- list(
# 					x=x,
# 					y=y,
# 					mu=mu,
# 					value=optim$value,
# 					aic=aic(y,length(y),mu,weights,optim$value) + 2*length(optim$par),
# 					par=optim$par,
# 					coefs=glm.coefs
# 					# alphas=get.alphas.from.model(glm.coefs, targets, competitors)
# 				)
# 			}
# 		}else{
# 			warning(
# 				"optimization from best-fit traits at d failed",
# 				paste0(" at dimension ", dimensions),
# 				call. = FALSE,
# 				immediate. = TRUE
# 			)
# 		}
		
# 	}
# }

