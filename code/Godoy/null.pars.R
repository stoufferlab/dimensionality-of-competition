
# given the data set up some null parameters to start with in the optimizer
null.pars <- function(null.fit, targets, competitors, dimensions, random.angles=FALSE){
	# lambdas and weightings are more straightforward
	lambdas <- coef(null.fit) # lambdas
	names(lambdas) <- targets

	weights <- log(sort(runif(dimensions))) # weights
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
				angles <- c(rnorm(x-1, 0, 0.1), rnorm(1, 0, 0.1))
				angles
			}
		))
	}
	names(response.angles) <- paste0("response",seq.int(length(response.angles)))

	effect.dof <- seq.int(length(competitors)-2,0)
	if(!random.angles){
		effect.angles <- rep(0, sum(effect.dof[seq.int(dimensions)]))	
	}else{
		effect.angles <- unlist(sapply(
			effect.dof[seq.int(min(dimensions,length(competitors)-2))],
			function(x){
				# the first n-1 are in [-pi/2, pi/2]
				# the last one is in [-pi, pi]
				angles <- c(runif(x-1, -pi/2, pi/2), runif(1, -pi, pi))
				angles <- c(rnorm(x-1, 0, 0.1), rnorm(1, 0, 0.1))
				angles
			}
		))
	}	
	names(effect.angles) <- paste0("effect",seq.int(length(effect.angles)))

	par <- c(lambdas, weights, response.angles, effect.angles)
	return(par)
}
