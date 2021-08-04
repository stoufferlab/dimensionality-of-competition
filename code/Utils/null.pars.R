
# given the data set up some null parameters to start with in the optimizer
null.pars <- function(targets, competitors, dimensions, random.angles=FALSE, lambdas=NULL){
	# lambdas and weights are most straightforward
	if(is.null(lambdas)){
		lambdas <- log(rep(1,length(targets)))
	}else{
		lambdas <- log(lambdas)
	}
	names(lambdas) <- targets

	weights <- log(0.01 / seq.int(dimensions)) # weights; note log transformation
	names(weights) <- paste0("weight",seq.int(dimensions))
	
	# the number of response angles is constrained by orthogonality
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

	# the number of effect angles is constrained by orthogonality
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

	# put the parameters together in the standardized order
	par <- c(lambdas, weights, response.angles, effect.angles)
	
	return(par)
}
