
# automatically generate the bounds for the optimizer
optim.bounds <- function(targets,competitors,dimensions){
	# lower bounds on parameters
	lower <- c(
		rep(-Inf, length(targets)), # lambdas
		rep(-Inf, dimensions) # weightings
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
