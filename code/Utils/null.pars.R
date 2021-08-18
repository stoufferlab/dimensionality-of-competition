
# given the data set up some null parameters to start with in the optimizer
null.pars <- function(targets, competitors, dimensions, lambdas=NULL, alphas=NULL){
	if(is.null(lambdas)){
		# very poor guess of starting intercepts: all = 1
		lambdas <- log(rep(1,length(targets)))
	}else{
		lambdas <- log(lambdas) # note log transformation
	}

	if(is.null(alphas)){
		# very poor guess at starting alphas: a matrix of uniform [0,1] alpha coefficients as a starting point
		alphas <- matrix(
			runif(length(targets)*length(competitors)),
			length(targets),
			length(competitors)
		)
	}else{
		# use SVD to reduce the dimensionality a priori
		S <- svd(alphas)
		if(dimensions>1){
			alphas <- S$u[,seq.int(dimensions),drop=FALSE] %*% diag(S$d[seq.int(dimensions)]) %*% t(S$v[,seq.int(dimensions),drop=FALSE])
		}else{
			alphas <- S$u[,seq.int(dimensions),drop=FALSE] %*% S$d[1] %*% t(S$v[,seq.int(dimensions),drop=FALSE])
		}
	}
	names(lambdas) <- paste0("lambda",targets)

	# use QR decomposition to get starting points for the parameterization of the matrix
	alphas.qr <- qr(alphas)
	S <- diag(sign(diag(qr.R(alphas.qr))))
	R <- qr.Q(alphas.qr) %*% S
	E <- t(S %*% qr.R(alphas.qr))
	R <- R[,seq.int(dimensions),drop=FALSE]
	E <- E[,seq.int(dimensions),drop=FALSE]

	# the number of response parameters is constrained by orthogonality
	response.params <- InverseRmatrix(R, length(targets), dimensions)
	names(response.params) <- paste0("response",seq_along(response.params))

	# effect diagonal elements are constrained to be positive
	effect.diag <- log(diag(E)) # note log transformation
	names(effect.diag) <- paste0("effect.diag",seq_along(effect.diag))

	# effect lower-triangular elements are unconstrained
	effect.lower <- E[lower.tri(E)]
	names(effect.lower) <- paste0("effect.lower",seq_along(effect.lower))

	# put the parameters together in the standardized order
	par <- c(lambdas, response.params, effect.diag, effect.lower)

	return(par)
}
