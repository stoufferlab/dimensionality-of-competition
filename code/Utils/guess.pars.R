
# take a Akakie-weighted average of parameters across regression models (possibly with Gaussian noise added)
guess.pars <- function(targets, competitors, m0=NULL, m1=NULL, m2=NULL, m3=NULL, m4=NULL, sigma=0){
	parms <- list()
	if(!is.null(m0)){
		parms[["0"]] <- get.alphas.from.model(m0, 0, targets, competitors, zeros=TRUE)
		parms[["0"]]$aic <- AIC(m0)
	}
	if(!is.null(m1)){
		parms[["1"]] <- get.alphas.from.model(m1, 1, targets, competitors, zeros=TRUE)
		parms[["1"]]$aic <- AIC(m1)
	}
	if(!is.null(m2)){
		parms[["2"]] <- get.alphas.from.model(m2, 2, targets, competitors, zeros=TRUE)
		parms[["2"]]$aic <- AIC(m2)
	}
	if(!is.null(m3)){
		parms[["3"]] <- get.alphas.from.model(m3, 3, targets, competitors, zeros=TRUE)
		parms[["3"]]$aic <- AIC(m3)
	}
	if(!is.null(m4)){
		parms[["4"]] <- get.alphas.from.model(m4, 4, targets, competitors, zeros=TRUE)
		parms[["4"]]$aic <- AIC(m4)
	}
	aics <- sapply(parms, function(x) x$aic)
	lambdas <- sapply(
		parms,
		function(x){x$lambdas}
	)
	lambda.aics <- matrix(
		rep(aics, nrow(lambdas)),
		nrow(lambdas),
		length(aics),
		byrow=TRUE
	)
	lambda.aics[is.na(lambdas)] <- Inf
	lambda.weights <- exp(-0.5*sweep(lambda.aics, 1, apply(lambda.aics, 1, min), "-"))
	lambdas <- rowSums(lambdas * lambda.weights)

	alphas <- sapply(
		parms,
		function(x){as.vector(x$alphas)}
	)
	alpha.aics <- matrix(
		rep(aics, nrow(alphas)),
		nrow(alphas),
		length(aics),
		byrow=TRUE
	)
	alpha.aics[is.na(alphas)] <- Inf
	alpha.weights <- exp(-0.5*sweep(alpha.aics, 1, apply(alpha.aics, 1, min), "-"))
	alphas <- matrix(
		rowSums(alphas * alpha.weights, na.rm=TRUE),
		length(targets),
		length(competitors)
	)

	# add some noise
	alphas <- alphas + rnorm(prod(dim(alphas)), 0, sigma)

	# add some names to the alphas
	rownames(alphas) <- targets
	colnames(alphas) <- competitors

	# return things as a list
	ret <- list(lambdas=lambdas, alphas=alphas)
	return(ret)
}
