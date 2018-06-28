
#source('glm.fit3.R')
source('dev.fun.R')
source('get.alphas.from.model.R')
source('glm.coefs.from.traits.R')
source('change.dimensions.R')

##############################################################################
###############################################################################
### Attempt things based on an SVD of the alpha matrix
###############################################################################
###############################################################################
# if(TRUE){
# 	# go through the normal fitting procedure to get the full alpha model to converge as a basis of comparison
# 	competitors <- colnames(mayfield)[9:39]

# 	mayfield$neighbors <- rowSums(mayfield[,competitors])
# 	model.formula <- as.formula("seeds ~ 0 + target + target:neighbors")
# 	gamma.fit.0 <- glm(
# 		model.formula,
# 		family=poisson(),
# 		data=mayfield,
# 		control=glm.control(maxit=1000) #,trace=2)
# 	)

# 	model.formula <- as.formula(paste0("seeds ~ 0 + target + ",paste0("target:",competitors,collapse=" + ")))
# 	startnames <- colnames(model.matrix(model.formula,mayfield))
# 	start1 <- rep(0,length(startnames)) # as.numeric(coef(gamma.fit.0)[gsub("background[A-Z]{4}:",'',startnames)])
# 	names(start1) <- startnames
# 	# start1[grepl("SOLO",names(start1))] <- 0

# 	gamma.fit.1 <- glm(
# 		model.formula,
# 		family=poisson(),
# 		data=mayfield,
# 		start=start1,
# 		#method=glm.fit3,
# 		control=list(maxit=1000) #,trace=2)
# 	)

# XX

# 	# what is the deviance of the lowD version of these coefficients?
# 	gamma.lowD<-list()

# 	alphas <- get.alphas.from.model(coef(gamma.fit.1), levels(mayfield$target), levels(mayfield$background))
# 	S <- svd(alphas)

# 	for(dimensions in seq.int(1,10)){
# 		par <- numeric(nlevels(mayfield$target) + nlevels(mayfield$target)*dimensions + (nlevels(mayfield$background)-1)*dimensions)
# 		par[seq.int(1, nlevels(mayfield$target))] <- coef(gamma.fit.1)[paste0("target",levels(mayfield$target))]

# 		response.traits <- S$u[,1:dimensions,drop=FALSE] %*% (diag(dimensions) * sqrt(S$d[1:dimensions]))
# 		effect.traits <- S$v[,1:dimensions,drop=FALSE] %*% (diag(dimensions) * sqrt(S$d[1:dimensions]))

# 		par[seq.int(nlevels(mayfield$target)+1, length(par))] <- c(as.vector(response.traits), as.vector(effect.traits))

# 		x <- model.matrix(model.formula, mayfield)
# 		y <- mayfield$fruits
# 		weights <- rep(1,length(y))

# 		linkinv <- poisson()$linkinv
# 		dev.resids <- poisson()$dev.resids
# 		aic <- poisson()$aic
# 		targets <- levels(mayfield$target)
# 		competitors <- levels(mayfield$background)

# 		dev <- dev.fun(par, dimensions, x, y, weights, linkinv, dev.resids, targets, competitors, trace=FALSE)

# 		glm.coefs <- glm.coefs.from.traits(par, targets, competitors, dimensions, colnames(x))
# 		mu <- linkinv(x %*% glm.coefs)

# 		gamma.lowD[[as.character(dimensions)]] <- list(
# 			x=x,
# 			y=y,
# 			mu=mu,
# 			value=dev,
# 			aic=aic(y,length(y),mu,weights,dev) + 2*length(par),
# 			par=par,
# 			coefs=glm.coefs,
# 			alphas=get.alphas.from.model(glm.coefs, targets, competitors)
# 		)
# 	}
# }

# XXX

###############################################################################
###############################################################################
### Attempt things just by optimizing the traits
###############################################################################
###############################################################################
if(TRUE){
	# specify the model family to fit
	which.family <- Gamma()

	# get null estimates of the model intercepts as a starting point
	null.formula <- as.formula("seeds ~ 0 + target")
	null.fit <- glm(
		null.formula,
		family=which.family,
		data=mayfield,
		# method=glm.fit3,
		control=list(maxit=1000) #,trace=2)
	)

	model.formula <- as.formula(paste0("seeds ~ 0 + target + ",paste0("target:",competitors,collapse=" + ")))
	x <- model.matrix(model.formula, mayfield)
	y <- mayfield$seeds

	weights <- rep(1,length(y))

	linkinv <- which.family$linkinv
	dev.resids <- which.family$dev.resids
	aic <- which.family$aic
	targets <- levels(mayfield$target)

	optim.lowD<-list()
	# optim.lowD[as.character(seq.int(10))] <- NA

	# now fit things in the lowD world
	# start from low dimension to high and fit from null traits, SVD traits, and d-1 traits (if they exist)
	for(dimensions in seq.int(1,10)){ #seq.int(1,2)){

		# there are four ways to try an initial guess for parameters to be optimized
		for(attempts in c(1,3)){ #seq.int(3)){
				message(
					"Message: trying to optimize at dimension ",
					dimensions,
					switch(attempts,
						" from null traits",
						" from SVD reduced traits of full dimension fit",
						" from best-fit traits at d-1"
					)					
				)

			par <- switch(attempts,
				c(coef(null.fit), rep(0,length(targets)*dimensions), rep(0,(length(competitors))*dimensions)),
				NULL, #gamma.lowD[[as.character(dimensions)]]$par,
				change.dimensions(optim.lowD[[as.character(dimensions-1)]]$par, targets, competitors, dimensions-1, dimensions)
			)

			poop <- ifelse(is.null(par), NA, dev.fun(par, dimensions, x, y, weights, linkinv, dev.resids, targets, competitors))

			# print(poop)
			if(is.na(poop)){
				message("Message: aborting due to NA starting conditions for optimization")
			}else{

				lower <- c(
					rep(0, nlevels(mayfield$target)),
					rep(0, nlevels(mayfield$target) * dimensions),
					rep(-2, length(competitors) * dimensions)
				)
				upper <- c(
					rep(Inf, nlevels(mayfield$target)),
					rep(2, length(par)-length(targets))
				)

				optim <- try(
					nloptr::sbplx(
						x0=par,
						fn=dev.fun,
						lower=lower,
						upper=upper,
						control=list(ftol_rel=1e-8, maxeval=100000),
						dimensions=dimensions,
						x=x,
						y=y,
						weights=weights,
						linkinv=linkinv,
						dev.resids=dev.resids,
						targets=targets,
						competitors=competitors
						# trace=TRUE,
					)
				)

				# who'd have thunk it, it worked!
				if(!inherits(optim, "try-error")){
					message("Dimension = ", dimensions, " Deviance = ", optim$value)
					if(!as.character(dimensions) %in% names(optim.lowD)){
						glm.coefs <- glm.coefs.from.traits(optim$par, targets, competitors, dimensions, colnames(x))
						mu <- linkinv(x %*% glm.coefs)

						optim.lowD[[as.character(dimensions)]] <- list(
							x=x,
							y=y,
							mu=mu,
							value=optim$value,
							aic=aic(y,length(y),mu,weights,optim$value) + 2*length(optim$par),
							par=optim$par,
							coefs=glm.coefs,
							alphas=get.alphas.from.model(glm.coefs, targets, competitors)
						)
					}else{
						if(optim$value < optim.lowD[[as.character(dimensions)]]$value){
							glm.coefs <- glm.coefs.from.traits(optim$par, targets, competitors, dimensions, colnames(x))
							mu <- linkinv(x %*% glm.coefs)

							optim.lowD[[as.character(dimensions)]] <- list(
								x=x,
								y=y,
								mu=mu,
								value=optim$value,
								aic=aic(y,length(y),mu,weights,optim$value) + 2*length(optim$par),
								par=optim$par,
								coefs=glm.coefs,
								alphas=get.alphas.from.model(glm.coefs, targets, competitors)
							)
						}
					}
				}else{
					warning(
						switch(attempts,
							"optimization from null traits failed",
							"optimization from SVD reduced traits failed",
							"optimization from best-fit traits at d-1 failed"
						),
						paste0(" at dimension ", dimensions),
						call. = FALSE,
						immediate. = TRUE
					)
				}
			}
		}
	}

	for(dimensions in seq.int(9,1)){

		# there are four ways to try an initial guess for parameters to be optimized
		message(
			"Message: trying to optimize at dimension ",
			dimensions,
 			" from best-fit traits at d+1"
		)					

		par <- change.dimensions(optim.lowD[[as.character(dimensions+1)]]$par, targets, competitors, dimensions+1, dimensions)

		poop <- dev.fun(par, dimensions, x, y, weights, linkinv, dev.resids, targets, competitors)

		if(is.na(poop)){
			message("Message: aborting due to NA starting conditions for optimization")
		}else{

			lower <- c(
				rep(0, nlevels(mayfield$target)),
				rep(0, nlevels(mayfield$target) * dimensions),
				rep(-2, length(competitors) * dimensions)
			)
			upper <- c(
				rep(Inf, nlevels(mayfield$target)),
				rep(2, length(par)-length(targets))
			)

			optim <- try(
				nloptr::sbplx(
					x0=par,
					fn=dev.fun,
					lower=lower,
					upper=upper,
					control=list(ftol_rel=1e-8, maxeval=100000),
					dimensions=dimensions,
					x=x,
					y=y,
					weights=weights,
					linkinv=linkinv,
					dev.resids=dev.resids,
					targets=targets,
					competitors=competitors
					# trace=TRUE,
				)
			)

			# who'd have thunk it, it worked!
			if(!inherits(optim, "try-error")){
				message("Dimension = ", dimensions, " Deviance = ", optim$value)

				if(optim$value < optim.lowD[[as.character(dimensions)]]$value){
					glm.coefs <- glm.coefs.from.traits(optim$par, targets, competitors, dimensions, colnames(x))

					mu <- linkinv(x %*% glm.coefs)

					optim.lowD[[as.character(dimensions)]] <- list(
						x=x,
						y=y,
						mu=mu,
						value=optim$value,
						aic=aic(y,length(y),mu,weights,optim$value) + 2*length(optim$par),
						par=optim$par,
						coefs=glm.coefs,
						alphas=get.alphas.from.model(glm.coefs, targets, competitors)
					)
				}
			}else{
				warning(
					"optimization from best-fit traits at d+1 failed",
					paste0(" at dimension ", dimensions),
					call. = FALSE,
					immediate. = TRUE
				)
			}
			
		}
	}

	for(dimensions in seq.int(1,10)){

		# there are four ways to try an initial guess for parameters to be optimized
		message(
			"Message: trying to optimize at dimension ",
			dimensions,
 			" from best-fit traits at d"
		)					

		par <- optim.lowD[[as.character(dimensions)]]$par

		poop <- dev.fun(par, dimensions, x, y, weights, linkinv, dev.resids, targets, competitors)

		if(is.na(poop)){
			message("Message: aborting due to NA starting conditions for optimization")
		}else{

			lower <- c(
				rep(0, nlevels(mayfield$target)),
				rep(0, nlevels(mayfield$target) * dimensions),
				rep(-2, length(competitors) * dimensions)
			)
			upper <- c(
				rep(Inf, nlevels(mayfield$target)),
				rep(2, length(par)-length(targets))
			)

			optim <- try(
				nloptr::sbplx(
					x0=par,
					fn=dev.fun,
					lower=lower,
					upper=upper,
					control=list(ftol_rel=1e-8, maxeval=100000),
					dimensions=dimensions,
					x=x,
					y=y,
					weights=weights,
					linkinv=linkinv,
					dev.resids=dev.resids,
					targets=targets,
					competitors=competitors,
					# trace=TRUE,
				)
			)

			# who'd have thunk it, it worked!
			if(!inherits(optim, "try-error")){
				message("Dimension = ", dimensions, " Deviance = ", optim$value)

				if(optim$value < optim.lowD[[as.character(dimensions)]]$value){
					glm.coefs <- glm.coefs.from.traits(optim$par, targets, competitors, dimensions, colnames(x))

					mu <- linkinv(x %*% glm.coefs)

					optim.lowD[[as.character(dimensions)]] <- list(
						x=x,
						y=y,
						mu=mu,
						value=optim$value,
						aic=aic(y,length(y),mu,weights,optim$value) + 2*length(optim$par),
						par=optim$par,
						coefs=glm.coefs,
						alphas=get.alphas.from.model(glm.coefs, targets, competitors)
					)
				}
			}else{
				warning(
					"optimization from best-fit traits at d failed",
					paste0(" at dimension ", dimensions),
					call. = FALSE,
					immediate. = TRUE
				)
			}
			
		}
	}
}
