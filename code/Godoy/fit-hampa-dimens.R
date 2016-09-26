
#source('glm.fit3.R')
source('dev.fun.R')
source('get.alphas.from.model.R')
source('glm.coefs.from.traits.R')
source('change.dimensions.R')


# read in Oscar's data
datadir <- "../../data/Godoy/"
hampa <- read.csv(paste0(datadir, "hampa_neigbours_survey.csv"), row.names=1)

# remove the extra columns at the end
hampa <- hampa[,which(!grepl("X",colnames(hampa)))]

# turn the NAs for background into a non-existent species SOLO which will help us when using the glm function (or equivalents) for fitting
levels(hampa$background) <- c(levels(hampa$background),"SOLO")
hampa$background[which(is.na(hampa$background))] <- "SOLO"

# the data should now be primed for analysis; woohoo!

# let's try just one treatment for simplicity in testing regime
hampa <- subset(hampa, treatment=="T")

###############################################################################
###############################################################################
### Attempt things based on an SVD of the alpha matrix
###############################################################################
###############################################################################
if(TRUE){
	# go through the normal fitting procedure to get the full alpha model to converge as a basis of comparison
	model.formula <- as.formula("fruits ~ 0 + target + target:neighbours_number")
	gamma.fit.0 <- glm(
		model.formula,
		family=Gamma(),
		data=hampa,
		control=glm.control(maxit=1000) #,trace=2)
	)

	model.formula <- as.formula("fruits ~ 0 + target + target:background:neighbours_number")
	startnames <- colnames(model.matrix(model.formula,hampa))
	start1 <- as.numeric(coef(gamma.fit.0)[gsub("background[A-Z]{4}:",'',startnames)])
	names(start1) <- startnames
	start1[grepl("SOLO",names(start1))] <- 0

	gamma.fit.1 <- glm(
		model.formula,
		family=Gamma(),
		data=hampa,
		start=start1,
		#method=glm.fit3,
		control=list(maxit=1000) #,trace=2)
	)

	# what is the deviance of the lowD version of these coefficients?
	gamma.lowD<-list()

	alphas <- get.alphas.from.model(coef(gamma.fit.1), levels(hampa$target), levels(hampa$background))
	S <- svd(alphas)

	for(dimensions in seq.int(1,10)){
		par <- numeric(nlevels(hampa$target) + nlevels(hampa$target)*dimensions + (nlevels(hampa$background)-1)*dimensions)
		par[seq.int(1, nlevels(hampa$target))] <- coef(gamma.fit.1)[paste0("target",levels(hampa$target))]

		response.traits <- S$u[,1:dimensions,drop=FALSE] %*% (diag(dimensions) * sqrt(S$d[1:dimensions]))
		effect.traits <- S$v[,1:dimensions,drop=FALSE] %*% (diag(dimensions) * sqrt(S$d[1:dimensions]))

		par[seq.int(nlevels(hampa$target)+1, length(par))] <- c(as.vector(response.traits), as.vector(effect.traits))

		x <- model.matrix(model.formula, hampa)
		y <- hampa$fruits
		weights <- rep(1,length(y))

		linkinv <- Gamma()$linkinv
		dev.resids <- Gamma()$dev.resids
		aic <- Gamma()$aic
		targets <- levels(hampa$target)
		competitors <- levels(hampa$background)

		dev <- dev.fun(par, dimensions, x, y, weights, linkinv, dev.resids, targets, competitors, trace=FALSE)

		glm.coefs <- glm.coefs.from.traits(par, targets, competitors, dimensions, colnames(x))
		mu <- linkinv(x %*% glm.coefs)

		gamma.lowD[[as.character(dimensions)]] <- list(
			x=x,
			y=y,
			mu=mu,
			value=dev,
			aic=aic(y,length(y),mu,weights,dev) + 2*length(par),
			par=par,
			coefs=glm.coefs,
			alphas=get.alphas.from.model(glm.coefs, targets, competitors)
		)
	}
}

###############################################################################
###############################################################################
### Attempt things just by optimizing the traits
###############################################################################
###############################################################################
if(TRUE){
	# get null estimates of the model intercepts as a starting point
	null.formula <- as.formula("fruits ~ 0 + target")
	null.fit <- glm(
		null.formula,
		family=Gamma(),
		data=hampa,
		# method=glm.fit3,
		control=list(maxit=1000) #,trace=2)
	)

	x <- model.matrix(model.formula, hampa)
	y <- hampa$fruits

	weights <- rep(1,length(y))

	linkinv <- Gamma()$linkinv
	dev.resids <- Gamma()$dev.resids
	aic <- Gamma()$aic
	targets <- levels(hampa$target)
	competitors <- levels(hampa$background)

	optim.lowD<-list()
	# optim.lowD[as.character(seq.int(10))] <- NA

	# now fit things in the lowD world
	# start from low dimension to high and fit from null traits, SVD traits, and d-1 traits (if they exist)
	for(dimensions in seq.int(1,10)){ #seq.int(1,2)){

		# there are four ways to try an initial guess for parameters to be optimized
		for(attempts in seq.int(3)){
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
				c(coef(null.fit), rep(0,length(targets)*dimensions), rep(0,(length(competitors)-1)*dimensions)),
				gamma.lowD[[as.character(dimensions)]]$par,
				change.dimensions(optim.lowD[[as.character(dimensions-1)]]$par, targets, competitors, dimensions-1, dimensions)
			)

			poop <- ifelse(is.null(par), NA, dev.fun(par, dimensions, x, y, weights, linkinv, dev.resids, targets, competitors))

			if(is.na(poop)){
				message("Message: aborting due to NA starting conditions for optimization")
			}else{

				lower <- c(
					rep(0, nlevels(hampa$target)),
					rep(-2, length(par)-length(targets))
				)
				upper <- c(
					rep(Inf, nlevels(hampa$target)),
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
				rep(0, nlevels(hampa$target)),
				rep(-2, length(par)-length(targets))
			)
			upper <- c(
				rep(Inf, nlevels(hampa$target)),
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
				rep(0, nlevels(hampa$target)),
				rep(-2, length(par)-length(targets))
			)
			upper <- c(
				rep(Inf, nlevels(hampa$target)),
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
