
optimal.traits <- function(data, dimensions, focals, competitors, start=NULL, maxit=glm.control()$maxit, ftol_rel=nloptr::nl.opts()$ftol_rel, maxeval=nloptr::nl.opts()$maxeval){
	# construct the faux model formula within which each pairwise interaction gets fit
	model.formula <- as.formula(paste0("seeds ~ 0 + focal + ", paste0('focal:', competitors, collapse=" + ")))

	# get a design matrix corresponding to the formula
	mm <- model.matrix(model.formula, data)

	# the number of seeds is what we're trying to predict
	y <- data$seeds

	# all observations have the same intrinsic weight (aside from differences in predicted variance)
	weights <- rep(1,length(y))

	# we're treating fecundities as gamma distributed observations
	family <- Gamma()
	# linkinv <- family$linkinv
	# dev.resids <- family$dev.resids
	# aic <- family$aic
	
	# message(
	# 		"Message: trying to optimize at dimension ",
	# 		dimensions,
	# 		" from null traits"
	# )	

	if(is.null(start)){
		# get null estimates of the model intercepts as a starting point
		null.formula <- as.formula("seeds ~ 0 + focal")
		null.fit <- glm(
			null.formula,
			family=Gamma(),
			data=data,
			control=list(maxit=maxit)
		)
		par <- c(coef(null.fit), rep(0,length(focals)*dimensions), rep(0,(length(competitors))*dimensions))
	}else{
		par <- start
		# TODO: should verify that the start has the correct dimensions
	}

	dev <- dev.fun(par, dimensions, mm, y, weights, family, focals, competitors)

	if(is.na(dev)){
		message("Message: aborting due to NA starting conditions for optimization")
	}else{
		# specify lower bounds for the optimization
		lower <- c(
			rep(0, nlevels(data$focal)),
			rep(-8, length(par)-length(focals))
		)
		# specify upper bounds for the optimization
		upper <- c(
			rep(Inf, nlevels(data$focal)),
			rep(8, length(par)-length(focals))
		)

		# optimze the hell out of this guy
		optim <- try(
			nloptr::sbplx(
				x0=par,
				fn=dev.fun,
				lower=lower,
				upper=upper,
				control=list(ftol_rel=ftol_rel, maxeval=maxeval),
				dimensions=dimensions,
				mm=mm,
				y=y,
				weights=weights,
				family=family,
				focals=focals,
				competitors=competitors,
				trace=TRUE,
			)
		)

		# if the optimization "worked"
		if(!inherits(optim, "try-error")){
			message("Dimension = ", dimensions, " Deviance = ", optim$value)
		
			# convert the parameter vector to what would appear in a glm
			glm.coefs <- glm.coefs.from.traits(optim$par, focals, competitors, dimensions, colnames(mm))

			# use those estimates to calculate the linear predictor
			mu <- family$linkinv(mm %*% glm.coefs)

			# generate a list
			optim <- list(
				mm=mm,
				y=y,
				dimensions=dimensions,
				mu=mu,
				value=optim$value,
				aic=family$aic(y,length(y),mu,weights,optim$value) + 2*length(optim$par),
				par=optim$par,
				coefs=glm.coefs,
				alphas=get.alphas.from.model(glm.coefs, focals, competitors)
			)

			# hess <- numDeriv::hessian(
			# 	dev.fun,
			# 	optim$par,
			# 	dimensions=dimensions,
			# 	mm=mm,
			# 	y=y,
			# 	weights=weights,
			# 	family=family,
			# 	focals=focals,
			# 	competitors=competitors
			# )

			# fisher_info<-solve(hess / 2.0)
			# prop_sigma<-sqrt(diag(fisher_info))
			# prop_sigma<-diag(prop_sigma)
			# optim$lower<-optim$par-1.96*prop_sigma
			# optim$upper<-optim$par+1.96*prop_sigma
		}else{
			warning(
				"optimization failed",
				paste0(" at dimension ", dimensions),
				call. = FALSE,
				immediate. = TRUE
			)
			# generate a list
			optim <- list(
				mm=mm,
				y=y,
				dimensions=dimensions,
				mu=NA,
				value=NA,
				aic=NA,
				par=NA,
				coefs=NA,
				alphas=NA
			)
		}
	}

	return(optim)
}