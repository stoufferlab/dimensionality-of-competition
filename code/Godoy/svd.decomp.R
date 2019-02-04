# ###############################################################################
# ###############################################################################
# ### Attempt things based on an SVD of the alpha matrix
# ###############################################################################
# ###############################################################################
# if(TRUE){
# 	# go through the normal fitting procedure to get the full alpha model to converge as a basis of comparison
# 	model.formula <- as.formula("fruits ~ 0 + target + target:neighbours_number")
# 	gamma.fit.0 <- glm(
# 		model.formula,
# 		family=Gamma(),
# 		data=hampa,
# 		control=glm.control(maxit=1000) #,trace=2)
# 	)

# 	model.formula <- as.formula("fruits ~ 0 + target + target:background:neighbours_number")
# 	startnames <- colnames(model.matrix(model.formula,hampa))
# 	start1 <- as.numeric(coef(gamma.fit.0)[gsub("background[A-Z]{4}:",'',startnames)])
# 	names(start1) <- startnames
# 	start1[grepl("SOLO",names(start1))] <- 0

# 	gamma.fit.1 <- glm(
# 		model.formula,
# 		family=Gamma(),
# 		data=hampa,
# 		start=start1,
# 		#method=glm.fit3,
# 		control=list(maxit=1000) #,trace=2)
# 	)

# 	# what is the deviance of the lowD version of these coefficients?
# 	gamma.lowD<-list()

# 	alphas <- get.alphas.from.model(coef(gamma.fit.1), levels(hampa$target), levels(hampa$background))
# 	S <- svd(alphas)

# 	for(dimensions in seq.int(1,10)){
# 		par <- numeric(nlevels(hampa$target) + nlevels(hampa$target)*dimensions + (nlevels(hampa$background)-1)*dimensions)
# 		par[seq.int(1, nlevels(hampa$target))] <- coef(gamma.fit.1)[paste0("target",levels(hampa$target))]

# 		response.traits <- S$u[,1:dimensions,drop=FALSE] %*% (diag(dimensions) * sqrt(S$d[1:dimensions]))
# 		effect.traits <- S$v[,1:dimensions,drop=FALSE] %*% (diag(dimensions) * sqrt(S$d[1:dimensions]))

# 		par[seq.int(nlevels(hampa$target)+1, length(par))] <- c(as.vector(response.traits), as.vector(effect.traits))

# 		x <- model.matrix(model.formula, hampa)
# 		y <- hampa$fruits
# 		weights <- rep(1,length(y))

# 		linkinv <- Gamma()$linkinv
# 		dev.resids <- Gamma()$dev.resids
# 		aic <- Gamma()$aic
# 		targets <- levels(hampa$target)
# 		competitors <- levels(hampa$background)

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
