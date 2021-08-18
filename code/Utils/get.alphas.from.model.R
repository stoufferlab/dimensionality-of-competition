
# given a regression model, its "type", targets, and competitors,
# return its implied lambdas and alpha matrix
get.alphas.from.model <- function(model, model.type=c(0,1,2,3,4), targets, competitors, zeros=FALSE){
	# TO DO: determine model type directly from model coefficients
	coefs <- coef(summary(model))

	# the intercepts are the easy part
	lambdas <- 1 / coefs[paste0("target",targets), "Estimate"]

	# generate a container for alphas coefficients
	alphas <- matrix(0, length(targets), length(competitors))
	rownames(alphas) <- targets
	colnames(alphas) <- competitors

	# populate the alpha coefficients as a function of the model construct
	if(model.type == 0){
		# alphas[targets, competitors] <- 0
	}else if(model.type == 1){
		if(!zeros || coefs["neighbors", "Pr(>|z|)"] < 0.05){
			alphas[targets, competitors] <- coefs["neighbors", "Estimate"]
		}
	}else if(model.type == 2){
		for(i in targets){
			if(!zeros || coefs[paste0("target",i,":neighbors"), "Pr(>|z|)"] < 0.05){
				alphas[i,] <- coefs[paste0("target",i,":neighbors"), "Estimate"]
			}
		}
	}else if(model.type == 3){
		for(j in competitors){
			if(j %in% rownames(coefs)){
				if(!zeros || coefs[j, "Pr(>|z|)"] < 0.05){
					alphas[,j] <- coefs[j, "Estimate"]
				}
			}
		}
	}else if(model.type == 4){
		for(i in targets){
			for(j in competitors){
				if(paste0("target",i,":",j) %in% rownames(coefs)){
					if(!zeros || coefs[paste0("target",i,":",j), "Pr(>|z|)"] < 0.05){
						alphas[i,j] <- coefs[paste0("target",i,":",j), "Estimate"]
					}
				}
			}
		}
	}else{
		alphas <- matrix(NA, length(targets), length(competitors))
	}

	# sweep out the lambda effect
	alphas <- sweep(alphas, 1, lambdas, "*")

	# bundle things together
	parms <- list(
		lambdas=lambdas,
		alphas=alphas
	)
	return(parms)
}
