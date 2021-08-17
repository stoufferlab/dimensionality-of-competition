
# given a regression model, its "type", targets, and competitors,
# return its implied lambdas and alpha matrix
get.alphas.from.model <- function(model, model.type=c(0,1,2,3,4), targets, competitors){
	# TO DO: determine model type directly from model coefficients
	coefs <- coef(model)

	# the intercepts are the easy part
	lambdas <- 1 / coefs[paste0("target",targets)]

	# generate a container for alphas coefficients
	alphas <- matrix(NA, length(targets), length(competitors))
	rownames(alphas) <- targets
	colnames(alphas) <- competitors

	# populate the alpha coefficients as a function of the model construct
	if(model.type == 0){
		alphas[targets, competitors] <- 0
	}else if(model.type == 1){
		alphas[targets, competitors] <- coefs["neighbors"]
	}else if(model.type == 2){
		for(i in targets){
			alphas[i,] <- coefs[paste0("target",i,":neighbors")]
		}
	}else if(model.type == 3){
		for(j in competitors){
			alphas[,j] <- coefs[j]
		}
	}else if(model.type == 4){
		for(i in targets){
			for(j in competitors){
				alphas[i,j] <- coefs[paste0("target",i,":",j)]
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
