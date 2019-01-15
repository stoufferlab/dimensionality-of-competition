
# given a list of model coefficients, targets, and competitors, return an alpha matrix
get.alphas.from.model <- function(start, targets, competitors){
	alphas <- matrix(NA, length(targets), length(competitors))
	rownames(alphas) <- targets
	colnames(alphas) <- competitors
	for(j in rownames(alphas)){
		for(k in colnames(alphas)){
			alpha <- start[which(names(start) == paste0("target",j,":",k))]
			alphas[j,k] <- alpha
		}
	}
	return(alphas)
}
