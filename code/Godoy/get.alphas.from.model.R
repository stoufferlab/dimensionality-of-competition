
# DEBUG we should maybe remove the effect of lambda here!
# given a list of model coefficients, targets, and competitors, return an alpha matrix
get.alphas.from.model <- function(coefs, targets, competitors){
	alphas <- matrix(NA, length(targets), length(competitors)-1)
	rownames(alphas) <- targets
	colnames(alphas) <- competitors[which(competitors!="SOLO")]
	for(j in rownames(alphas)){
		for(k in colnames(alphas)){
			alpha <- coefs[which(names(coefs) == paste0("target",j,":background",k,":neighbours_number"))]
			alpha <- alpha / coefs[paste0("target",j)]
			alphas[j,k] <- alpha
		}
	}
	return(alphas)
}
