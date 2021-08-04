
# given a list of regression coefficients (with standardized naming scheme), targets, and competitors, return an alpha matrix
get.alphas.from.model <- function(coefs, targets, competitors){
	alphas <- matrix(NA, length(targets), length(competitors))
	rownames(alphas) <- targets
	colnames(alphas) <- competitors
	for(j in rownames(alphas)){
		for(k in colnames(alphas)){
			if(paste0("target",j,":",k) %in% names(coefs)){
				alpha <- coefs[which(names(coefs) == paste0("target",j,":",k))]
				alpha <- alpha / coefs[paste0("target",j)]
			}else{
				alpha <- NA
			}
			alphas[j,k] <- alpha
		}
	}
	return(alphas)
}
