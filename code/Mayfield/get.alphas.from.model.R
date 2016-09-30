
# given a list of model coefficients, focals, and competitors, return an alpha matrix
get.alphas.from.model <- function(start, focals, competitors){
	alphas <- matrix(NA, length(focals), length(competitors))
	rownames(alphas) <- focals
	colnames(alphas) <- competitors[which(competitors!="SOLO")]
	for(j in rownames(alphas)){
		for(k in colnames(alphas)){
			alpha <- start[which(names(start) == paste0("focal",j,":",k))]
			alphas[j,k] <- alpha
		}
	}
	return(alphas)
}
