
glm.coefs.from.traits <- function(par, targets, competitors, dimensions, xnames){
    # turn the linear version of the response traits into a species by trait matrix
    response.traits <- matrix(
        par[seq.int(length(targets)+1, length(targets)+length(targets)*dimensions)],
        length(targets),
        dimensions
    )

    # turn the linear version of the effect traits into a species by trait matrix
    effect.traits <- matrix(
        par[seq.int(length(targets)+length(targets)*dimensions+1, length(targets)+length(targets)*dimensions+(length(competitors))*dimensions)],
        length(competitors),
        dimensions
    )

    # matrix multiplication!
    alphas <- response.traits %*% t(effect.traits)
    rownames(alphas) <- targets
    colnames(alphas) <- competitors #[competitors!="SOLO"]

    # and back to a linear form for the regression model
    coefs <- numeric(length(targets) + length(targets) * length(competitors))
    names(coefs) <- xnames
    
    # add the lambdas to the appropriate places
    coefs[paste0("target",targets)] <- par[seq.int(1, length(targets))]

    # add the alphas to the appropriate places
    for(i in rownames(alphas)){
        for(j in colnames(alphas)){
            coefs[paste0("target",i,":",j)] <- alphas[i,j]
        }
    }

    return(coefs)
}