
glm.coefs.from.traits <- function(par, focals, competitors, dimensions, xnames){
    # turn the linear version of the response traits into a species by trait matrix
    response.traits <- matrix(
        par[seq.int(length(focals)+1, length(focals)+length(focals)*dimensions)],
        length(focals),
        dimensions
    )

    # turn the linear version of the effect traits into a species by trait matrix
    effect.traits <- matrix(
        par[seq.int(length(focals)+length(focals)*dimensions+1, length(focals)+length(focals)*dimensions+(length(competitors))*dimensions)],
        length(competitors),
        dimensions
    )

    # matrix multiplication!
    alphas <- response.traits %*% t(effect.traits)
    rownames(alphas) <- focals
    colnames(alphas) <- competitors[competitors!="SOLO"]

    # and back to a linear form for the regression model
    coefs <- numeric(length(focals) + length(focals) * length(competitors))
    names(coefs) <- xnames
    
    # add the lambdas to the appropriate places
    coefs[paste0("focal",focals)] <- par[seq.int(1, length(focals))]

    # add the alphas to the appropriate places
    for(i in rownames(alphas)){
        for(j in colnames(alphas)){
            coefs[paste0("focal",i,":",j)] <- alphas[i,j]
        }
    }

    return(coefs)
}