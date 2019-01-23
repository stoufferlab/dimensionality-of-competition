# DEBUG this function should use response.effect.from.pars for the first part!
glm.coefs.from.traits <- function(par, targets, competitors, dimensions, xnames){
    # the intrinsic fecundities come first
    lambdas <- par[seq.int(length(targets))]
    names(lambdas) <- targets

    # "eigenvalues" for the different dimensions come next
    weights <- exp(par[seq.int(length(lambdas)+1, length(lambdas)+dimensions)])

    # angles for response traits come next
    n.angles <- (choose(length(targets),2) - choose(length(targets)-dimensions,2))
    response.angles <- par[seq.int(
        from=length(targets)+length(weights)+1,
        to=length(targets)+length(weights)+n.angles
    )]

    # angles for effect traits come last
    # the -1 comes from "background" being listed as a competitor
    n.angles <- (choose(length(competitors)-1,2) - choose(length(competitors)-1-dimensions,2))
    effect.angles <- par[seq.int(
        from=length(targets)+length(weights)+length(response.angles)+1,
        to=length(targets)+length(weights)+length(response.angles)+n.angles
    )]

    # now we need to convert the response and effect angles into usable low-dimensional traits
    # we first need to tack on zeros at the end
    response.angles <- c(response.angles, rep(0,choose(length(targets),2) - length(response.angles)))

    # now we turn angles into 
    response.traits <- gea_orthogonal_from_angles(response.angles)

    # and we use the last columns as the actual response traits
    response.traits <- response.traits[,seq.int(ncol(response.traits),ncol(response.traits)-dimensions+1),drop=FALSE]

    # and again for the effects
    # we first need to tack on zeros at the end
    effect.angles <- c(effect.angles, rep(0,choose(length(competitors)-1,2) - length(effect.angles)))

    # now we turn angles into 
    effect.traits <- gea_orthogonal_from_angles(effect.angles)

    # and we use the last columns as the actual response traits
    effect.traits <- effect.traits[,seq.int(ncol(effect.traits),ncol(effect.traits)-dimensions+1),drop=FALSE]

    # generate alphas from matrix multiplication!
    alphas <- response.traits %*% diag(weights,dimensions,dimensions) %*% t(effect.traits)
    rownames(alphas) <- targets
    colnames(alphas) <- competitors[competitors!="SOLO"]
    # alphas

    # and back to a linear form for the regression model
    coefs <- numeric(length(targets) + length(targets) * length(competitors))
    names(coefs) <- xnames
    
    # add the lambdas to the appropriate places
    coefs[paste0("target",targets)] <- par[seq.int(1, length(targets))]

    # add the alphas to the appropriate places
    for(i in rownames(alphas)){
        for(j in colnames(alphas)){
            coefs[paste0("target",i,":background",j,":neighbours_number")] <- lambdas[i] * alphas[i,j]
        }
    }

    return(coefs)
}