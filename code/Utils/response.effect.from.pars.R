
# convert parameter vector to "human readable" parameters
# note transformations of many parameters to match non-standard bounds
response.effect.from.pars <- function(par, targets, competitors, dimensions){
    # the intrinsic fecundities come first
    # these are log transformed in the optimizer to have no effective bounds
    lambdas <- exp(par[seq.int(length(targets))])
    names(lambdas) <- targets

    # "eigenvalues" for the different dimensions come next
    # these are log transformed in the optimizer to have no effective bounds
    weights <- exp(par[seq.int(length(lambdas)+1, length(lambdas)+dimensions)])

    # angles for response traits come next
    n.angles <- (choose(length(targets),2) - choose(length(targets)-dimensions,2))
    response.angles <- par[seq.int(
        from=length(targets)+length(weights)+1,
        to=length(targets)+length(weights)+n.angles
    )]

    # angles for effect traits come last
    n.angles <- (choose(length(competitors),2) - choose(length(competitors)-dimensions,2))
    effect.angles <- par[seq.int(
        from=length(targets)+length(weights)+length(response.angles)+1,
        to=length(targets)+length(weights)+length(response.angles)+n.angles
    )]

    # now we need to convert the response and effect angles into usable low-dimensional traits
    # we first need to tack on zeros at the end
    response.angles <- c(response.angles, rep(0,choose(length(targets),2) - length(response.angles)))

    # now we turn angles into orthogonal vectors
    response.traits <- gea_orthogonal_from_angles(response.angles)

    # and we use the last columns as the actual response traits
    response.traits <- response.traits[,seq.int(ncol(response.traits),ncol(response.traits)-dimensions+1),drop=FALSE]

    # name things
    rownames(response.traits) <- targets
    colnames(response.traits) <- paste0("response",seq.int(dimensions))

    # and again for the effects
    # we first need to tack on zeros at the end
    effect.angles <- c(effect.angles, rep(0,choose(length(competitors),2) - length(effect.angles)))

    # now we turn angles into orthogonal vectors
    effect.traits <- gea_orthogonal_from_angles(effect.angles)

    # and we use the last columns as the actual response traits
    effect.traits <- effect.traits[,seq.int(ncol(effect.traits),ncol(effect.traits)-dimensions+1),drop=FALSE]

    # name things
    rownames(effect.traits) <- competitors
    colnames(effect.traits) <- paste0("effect",seq.int(dimensions))

    # generate alphas from matrix multiplication!
    alphas <- response.traits %*% diag(weights,dimensions,dimensions) %*% t(effect.traits)
    rownames(alphas) <- targets
    colnames(alphas) <- competitors

    # lets reorder things in decreasing order of the weights across each dimension
    response.traits <- response.traits[,order(weights,decreasing = TRUE),drop=FALSE]
    effect.traits <- effect.traits[,order(weights,decreasing = TRUE),drop=FALSE]
    weights <- weights[order(weights, decreasing = TRUE)]

    return(list(lambdas=lambdas, weights=weights, response=response.traits, effect=effect.traits, alphas=alphas))
}