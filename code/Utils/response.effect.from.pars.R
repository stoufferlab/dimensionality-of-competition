
# convert parameter vector to "human readable" parameters
# note transformations of many parameters to match non-standard bounds
response.effect.from.pars <- function(par, targets, competitors, dimensions){
    # the intrinsic fecundities come first
    # these are constrained to be positive
    lambdas <- exp(par[seq.int(length(targets))])
    names(lambdas) <- targets

    # trim the lambdas off
    par <- par[-seq.int(length(targets))]

    # parameters for the response matrix come next
    response.dof <- length(targets)*dimensions - dimensions*(dimensions+1)/2
    response.params <- par[seq.int(response.dof)]

    # trim the response params off
    par <- par[-seq.int(response.dof)]

    # parameters for the effect diagonal come next
    # these are constrained to be positive
    effect.diag <- pmin(exp(par[seq.int(dimensions)]), .Machine$double.xmax)

    # trim the effect diag off
    par <- par[-seq.int(dimensions)]

    # parameters for the rest of the effect matrix come next
    effect.dof <- length(competitors)*dimensions - dimensions*(dimensions+1)/2
    effect.lower <- par[seq.int(effect.dof)]

    # trim the effect lower off
    par <- par[-seq.int(effect.dof)]

    # convert response parameters to orthogonal R matrix
    R <- Rmatrix(length(targets), dimensions, response.params)

    # convert effect parameters to lower triangular matrix
    E <- Ematrix(length(competitors), dimensions, effect.diag, effect.lower)

    # generate alphas from matrix multiplication!
    alphas <- R %*% t(E)
    rownames(alphas) <- targets
    colnames(alphas) <- competitors

    # perform SVD to get "proper" response and effect traits
    S <- svd(alphas)

    # weights are the singular values
    weights <- S$d[seq.int(dimensions),drop=FALSE]

    # response and effect traits are the columns 1 to dimensions of u and v
    response.traits <- S$u[,seq.int(dimensions),drop=FALSE]
    effect.traits <- S$v[,seq.int(dimensions),drop=FALSE]

    # for uniqueness keep "diagonal" of effect traits positive
    response.traits <- sweep(
        response.traits,
        2,
        sign(diag(effect.traits)),
        "*"
    )
    effect.traits <- sweep(
        effect.traits,
        2,
        sign(diag(effect.traits)),
        "*"
    )

    # assign names to response traits matrix
    rownames(response.traits) <- targets
    colnames(response.traits) <- paste0("response",seq.int(dimensions))

    # assign names to effect traits matrix
    rownames(effect.traits) <- competitors
    colnames(effect.traits) <- paste0("effect",seq.int(dimensions))

    # put all variables together in a list
    ret <- list(
        lambdas=lambdas,
        alphas=alphas,
        response=response.traits,
        effect=effect.traits,
        weights=weights
    )
    return(ret)
}