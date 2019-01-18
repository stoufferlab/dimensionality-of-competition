
response.effect.from.pars <- function(par, targets, competitors, dimensions){
    lambda <- 1. / par[seq.int(length(targets))]
    names(lambda) <- targets

    # turn the linear version of the response traits into a species by trait matrix
    response.traits <- matrix(
        par[seq.int(length(targets)+1, length(targets)+length(targets)*dimensions)],
        length(targets),
        dimensions
    )

    # since we use the Gamma form we need to pull out the effect of lambda
    response.traits <- sweep(response.traits, 1, lambda, "/")

    # names help us later
    rownames(response.traits) <- targets

    # turn the linear version of the effect traits into a species by trait matrix
    effect.traits <- matrix(
        par[seq.int(length(targets)+length(targets)*dimensions+1, length(targets)+length(targets)*dimensions+(length(competitors)-1)*dimensions)],
        length(competitors)-1,
        dimensions
    )

    # names help us later
    rownames(effect.traits) <- competitors[competitors!="SOLO"]

    return(list(lambda=lambda, response=response.traits, effect=effect.traits))
}