
# convert a parameter vector from the optimization process to "glm equivalent" coefficients
glm.coefs.from.traits <- function(par, targets, competitors, dimensions, xnames){
    # take the parameter vector and turn it into something more operationalizable
    par <- response.effect.from.pars(par, targets, competitors, dimensions)

    # regression coefficients also take a vector form
    coefs <- numeric(length(targets) + length(targets) * length(competitors))
    names(coefs) <- xnames
    
    # add the lambdas to the appropriate places
    # NOTE: transformation because of the inverse link function used to match Beverton-Holt model
    coefs[paste0("target",targets)] <- 1. / par$lambdas

    # add the alphas to the appropriate places
    # NOTE: transformation because of the inverse link function used to match Beverton-Holt model
    for(i in rownames(par$alphas)){
        for(j in colnames(par$alphas)){
            coefs[paste0("target",i,":",j)] <- (1. / par$lambdas[i]) * par$alphas[i,j]
        }
    }

    return(coefs)
}
