
glm.coefs.from.traits <- function(par, targets, competitors, dimensions, xnames){
    # take the paramater vector and turn it into something "useful"
    par <- response.effect.from.pars(par, targets, competitors, dimensions)

    # and back to a linear form for the regression model
    coefs <- numeric(length(targets) + length(targets) * length(competitors))
    names(coefs) <- xnames
    
    # add the lambdas to the appropriate places
    # NOTE: transformation because of the inverse link function
    coefs[paste0("target",targets)] <- 1. / par$lambdas

    # add the alphas to the appropriate places
    # NOTE: transformation because of the inverse link function
    for(i in rownames(par$alphas)){
        for(j in colnames(par$alphas)){
            coefs[paste0("target",i,":",j)] <- (1. / par$lambdas[i]) * par$alphas[i,j]
        }
    }

    return(coefs)
}
