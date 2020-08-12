
glm.coefs.from.traits <- function(par, targets, competitors, dimensions, xnames){
    # take the paramater vector and turn it into something "useful"
    par <- response.effect.from.pars(par, targets, competitors, dimensions, godoy=TRUE)

    # and back to a linear form for the regression model
    coefs <- numeric(length(targets) + length(targets) * length(competitors))
    names(coefs) <- xnames
    
    # add the lambdas to the appropriate places
    # DEBUG: note transformation because of Gamma family
    coefs[paste0("target",targets)] <- 1. / par$lambdas

    # add the alphas to the appropriate places
    # DEBUG: note transformation because of Gamma family
    for(i in rownames(par$alphas)){
        for(j in colnames(par$alphas)){
            coefs[paste0("target",i,":background",j,":neighbours_number")] <- (1. / par$lambdas[i]) * par$alphas[i,j]
        }
    }

    return(coefs)
}
