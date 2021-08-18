
# calculate the deviance for the fit according to family's specified deviance function
dev.fun <- function(par, family, X, y, targets, competitors, dimensions, trace=FALSE){
    # convert a long vector of parameters (par) to their glm equivalent
    coefs <- glm.coefs.from.traits(par, targets, competitors, dimensions, colnames(x))

    # calculate the linear predictor
    eta <- drop(x %*% coefs)

    # use the inverse link function to get in the response scale
    mu <- family$linkinv(eta)

    # check whether or not the parameters lead to valid mu values
    validmu <- family$validmu(mu)

    # the total deviance is the sum of deviance residuals
    dev <- ifelse(
        validmu,
        sum(family$dev.resids(y, mu, wt=1)),
        Inf
    )

    # if we are monitoring progress, print out some statistics
    if(trace){
        cat("Dimension = ", dimensions, " Deviance = ", dev, "\n", sep = "")
    }

    # return the deviance value for these parameter values
    return(dev)
}

# calculate the negative log likelihood for the fit
nll.fun <- function(par, family, X, y, targets, competitors, dimensions, trace=FALSE){
    # convert a long vector of parameters (par) to their glm equivalent
    coefs <- glm.coefs.from.traits(par, targets, competitors, dimensions, colnames(x))

    # calculate the linear predictor
    eta <- drop(x %*% coefs)

    # use the inverse link function to get in the response scale
    mu <- family$linkinv(eta)

    # check whether or not the parameters lead to valid mu values
    validmu <- family$validmu(mu)

    # the total deviance is the sum of deviance residuals
    nll <- ifelse(
        validmu,
        -sum(dpois(y, mu, log=TRUE)),
        Inf
    )

    # if we are monitoring progress, print out some statistics
    if(trace){
        cat("Dimension = ", dimensions, " nLL = ", nll, "\n", sep = "")
    }

    # return the nLL value for these parameter values
    return(nll)
}
