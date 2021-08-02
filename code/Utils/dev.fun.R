
# calculate the deviance for the fit according to family's specified deviance function
dev.fun <- function(par, dimensions, x, y, weights=rep(1,length(y)), family, targets, competitors, trace=FALSE){
    # convert a long vector of parameters (par) to their glm equivalent
    coefs <- glm.coefs.from.traits(par, targets, competitors, dimensions, colnames(x))

    # calculate the linear predictor
    eta <- drop(x %*% coefs)

    # use the inverse link function to get in the response scale
    mu <- family$linkinv(eta)

    # the total deviance is the sum of deviance residuals
    dev <- sum(family$dev.resids(y, mu, weights))

    # if we are monitoring progress, print out some statistics
    if(trace){
        cat("Dimension = ", dimensions, " Deviance = ", dev, "\n", sep = "")
    }

    # return the deviance value for these parameter values
    return(dev)
}
