
dev.fun <- function(par, dimensions, x, y, weights, linkinv, dev.resids, targets, competitors, trace=FALSE){
    # stuff from glm.fit that can probably be removed
    # good <- weights > 0
    # varmu <- variance(mu)[good]
    # if (anyNA(varmu)) 
    #     stop("NAs in V(mu)")
    # if (any(varmu == 0)) 
    #     stop("0s in V(mu)")
    # mu.eta.val <- mu.eta(eta)
    # if (any(is.na(mu.eta.val[good]))) 
    #     stop("NAs in d(mu)/d(eta)")
    # good <- (weights > 0) & (mu.eta.val != 0)
    # if (all(!good)) {
    #     conv <- FALSE
    #     warning(gettextf("no observations informative at iteration %d", 
    #       iter), domain = NA)
    #     break
    # }

    coefs <- glm.coefs.from.traits(par, targets, competitors, dimensions, colnames(x))

    eta <- drop(x %*% coefs)
    mu <- linkinv(eta)
    dev <- sum(dev.resids(y, mu, weights))
    if (trace) 
        cat("Dimension = ", dimensions, " Deviance = ", dev,
          "\n", sep = "")
    return(dev)
}
