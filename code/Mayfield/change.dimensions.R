
change.dimensions <- function(par, targets, competitors, olddimensions, newdimensions){
    if(olddimensions == newdimensions){
        return(par)
    }else{
        # old intercepts
        intercepts.old <- par[seq.int(1,length(targets))]

        # turn the linear version of the response traits into a species by trait matrix
        response.traits.old <- par[seq.int(length(intercepts.old)+1,length(intercepts.old)+length(targets)*olddimensions)]

        # turn the linear version of the effect traits into a species by trait matrix
        effect.traits.old <- par[seq.int(length(intercepts.old)+length(response.traits.old)+1, length(intercepts.old)+length(response.traits.old)+(length(competitors))*olddimensions)]

        if(olddimensions > newdimensions){
            response.traits.new <- response.traits.old[seq.int(1, length(targets)*newdimensions)]
            effect.traits.new <- effect.traits.old[seq.int(1, (length(competitors))*newdimensions)]
        }else{
            response.traits.new <- c(response.traits.old, rep(0, length(targets)*(newdimensions - olddimensions)))
            effect.traits.new <- c(effect.traits.old, rep(0,(length(competitors))*(newdimensions - olddimensions)))
        }
        
        par.new <- c(intercepts.old, response.traits.new, effect.traits.new)

        return(par.new)
    }
}