
change.dimensions <- function(par, targets, competitors, olddimensions, newdimensions){
    if(olddimensions == newdimensions){
        return(par)
    }else{
        # the intrinsic fecundities come first
        lambdas <- par[seq.int(length(targets))]
        names(lambdas) <- targets

        # "eigenvalues" for the different dimensions come next
        weights <- par[seq.int(length(lambdas)+1, length(lambdas)+olddimensions)]

        # angles for response traits come next
        n.angles <- (choose(length(targets),2) - choose(length(targets)-olddimensions,2))
        response.angles <- par[seq.int(
            from=length(targets)+length(weights)+1,
            to=length(targets)+length(weights)+n.angles
        )]

        # angles for effect traits come last
        # the -1 comes from "background" being listed as a competitor
        n.angles <- (choose(length(competitors)-1,2) - choose(length(competitors)-1-olddimensions,2))
        effect.angles <- par[seq.int(
            from=length(targets)+length(weights)+length(response.angles)+1,
            to=length(targets)+length(weights)+length(response.angles)+n.angles
        )]

        # we need to remove the last set of angles
        if(olddimensions > newdimensions){
            # use the first newdimensions weights
            weights.new <- weights[seq.int(newdimensions)]

            # this is the number of response angles per incremental dimension
            dof <- seq.int(length(targets)-1, 1)
            
            # we use the first newdimensions dof
            dof.new <- sum(dof[seq.int(newdimensions)])

            # use the first response angles
            response.angles.new <- response.angles[seq.int(dof.new)]

            # this is the number of effect angles per incremental dimension
            dof <- seq.int(length(competitors)-1-1, 1)
            
            # we use the first newdimensions dof
            dof.new <- sum(dof[seq.int(newdimensions)])

            # use the first response angles
            effect.angles.new <- effect.angles[seq.int(dof.new)]
        }
        # we need to add new angles at the end
        else{
            # use the first newdimensions weights
            weights.new <- c(weights, rep(-10, newdimensions - olddimensions))

            # this is the number of response angles per incremental dimension
            dof <- seq.int(length(targets)-1, 1)
            
            # we use the first newdimensions dof
            dof.new <- sum(dof[seq.int(newdimensions)])

            # use the first response angles
            response.angles.new <- c(response.angles, rep(0, dof.new - length(response.angles)))

            # this is the number of effect angles per incremental dimension
            dof <- seq.int(length(competitors)-1-1, 1)
            
            # we use the first newdimensions dof
            dof.new <- sum(dof[seq.int(newdimensions)])

            # use the first response angles
            effect.angles.new <- c(effect.angles, rep(0, dof.new - length(effect.angles)))
        }

        names(weights.new) <- paste0("weight",seq.int(newdimensions))
        names(response.angles.new) <- paste0("response",seq.int(length(response.angles.new)))
        names(effect.angles.new) <- paste0("effect",seq.int(length(effect.angles.new)))

        # stich it all back together into one long vector
        par.new <- c(lambdas, weights.new, response.angles.new, effect.angles.new)

        return(par.new)
    }
}