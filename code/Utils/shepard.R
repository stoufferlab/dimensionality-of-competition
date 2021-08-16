
# QR-like parameterization based method using Householder reflectors appearing in
# The multifacet graphically contracted function method. II. A general procedure for the parameterization of orthogonal matrices and its application to arc factors
# The Journal of Chemical Physics (2014)
# http://dx.doi.org/10.1063/1.4890735
# Shepard, Gidofalvi, and Brozell
# and
# The representation and parameterization of orthogonal matrices
# The Journal of Physical Chemistry (2015)
# https://doi.org/10.1021/acs.jpca.5b02015
# Shepard, Brozell, and Gidofalvi

# convert vector of lower triangular values for first d columns of S x S matrix to S x d matrix
# i.e., put those parameters into unit lower triangular matrix (column wise)
Vmatrix <- function(S, d, v){
    V <- matrix(0,nrow=S,ncol=d)
    idx <- 1
    for(i in 1:S){
        for(j in 1:d){
            if(i > j){
                V[i, j] <- v[idx]
                idx <- idx + 1
            }
        }
    }
    return(V)
}

# modified Householder reflector corresponding to kth column of V
Householder <- function(V, k){
    S <- nrow(V)
    if(k<S){
        vk <- V[(1+k):S, k]
        uk <- c(rep(0,k-1),1,vk)
        tauk <- 2 / (1 + sum(vk**2))
        H <- diag(S) - tauk * (uk %*% t(uk))
    }else{
        uk <- c(rep(0,k-1),1)
        tauk <- 2
        H <- diag(S) - tauk * (uk %*% t(uk))
    }
    return(H)
}

# compute product of householder reflections from lower-triangular matrix of parameters
H_prod_right <- function(V){
    S <- nrow(V)
    d <- ncol(V)
    H_prod <- diag(S)
    for(p in d:1){
        H_prod <- Householder(V, p) %*% H_prod
    }
    return(H_prod)
}

# given parameter vector v return S x d semi-orthonormal matrix R (for responses)
Rmatrix <- function(S, d, v){
    V <- Vmatrix(S, d, v)
    H_prod <- H_prod_right(V)
    ones <- diag(1,S,d)
    R <- H_prod %*% ones
    return(R)
}

# given parameter vectors u and v return S x d matrix E (for effects)
# u corresponds to the diagonal
# v corresponds to the lower triangle
Ematrix <- function(S, d, u, v){
    E <- diag(u,nrow=S,ncol=d)
    idx <- 1
    for(i in 1:S){
        for(j in 1:d){
            if (i > j){
                E[i, j] <- v[idx]
                idx <- idx + 1
            }            
        }
    }
    return(E)
}

# # determine the values that produced a semi-orthonormal matrix produced by d Householder transformations (as above)
# InverseRmatrix <- function(R, S, d){
#     vs <- list()
#     ones <- diag(1, S, d)
#     H_prod <- solve()
#     for(p in 1:d){
#         vs[[p]] <- HU[q:nrow(HU),q:ncol(HU),drop=FALSE][,1]
#         H <- Householder2(vs[[q]], D)
#         HU <- solve(a=H, b=HU)
#     }
#     vs <- lapply(vs,
#         function(x){
#             x <- x / x[1]
#             x <- x[-1]
#             return(x)
#         }
#     )
#     vs <- unlist(vs)
#     return(vs)
# }
