
# QR-like parameterization based method using Cayley transformations appearing in
# The representation and parameterization of orthogonal matrices
# The Journal of Physical Chemistry (2015)
# https://doi.org/10.1021/acs.jpca.5b02015
# Shepard, Brozell, and Gidofalvi
# and
# Random orthogonal matrices and the Cayley transform (2018)
# https://arxiv.org/abs/1810.02881
# Jauch, Hoff, and Dunson

# take vector of lower triangular values for first d columns of S x S matrix
# fill matrix column wise and then and convert it to a skew-symmetric S x S matrix
Xmatrix <- function(S, d, v){
    X <- matrix(0,nrow=S,ncol=S)
    idx <- 1
    for(j in 1:d){
        for(i in 1:S){
            if(i > j){
                X[i, j] <- v[idx]
                idx <- idx + 1
            }
        }
    }
    X <- X - t(X)
    return(X)
}

# Cayley transform to produce S x S orthonormal matrix from S x S matrix X
cayley <- function(X){
    S <- nrow(X)
    C <- (diag(S) + X) %*% solve(diag(S) - X)
    return(C)
}

# given parameter vector v return S x d semi-orthonormal matrix R (for responses)
Rmatrix <- function(S, d, v){
    X <- Xmatrix(S, d, v)
    R <- cayley(X) %*% diag(1, S, d)
    return(R)
}

# given parameter vectors u and v return S x d matrix E (for effects)
# u corresponds to the diagonal
# v corresponds to the lower triangle
Ematrix <- function(S, d, u, v){
    E <- diag(u,nrow=S,ncol=d)
    idx <- 1
    for(j in 1:d){
        for(i in 1:S){
            if (i > j){
                E[i, j] <- v[idx]
                idx <- idx + 1
            }            
        }
    }
    return(E)
}

# determine the values that produced a semi-orthonormal matrix produced by modified Cayley transformation (as above)
InverseRmatrix <- function(R, S, d){
    if(d==S) d <- d - 1

    R1 <- R[1:d,1:d,drop=FALSE]
    R2 <- R[(d+1):S,1:d,drop=FALSE]

    F <- (diag(d) - R1) %*% solve(diag(d) + R1)
    B <- 0.5 * (t(F) - F)
    A <- 0.5 * R2 %*% (diag(d) + F)

    X <- rbind(B,A)
    v <- X[lower.tri(X)]

    return(v)
}
