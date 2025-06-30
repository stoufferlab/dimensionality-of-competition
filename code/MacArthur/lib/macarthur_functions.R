
require(ecodist)

# generate a set of parameters for MacArthur's consumer-resource model
# sensu MacArthur, R. Species packing and competitive equilibrium for many species. Theor. Popul. Biol. 1, 1–11 (1970).
# S = number of species
# d = number of resources
# l_var = TRUE indicates that resource availabilities should be parameterised at random
# w_var = TRUE indicates that resource values across species should be parameterised at random
# c_var = TRUE indicates that resource utilization across species should be parameterised at random
random_cr_model <- function(S, d, w_var = TRUE, c_var = TRUE, l_var = TRUE){
	# randomly generated "resource value" matrix
	if(w_var){
		w_mat <- matrix(
			runif(S*d),
			S,
			d
		)
	}else{
		w_mat <- matrix(0.5, S, d)
	}

	# randomly generated "resource utilization" matrix
	if(c_var){
		c_mat <- matrix(
			runif(S*d),
			S,
			d
		)
		# normalize such that rows sum to 1
		c_mat <- sweep(
			c_mat,
			1,
			rowSums(c_mat),
			'/'
		)
	}else{
		c_mat <- matrix(0.5, S, d)
	}

	# resource dynamics parameters
	# (i.e., resource carrying capacities divided by resource growth rates)
	if(l_var){
		K_div_rho_l <- runif(d)
		while(any(K_div_rho_l < 0)){
			K_div_rho_l <- runif(d)
		}
	}else{
		K_div_rho_l <- rep(0.5,d)
	}

	return(list(c=c_mat, w=w_mat, K_div_rho=K_div_rho_l))
}

# use the underlying parameterization to get the corresponding alpha matrix
cr_model_A_matrix <- function(cr_parms){
	# resulting consumer-consumer interaction matrix
	A_mat <- sweep(
		(cr_parms$w * cr_parms$c),
		2,
		cr_parms$K_div_rho,
		"*"
	) %*% t(cr_parms$c)

	# additive version partitioned across resource dimensions
	A_mats <- array(NA, dim = c(nrow(A_mat), ncol(A_mat), d))
	for(i in 1:d){
		A_mats[,,i] <- cr_parms$K_div_rho[i] * outer(cr_parms$w[,i] * cr_parms$c[,i], cr_parms$c[,i])
	}

	# note that A_mat == apply(A_mats, c(1,2), sum)
	return(list(A = A_mat, A_d = A_mats))
}

# estimate amount of variation across different components of the randomization
estimate_cr_variation <- function(cr_parms){
	# Bray-Curtis distance across species in terms of resource value
	w_var <- mean(ecodist::bcdist(cr_parms$w))

	# Bray-Curtis distance across species in terms of resource utlization
	c_var <- mean(ecodist::bcdist(cr_parms$c))
	
	# resource dynamics parameters are a single vector so we can just use the variance
	if(nrow(cr_parms$c) == 2){
		l_var <- abs(diff(cr_parms$K_div_rho))
	}
	else{
		l_var <- var(cr_parms$K_div_rho)
	}

	# estimate the A matrix and then perform SVD on it to get the variance explained by the different dimension
	A <- cr_model_A_matrix(cr_parms)$A
	A_singular_vals <- svd(A)$d

	# variance per dimension
	A_var_exp <- A_singular_vals**2 / sum(A_singular_vals**2)

	# cumulative variance as the number of dimensions increases
	A_tot_var <- cumsum(A_var_exp)

	# return results
	res <-list(
		w_var = w_var,
		c_var = c_var,
		l_var = l_var,
		d_var_exp = A_var_exp,
		d_tot_var = A_tot_var
	)

	# # MCT niche overlap
	# if(nrow(A) == 2){
	# 	res$niche_overlap <- sqrt(A[1,2]*A[2,1]/(A[1,1]*A[2,2]))
	# }
	
	return(res)
}

# Get angle between two n-dimensional vectors
angle_btw <- function(v1, v2) {

  signbit <- function(x) {
    x < 0
  }

  u1 <- v1 / norm(v1, "2")
  u2 <- v2 / norm(v2, "2")

  y <- u1 - u2
  x <- u1 + u2

  a0 <- 2 * atan(norm(y, "2") / norm(x, "2"))

  if (!(signbit(a0) || signbit(pi - a0))) {
    a <- a0
  } else if (signbit(a0)) {
    a <- 0.0
  } else {
    a <- pi
  }

  a * 180 / pi
}
