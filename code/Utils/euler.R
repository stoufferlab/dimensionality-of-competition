
# see https://github.com/MaterialsDiscovery/PyChemia/blob/b65bfe350003359d2c714588865cdaeabb241675/pychemia/utils/mathematics.py
# and Generalization of Euler Angles to N-Dimensional Orthogonal Matrices
#     Journal of Mathematical Physics 13, 528 (1972); https://doi.org/10.1063/1.1666011
#     David K. Hoffman, Richard C. Rafenetti, and Klaus Ruedenberg

# given an orthonormal vector convert it to euler angles
# eq 1 from Hoffman (1972)
gea_angles <- function(uvector){
	n <- length(uvector)
	angles <- numeric()
	for(i in seq.int(n-1)){
		if(uvector[1] < -1){
			uvector[1] <- -1
		}else{
			if(uvector[1] > 1){
				uvector[1] <- 1
			}
		}
		theta <- asin(uvector[1])
		if(length(uvector) == 2 && uvector[2] < 0){
			if(theta > 0){
				theta <- pi - theta
			}else{
				if(theta < 0){
					theta <- -1 * pi - theta
				}
			}
		}
		angles <- c(angles, theta)
		# warning: cos(theta) could be zero!
		uvector <- uvector[2:length(uvector)] / cos(theta)
	}
	angles <- c(angles, pi/2)
	return(angles)
}

# given a set of euler angles convert them to an orthonormal vector
# reverse of eq 1 from Hoffman (1972)
gea_vector <- function(angles){
	x <- numeric(length(angles))
	x[1] <- sin(angles[1])
	for(i in 2:(length(angles)-1)){
		x[i] <- prod(cos(angles[1:(i-1)])) * sin(angles[i])
	}
	x[length(angles)] <- prod(cos(angles[1:(length(angles)-1)]))
	return(x)
}

# generate the matrix a from euler angles to construct orthogonal matrix
# eqs 15-19 from Hoffman (1972)
gea_matrix_a <- function(angles){
	n <- length(angles)
	matrix_a <- diag(n)

	# Region I: elements A_ii
	for(i in seq.int(n-1)){
		matrix_a[i,i] <- cos(angles[i])
	}

	# Region II: elements A_iN
	for(i in seq.int(n)){
		matrix_a[i,n] <- prod(cos(angles[seq.int(i)])) * tan(angles[i])
	}

	# Regions III & IV: elements A_ik
	for(i in seq.int(n)){
		for(k in seq.int(n)){
			if(i > k){
				matrix_a[i,k] <- -1 * prod(cos(angles[seq.int(k,i)])) * tan(angles[i]) * tan(angles[k])
			}else{
				# do nothing because these elements are already zero!
			}
		}
	}

	return(matrix_a)
}

# construct a list of k*(k-1)/2 angles which define a specified orthogonal matrix
# to do so we perform a series of transformations while dropping degrees of freedom in the basis set
gea_all_angles <- function(ortho_matrix){
	b <- ortho_matrix

	# the vectors get taken from right to left which is somewhat counter-intuitive
	# we therefore reverse the column order of the matrix before determining the associated angles
	# b <- b[,order(ncol(b):1)]

	# take the orthogonal vectors right to left
	n <- ncol(ortho_matrix)
	ret <- numeric()
	for(i in seq.int(n-1)){
		x <- b[,ncol(b)]
		angles <- gea_angles(x)
		ret <- c(ret, angles[seq.int(length(angles)-1)])

		a <- gea_matrix_a(angles)
		b <- t(b) %*% a
		b <- b[-nrow(b), -ncol(b)]
	}
	return(ret)
}

# given a list of k*(k-1)/2 angles construct the corresponding orthogonal matrix
gea_orthogonal_from_angles <- function(angles_list){
	b <- diag(2)
	n <- round(sqrt(length(angles_list)*8+1)/2 + 0.5)

	# transformation for each successive orthogonal column
	for(i in seq.int(n-1)){
		# get the angles corresponding to this vector
		angles <- c(angles_list[(length(angles_list)-i+1):length(angles_list)], pi/2)

		# remove these angles since they aren't needed anymore
		angles_list <- angles_list[1:(length(angles_list)-i)]

		# generate a matrix corresponding to this vector
		a <- gea_matrix_a(angles)

		# print(t(b))

		# apply transformation of b by the matrix a
		b <- a %*% t(b)

		# the next b matrix is the transformed matrix with a unit matrix added to it
		if(i < n-1){
			c <- diag(i+2)
			c[seq.int(i+1),seq.int(i+1)] <- b
			b <- c
		}

		# print(t(b))
	}

	# the method constructs from left to right so we reverse columns at the end
	# b <- b[,order(ncol(b):1)]
	return(b)
}

# generate random angles for an orthogonal matrix
# this function is largely to test things
gea_random_angles <- function(n, dimensions=n){
	dof <- seq.int(n-1, 1)

	phis <- unlist(sapply(
		seq.int(length(dof)),
		function(x, dof, d){
			if(x <= d){
				c(runif(dof[x]-1,-pi/2,pi/2),runif(1,-pi,pi))
			}else{
				rep(0,dof[x])
			}
		},
		dof = dof,
		d = dimensions
	))

	return(phis)
}