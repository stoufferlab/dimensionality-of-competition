
# see https://github.com/MaterialsDiscovery/PyChemia/blob/b65bfe350003359d2c714588865cdaeabb241675/pychemia/utils/mathematics.py

# given an orthonormal vector convert it to euler angles
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
gea_vector <- function(angles){
	x <- numeric(length(angles))
	x[1] <- sin(angles[1])
	for(i in 2:(length(angles)-1)){
		x[i] <- prod(cos(angles[1:(i-1)])) * sin(angles[i])
	}
	# note to self: I do not understand why we omit the last angle here...
	x[length(angles)] <- prod(cos(angles[1:(length(angles)-1)]))
	return(x)
}

# generate an orthogonal matrix given a comprehensive list of n*(n-1)/2 euler angles
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

# construct a list of angles which define an orthogonal matrix within polar coordinates
gea_all_angles <- function(ortho_matrix){
	# we perform a series of transformations while dropping degrees of freedom in the basis set
	b <- ortho_matrix
	n <- nrow(ortho_matrix)
	ret <- numeric()

	# take the orthogonal vectors from the last column (DEBUG we could/dshould probably change this)
	for(i in seq.int(n-1)){
		x <- b[,ncol(b)]
		angles <- gea_angles(x)
		a <- gea_matrix_a(angles)
		b <- t(b) %*% a
		b <- b[-nrow(b), -ncol(b)]

		ret <- c(ret, angles[seq.int(length(angles)-1)])
	}
	return(ret)
}

# given a list of angles (which are the free parameters) construct an orthogonal matrix right to left
gea_orthogonal_from_angles <- function(angles_list){
	b <- diag(2)
	n <- round(sqrt(length(angles_list)*8+1)/2 + 0.5)

	for(i in seq.int(n-1)){
		# get the angles corresponding to this vector
		angles <- c(angles_list[(length(angles_list)-i+1):length(angles_list)], pi/2)

		# remove these angles since they aren't needed anymore
		angles_list <- angles_list[1:(length(angles_list)-i)]

		# generate a matrix corresponding to this vector
		ma <- gea_matrix_a(angles)

		# something from a paper I should read
		b <- t(b %*% t(ma))

		if(i < n-1){
			c <- diag(i+2)
			c[seq.int(i+1),seq.int(i+1)] <- b
			b <- c
		}
	}
	return(b)
}

# generate random angles for an orthogonal matrix
# this function is largely to test things
gea_random_angles <- function(n, dimensions=n){
	dof <- seq.int(n-1, 1)

	phis <- sapply(
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
	)

	return(phis)
}