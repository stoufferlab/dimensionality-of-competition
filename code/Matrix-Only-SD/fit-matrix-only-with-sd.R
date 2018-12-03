
`listToMatrix` <- function(listdata,which.row=1,which.col=2,which.val=3){
        x <- as.factor(listdata[,which.row])
        y <- as.factor(listdata[,which.col])
        z <- listdata[,which.val]

        mat <- matrix(
        	NA,
            nrow=nlevels(x),
            ncol=nlevels(y),
            dimnames=list(levels(x),levels(y))
        )

        mat[cbind(x,y)] <- z
        mat
}

# data files are here
datadir <- "../../data/"

# matrix-only with SD datasets are
datasets <- c(
	"Goldberg/freckleton-2001-competition-coefficients.csv",
	"Levine/levine-2009-competition-coefficients.csv"
)

par(mfrow=c(1,2))
for(dd in datasets){
	# read in the data
	d <- read.csv(paste0(datadir, dd))

	# convert data to mean and sd matrices
	d.mean <- listToMatrix(d,which.val=which(colnames(d)=="Mean"))

	# we need to convert from SE to SD
	if(!"SD" %in% colnames(d)){
		d$"SD" <- d$"SE" * sqrt(d$n)
	}
	d.sd <- listToMatrix(d,which.val=which(colnames(d)=="SD"))

	# generate randomly sampled d matrices
	d.samples <- replicate(
		1000,
		matrix(
			rnorm(
				nrow(d.mean)*ncol(d.mean),
				d.mean,
				d.sd**2
			),
            nrow(d.mean),
            ncol(d.mean)
		),
        simplify=FALSE
    )

	# compute the rsq-like statistic for each dimension of each svd
	rsq <- lapply(
		d.samples,
		function(d){
			SSS <- svd(d) 
			nll <- sum((d - mean(d))^2)
			rsq <- sapply(
				1:length(SSS$d),
				function(x,d,SSS){
					if(x == 1){
						dd <- SSS$u[,1,drop=FALSE] %*% SSS$d[1] %*% t(SSS$v[,1,drop=FALSE])
					}else{
						dd <- SSS$u[,1:x,drop=FALSE] %*% diag(SSS$d[1:x]) %*% t(SSS$v[,1:x,drop=FALSE])
					}
					return(sum((d - dd)^2))
				},
				d=d,
				SSS=SSS
			)
			rsq <- c(0, 1 - rsq / nll)
			return(rsq)
		}
	)

	# put it all together
	rsq <- do.call(rbind, rsq)

	# calculate the mean at each dimensionality
	rsq.mean <- colMeans(rsq)

	# get quantiles for the bounds
	rsq.lb <- apply(rsq, 2, quantile, probs=c(0.025))
	rsq.ub <- apply(rsq, 2, quantile, probs=c(0.975))
	# rsq.sd <- apply(rsq, 2, sd)
	
	# plot the mean values
	plot(0:(ncol(rsq)-1), colMeans(rsq), ylim=c(0.0,1))

	# add some error bars
	segments(
		0:(ncol(rsq)-1),
		rsq.lb,
		0:(ncol(rsq)-1),
		rsq.ub
	)
		# col=col, lty=lty)

	# arbitrary threshold
	abline(h=0.90, lty=3, col='red')

	# write out the results for prettier graph making outside of R
	kkk <- data.frame(
		d=0:(length(rsq[1,])-1),
		rsq=rsq.mean,
		rsq.ub = rsq.ub - rsq.mean,
		rsq.lb = rsq.mean - rsq.lb
	)

	# write the file to the specified directory
	write.table(
		kkk,
		paste0(datadir, strsplit(dd,'/')[[1]][1], "/pseudo-rsquared.csv"),
		quote=FALSE,
		col.names=FALSE,
		sep=" ",
		row.names=FALSE
	)

	# break
}
