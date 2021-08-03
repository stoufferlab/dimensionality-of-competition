
# data files are here
datadir <- "../../data/"

# matrix-only datasets from Levine are
datasets <- rbind(
	c("engel-2008.csv",      "yield"),
	c("goldberg-1991.csv",   "yield"),
	c("johannsson-1991.csv", "scaled yield"),
	c("levine-2009.csv",     "invasion growth"),
	c("mitchley-1986.csv",   "relative yield"),
	c("perkins-2007.csv",    "relative yield"),
	c("williams-1962.csv",   "yield"),
	c("wilson-1986.csv",     "relative yield")
)

# read in the data and convert it if/how required
from.levine <- list()
for(i in 1:nrow(datasets)){
	dname <- gsub(".csv","",datasets[i,1])

	# shove everything into this container
	temp.d <- list()

	# add a name for later
	temp.d$Study <- dname

	# read in the data
	temp.d$raw <- read.csv(paste0("../../data/Levine/", datasets[i,1]), row.names=1)

	# convert the raw data to an appropriate format
	if(datasets[i,2] == "relative yield"){
		temp.d$P <- temp.d$raw
	}else
	if(datasets[i,2] == "yield"){
		temp.d$P <- sweep(temp.d$raw, 1, diag(as.matrix(temp.d$raw)), "/")
	}else
	if(datasets[i,2] == "invasion growth"){
		temp.d$P <- sweep(temp.d$raw, 1, diag(as.matrix(temp.d$raw)), "/")
	}else
	if(datasets[i,2] == "scaled yield"){
		temp.d$alpha <- (1/temp.d$raw) - 1
	}else{
		stop("something went wrong")
	}

	# if we have relative yield, flip it for the SVD calculations
	if("P" %in% names(temp.d)){
		temp.d$Q <- 1 / temp.d$P
	}

	from.levine[[dname]] <- temp.d
}

# perform an svd as appropriate given data type
from.levine <- lapply(
	from.levine,
	function(x) {
		if("alpha" %in% names(x)){
			x$SVD <- svd(x$alpha)
		}else{
			x$SVD <- svd(x$P)
		}
		return(x)
	}
)

# convert SVD values to variance explained and write out traits
lapply(
	from.levine,
	function(x){
		prefix <- gsub(" ",".",x$Study)

		# singular value based r squared		
		rsq <- (x$SVD$d**2)/sum((x$SVD$d**2))
		rsq.cum <- cumsum(rsq)
		write.table(
			cbind(x$SVD$d, rsq, rsq.cum),
			file=paste0("../../results/Levine/",prefix,".pseudo-rsquared.csv"),
			quote=FALSE,
			col.names=FALSE,
			sep=" ",
			row.names=FALSE
		)

		# leading traits
		dhat <- igraph::dim_select(x$SVD$d)
		response.traits <- x$SVD$u[,1:dhat,drop=FALSE]
		effect.traits <- x$SVD$v[,1:dhat,drop=FALSE]

		for(i in 1:dhat){
			write.table(
				response.traits[,i],
				file=paste0("../../results/Levine/",prefix,".response.",i,".csv"),
				quote=FALSE,
				col.names=FALSE,
				sep=" ",
				row.names=FALSE
			)
			write.table(
				effect.traits[,i],
				file=paste0("../../results/Levine/",prefix,".effect.",i,".csv"),
				quote=FALSE,
				col.names=FALSE,
				sep=" ",
				row.names=FALSE
			)
		}

		# calculate the low-d approximation of the matrix
		if(dhat>1){
			approximation <- response.traits %*% diag(x$SVD$d[1:dhat]) %*% t(effect.traits)
		}else{
			approximation <- response.traits %*% x$SVD$d[1] %*% t(effect.traits)
		}

		if("alpha" %in% names(x)){
			write.table(
				approximation,
				file=paste0("../../results/Levine/",prefix,".alphas.fit.csv"),
				quote=FALSE,
				col.names=FALSE,
				sep=" ",
				row.names=FALSE
			)

			write.table(
				x$alpha,
				file=paste0("../../results/Levine/",prefix,".alphas.orig.csv"),
				quote=FALSE,
				col.names=FALSE,
				sep=" ",
				row.names=FALSE
			)
		}else{
			write.table(
				approximation,
				file=paste0("../../results/Levine/",prefix,".P.fit.csv"),
				quote=FALSE,
				col.names=FALSE,
				sep=" ",
				row.names=FALSE
			)

			write.table(
				x$P,
				file=paste0("../../results/Levine/",prefix,".P.orig.csv"),
				quote=FALSE,
				col.names=FALSE,
				sep=" ",
				row.names=FALSE
			)
		}

	}
)
