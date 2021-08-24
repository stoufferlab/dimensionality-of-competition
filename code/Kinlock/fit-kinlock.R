
# read in the Kinlock data
kinlock <- read.csv("../../data/Kinlock/Data_PlantInteractionNetworks.csv")

# remove small datasets
kinlock <- subset(kinlock, SpeciesRichness >= 3)

# convert Kinlock's RII to alphas or relative yield
kinlock.transformed <- apply(
	kinlock,
	1,
	function(x){
		res <- as.list(x)
		spp <- as.integer(x["SpeciesRichness"])
		if(x["TypeOfControl"] == "True ctrl"){
			res$RII <- matrix(
				as.numeric(strsplit(as.character(x["NetworkTrueCtrl"]),",")[[1]]),
				byrow=TRUE,
				nrow=spp,
				ncol=spp
			)
			res$alphaijNj <- (1 - res$RII)/(1 + res$RII) - 1
			res$usable <- ifelse(any(is.na(res$alphaijNj)),FALSE,TRUE)
		}else{
			res$RII <- matrix(
				as.numeric(strsplit(as.character(x["NetworkMonoCtrl"]),",")[[1]]),
				byrow=TRUE,
				nrow=spp,
				ncol=spp
			)
			res$RelativeYield <- (1 + res$RII)/(1 - res$RII)
			res$usable <- FALSE #ifelse(any(is.na(res$RelativeYield)),FALSE,TRUE)
		}
		
		return(res)
	}
)

# keep only "full" matrices
kinlock.full <- kinlock.transformed[which(sapply(kinlock.transformed,function(x) x$usable))]

# perform an svd as appropriate
kinlock.full <- lapply(
	kinlock.full,
	function(x) {
		if("alphaijNj" %in% names(x)){
			x$SVD <- svd(x$alphaijNj)
		}
		# else{
		# 	x$SVD <- svd(x$RelativeYield)
		# }
		return(x)
	}
)

# convert SVD values to variance explained and write out traits
lapply(
	kinlock.full,
	function(x){
		prefix <- gsub(" ",".",x$Study)

		# singular value based r squared		
		rsq <- (x$SVD$d**2)/sum((x$SVD$d**2))
		rsq.cum <- cumsum(rsq)
		write.table(
			cbind(x$SVD$d, rsq, rsq.cum),
			file=paste0("../../results/Kinlock/",prefix,".pseudo-rsquared.csv"),
			quote=FALSE,
			col.names=FALSE,
			sep=" ",
			row.names=FALSE
		)

		# leading traits
		dhat <- min(which(rsq.cum > 0.95))
		response.traits <- x$SVD$u[,1:dhat,drop=FALSE]
		effect.traits <- x$SVD$v[,1:dhat,drop=FALSE]

		for(i in 1:dhat){
			write.table(
				response.traits[,i],
				file=paste0("../../results/Kinlock/",prefix,".response.",i,".csv"),
				quote=FALSE,
				col.names=FALSE,
				sep=" ",
				row.names=FALSE
			)
			write.table(
				effect.traits[,i],
				file=paste0("../../results/Kinlock/",prefix,".effect.",i,".csv"),
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

		if("alphaijNj" %in% names(x)){
			write.table(
				approximation,
				file=paste0("../../results/Kinlock/",prefix,".alphas.fit.csv"),
				quote=FALSE,
				col.names=FALSE,
				sep=" ",
				row.names=FALSE
			)

			write.table(
				x$alpha,
				file=paste0("../../results/Kinlock/",prefix,".alphas.orig.csv"),
				quote=FALSE,
				col.names=FALSE,
				sep=" ",
				row.names=FALSE
			)
		}else{
			write.table(
				approximation,
				file=paste0("../../results/Kinlock/",prefix,".P.fit.csv"),
				quote=FALSE,
				col.names=FALSE,
				sep=" ",
				row.names=FALSE
			)

			write.table(
				x$P,
				file=paste0("../../results/Kinlock/",prefix,".P.orig.csv"),
				quote=FALSE,
				col.names=FALSE,
				sep=" ",
				row.names=FALSE
			)
		}

	}
)

