
# a few utilities we need below
library(here)
source(here('code/Utils/cayley.R'))
source(here('code/Utils/get.alphas.from.model.R'))
source(here('code/Utils/response.effect.from.pars.R'))

for(which.treatment in c('C','T')){

	# read in the data for the specified treatment and assign some common variables
	source('prep.data.R')

	# rename the which.treatment variable in full to be consistent with the files elsewhere
	which.treatment <- ifelse(which.treatment == 'C','Control','Treatment')

	# scrape the shit out of the output files
	# and generate a potentially useful table of fit statistics
	all.fits <- lapply(
		seq.int(length(targets)),
		function(d){
			saved.fits <- list.files(
				'../../results/Spain/',
				pattern=paste0(which.treatment,'.optim.D',d,'[.]')
			)
			aics <- sapply(
				saved.fits,
				function(x){
					q <- try(load(paste0('../../results/Spain/',x)))
					# print(paste0('../../results/Spain/',x))
					assign("y", eval(parse(text = paste0(which.treatment,".optim.lowD"))))
					if(!y@details$good){
						ret <- c(
							d,
							NA,
							NA,
							NA,
							rep(NA,length(targets))
						)
						return(ret)
					}else{
						refp <- response.effect.from.pars(
							y@coef,
							targets,
							competitors,
							d
						)
						ret <- c(
							d,
							y@details$deviance,
							y@min,
							AIC(y),
							refp$weights,rep(.Machine$double.eps,length(targets)-d)
						)
						return(ret)
					}
				}
			)
		}
	)

	# pretty up the summary table by making it a data frame with column names
	all.fits <- t(do.call(cbind, all.fits))
	colnames(all.fits) <- c(
		"dimensions",
		"deviance",
		"logLik",
		"AIC",
		paste0("weight.",seq.int(length(targets)))
	)
	all.fits <- data.frame(all.fits)

	# use the singular-value criterion to select the ideal dimensionality
	all.fits$dimselect <- apply(
		all.fits,
		1,
		function(x){
			if(any(is.na(x))){
				return(NA)
			}else{
				cls <- grep("weight",names(x),value=TRUE)
				return(min(x["dimensions"],igraph::dim_select(x[cls])))
			}
		}
	)

	# use the effective singular values to estimate the variance explained by each dimension
	rsq <- t(apply(
		all.fits,
		1,
		function(x){
			if(any(is.na(x))){
				return(rep(NA,length(grep("weight",names(x),value=TRUE))))
			}else{
				cls <- grep("weight",names(x),value=TRUE)
				return(x[cls]**2 / sum(x[cls]**2))
			}	
		}
	))
	colnames(rsq) <- paste0("var.exp.",seq.int(length(targets)))
	all.fits <- cbind(all.fits, rsq)

	# write out fit summaries
	write.table(
		all.fits,
		file=paste0("../../results/Spain/spain.",which.treatment,".fit.summary.csv")
	)

	# fit the regression models as a point of comparison
	source(here('code/Utils/model.comparison.R'))

	# extract and save the "full" model fit alphas
	alphas <- get.alphas.from.model(inverse.poisson.fit.4, 4, targets, competitors)$alphas
	write.table(
		alphas,
		paste0("../../results/Spain/spain.",which.treatment,".alphas.regression.csv")
	)

	# obtain the best full dimension fit and write out its alpha decomposition
	which.fit <- which.min(subset(all.fits, dimensions==max(all.fits$dimensions))$AIC)
	load(paste0('../../results/Spain/',rownames(subset(all.fits, dimensions==max(all.fits$dimensions))[which.fit,])))
	assign("y", eval(parse(text = paste0(which.treatment,".optim.lowD"))))
	refp <- response.effect.from.pars(
		y@coef,
		targets,
		competitors,
		max(all.fits$dimensions)
	)
	# interactions
	write.table(
		refp$alphas,
		paste0("../../results/Spain/spain.",which.treatment,".full.alphas.fit.csv")
	)
	# R^2
	write.table(
			cbind(
				refp$weights,
				refp$weights**2 / sum(refp$weights**2),
				cumsum(refp$weights**2 / sum(refp$weights**2))
			),
			file=paste0(
				"../../results/Spain/spain.",
				which.treatment,
				".pseudo-rsquared.csv"
			),
			row.names=FALSE,
			col.names=FALSE
	)


	# write out the dimension by dimension alphas
	for(d in seq.int(max(all.fits$dimensions))){
		alphas.d <- refp$weights[d] * (refp$response[,d] %*% t(refp$effect[,d]))
		write.table(
			alphas.d,
			paste0("../../results/Spain/spain.",which.treatment,".full.alphas.",d,".fit.csv")
		)
	}

	which.dim <- min(which(cumsum(refp$weights**2 / sum(refp$weights**2))>0.95))

	# obtain the best 1D fit and write out its alpha values
	which.fit <- which.min(subset(all.fits, dimensions==which.dim)$AIC)
	load(paste0('../../results/Spain/',rownames(subset(all.fits, dimensions==which.dim)[which.fit,])))
	assign("y", eval(parse(text = paste0(which.treatment,".optim.lowD"))))
	refp <- response.effect.from.pars(
		y@coef,
		targets,
		competitors,
		which.dim
	)
	write.table(
		refp$alphas,
		paste0("../../results/Spain/spain.",which.treatment,".D",which.dim,".alphas.fit.csv")
	)

}
