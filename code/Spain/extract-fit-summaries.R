
# a few utilities we need below
library(here)
source(here('code/Utils/cayley.R'))
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
}
