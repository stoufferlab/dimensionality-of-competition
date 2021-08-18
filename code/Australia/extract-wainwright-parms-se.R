
# extract estimated model coefficients for lowest AIC dimensionality
# and calculate their SE values for when we plot them

# a few utilities we need below
source('../Godoy/response.effect.from.pars.R')
source('../Godoy/polar.transform.R')

for(which.treatment in c('Open','Shade')){

	# read in the wainwright data for the specified treatment and assign some common variables
	source('prep.data.R')

	# read in the absolute best as this will tell us what dimensionality to focus on
	load(paste0("../../results/Wainwright/wainwright.",which.treatment,".best.Rdata"))
	d <- length(Wainwright.best$weights)

	# scrape the shit out of the output files
	saved.fits <- list.files('../../results/Wainwright/', pattern=paste0(which.treatment,'.optim.D',d))
	best.fits <- sapply(
		saved.fits,
		function(x){
			q <- try(load(paste0('../../results/Wainwright/',x)))
			if(inherits(q, "try-error")){
				message(x)
				return(NA)
			}else{
				assign("y", eval(parse(text = paste0(which.treatment,".optim.lowD"))))
				return(y)
			}
		}
	)

	# best.aics <- sapply(
	# 	best.fits,
	# 	function(x) x$aic
	# )

	# best.weights <- exp(-0.5*(best.aics - min(best.aics)))
	# best.weights <- best.weights/sum(best.weights)

	# convert the optim par vectors into intelligible parameter values
	best.pars <- lapply(
		best.fits,
		function(x){
			pars <- response.effect.from.pars(
				x$par,
				targets,
				competitors,
				dimensions=d,
				godoy=FALSE
			)
		}
	)

	# calculate the mean and SE of the lambdas
	lambdas <- t(sapply(
		best.pars,
		function(x) x$lambdas
	))
	lambdas.mean <- colMeans(lambdas)
	lambdas.sd <- apply(lambdas, 2, sd)
	lambdas.se <- lambdas.sd / sqrt(nrow(lambdas))

	# calculate the mean and SE of the responses
	responses <- sapply(
		best.pars,
		function(x) x$response,
		simplify = 'array'
	)
	responses.mean <- apply(responses, c(1,2), mean)
	responses.sd <- apply(responses, c(1,2), sd)
	responses.se <- responses.sd / sqrt(nrow(lambdas))

	# calculate the mean and SE of the effects
	effects <- sapply(
		best.pars,
		function(x) x$effect,
		simplify = 'array'
	)
	effects.mean <- apply(effects, c(1,2), mean)
	effects.sd <- apply(effects, c(1,2), sd)
	effects.se <- effects.sd / sqrt(nrow(lambdas))

	# calculate the mean and SE of the alphas
	alphas <- sapply(
		best.pars,
		function(x) x$alphas,
		simplify = 'array'
	)
	alphas.mean <- apply(alphas, c(1,2), mean)
	alphas.sd <- apply(alphas, c(1,2), sd)
	alphas.se <- alphas.sd / sqrt(nrow(lambdas))

	# convert the above three matrices to long format
	alphas.mean <- setNames(as.data.frame(as.table(alphas.mean)), c("row","col","alpha"))
	alphas.sd <- setNames(as.data.frame(as.table(alphas.sd)), c("row","col","sd"))
	alphas.se <- setNames(as.data.frame(as.table(alphas.se)), c("row","col","se"))

	# determine which alphas actually have corresponding OBSERVATIONAL data
	observed.alphas <- unlist(apply(
		wainwright,
		MAR=1,
		FUN=function(x) {
			if(any(x[competitors]>0)){
				return(paste(x["target"], competitors[which(x[competitors] > 0)], sep=":"))
			}else{
				return(c())
			}
		}
	))
	alphas.mean$observed <- interaction(alphas.mean$row, alphas.mean$col, sep=":") %in% observed.alphas

	# classify a competitor as common if it was observed in three or more plots
	common.competitors <- colSums(wainwright[,competitors])>=3
	alphas.mean$common <- common.competitors[alphas.mean$col]

	# write out lambdas and their SEs
	write.table(
		cbind(
			lambdas.mean,
			lambdas.se
		),
		paste0("../../results/Wainwright/wainwright.",which.treatment,".lambdas.csv"),
		quote=FALSE,
		col.names=TRUE,
		sep=" ",
		row.names=TRUE
	)

	# write out responses, effects, and their SEs across each trait dimension
	for(i in 1:d){
		write.table(
			cbind(
				responses.mean[,d],
				responses.se[,d]
			),
			paste0("../../results/Wainwright/wainwright.",which.treatment,".response.",i,".csv"),
			quote=FALSE,
			col.names=TRUE,
			sep=" ",
			row.names=TRUE
		)
		write.table(
			cbind(
				effects.mean[,d],
				effects.se[,d]
			),
			paste0("../../results/Wainwright/wainwright.",which.treatment,".effect.",i,".csv"),
			quote=FALSE,
			col.names=TRUE,
			sep=" ",
			row.names=TRUE
		)
	}

	# write out alphas and their SEs
	write.table(
		cbind(
			alphas.mean,
			alphas.se$se
		),
		paste0("../../results/Wainwright/wainwright.",which.treatment,".alphas.fit.csv"),
		quote=FALSE,
		col.names=TRUE,
		sep=" ",
		row.names=TRUE
	)

}
