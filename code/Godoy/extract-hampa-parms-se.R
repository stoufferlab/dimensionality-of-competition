
# extract estimated model coefficients for lowest AIC dimensionality
# and calculate their SE values for when we plot them

# a few utilities we need below
source('./response.effect.from.pars.R')
source('./polar.transform.R')

for(which.treatment in c('C','T')){

	# read in the mayfield data for the specified treatment and assign some common variables
	source('prep.data.R')

	# read in the absolute best as this will tell us what dimensionality to focus on
	load(paste0("../../results/Godoy/godoy.",which.treatment,".best.Rdata"))
	d <- length(Godoy.best$weights)

	# scrape the shit out of the output files
	saved.fits <- list.files('../../results/Godoy/', pattern=paste0(which.treatment,'.optim.D',d))
	best.fits <- sapply(
		saved.fits,
		function(x){
			q <- try(load(paste0('../../results/Godoy/',x)))
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
				godoy=TRUE
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

	# calculate the mean of the weights across dimensions
	weights <- t(sapply(
		best.pars,
		function(x) x$weights,
		simplify = 'array'
	))
	weights.mean <- colMeans(weights)
	
	# calculate the mean and SE of the responses
	responses <- sapply(
		best.pars,
		function(x) x$response, # %*% diag(sqrt(weights.mean)),
		simplify = 'array'
	)
	responses.mean <- apply(responses, c(1,2), mean)
	responses.sd <- apply(responses, c(1,2), sd)
	responses.se <- responses.sd / sqrt(nrow(lambdas))

	# calculate the mean and SE of the effects
	effects <- sapply(
		best.pars,
		function(x) x$effect, # %*% diag(sqrt(weights.mean)),
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

	# write out lambdas and their SEs
	write.table(
		cbind(
			lambdas.mean,
			lambdas.se
		),
		paste0("../../results/Godoy/godoy.",which.treatment,".lambdas.csv"),
		quote=FALSE
	)

	# write out responses, effects, and their SEs across each trait dimension
	for(i in 1:d){
		write.table(
			cbind(
				responses.mean[,i],
				responses.se[,i]
			),
			paste0("../../results/Godoy/godoy.",which.treatment,".response.",i,".csv"),
			quote=FALSE
		)
		write.table(
			cbind(
				effects.mean[,i],
				effects.se[,i]
			),
			paste0("../../results/Godoy/godoy.",which.treatment,".effect.",i,".csv"),
			quote=FALSE
		)
	}

	# write out alphas and their SEs
	write.table(
		cbind(
			alphas.mean,
			alphas.se$se
		),
		paste0("../../results/Godoy/godoy.",which.treatment,".alphas.fit.csv"),
		quote=FALSE
	)

}
