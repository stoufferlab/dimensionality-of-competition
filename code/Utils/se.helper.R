
library(bbmle)
library(HelpersMG)
library(here)
library(mvtnorm)

source(here('code/Utils/dev.fun.R'))
source(here('code/Utils/get.alphas.from.model.R'))
source(here('code/Utils/glm.coefs.from.traits.R'))
source(here('code/Utils/null.pars.R'))
source(here('code/Utils/response.effect.from.pars.R'))
source(here('code/Utils/cayley.R'))

# estimate the hessian of the fit relative to the parameters
hessian <- optimHess(
	optim.lowD@coef,
	nll.fun,
	family=optim.lowD@data$family,
	X=optim.lowD@data$X,
	y=optim.lowD@data$y,
	targets=optim.lowD@data$targets,
	competitors=optim.lowD@data$competitors,
	dimensions=optim.lowD@data$dimensions,
	control=list(
		trace=5,
		maxit=10000,
		parscale=abs(optim.lowD@coef)
	)
)

# use the hessian to estimate the vcov of the parameters
sigma <- solve(as.matrix(nearPD(hessian)$mat))

# generate samples given the mean and vcov of parameters
samples <- rmvnorm(1000, mean=optim.lowD@coef, sigma=sigma)

# convert these parameters to "human useable" equivalents
refp <- apply(samples, 1, function(x){response.effect.from.pars(x, optim.lowD@data$targets, optim.lowD@data$competitors, optim.lowD@data$dimensions)})

responses <- sapply(refp, function(x,d) x$response[,d], d=which.trait)
effects <- sapply(refp, function(x,d) x$effect[,d], d=which.trait)

# plot(
# 	y=apply(responses, 1, quantile, probs=c(0.5)),
# 	x=apply(effects, 1, quantile, probs=c(0.5)),
# 	xlim=c(-1,1),
# 	ylim=c(-1,1)
# )

# quants <- 0.25
# segments(
# 	apply(effects, 1, quantile, probs=c(quants)),
# 	apply(responses, 1, quantile, probs=c(0.5)),
# 	apply(effects, 1, quantile, probs=c(1-quants)),
# 	apply(responses, 1, quantile, probs=c(0.5))
# )

# segments(
# 	apply(effects, 1, quantile, probs=c(0.5)),
# 	apply(responses, 1, quantile, probs=c(quants)),
# 	apply(effects, 1, quantile, probs=c(0.5)),
# 	apply(responses, 1, quantile, probs=c(1-quants)),
# )

# points(
# 	y=apply(responses, 1, quantile, probs=c(0.5)),
# 	x=apply(effects, 1, quantile, probs=c(0.5))
# )