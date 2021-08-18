
library(bbmle)
library(here)
source(here('code/Utils/dev.fun.R'))
source(here('code/Utils/get.alphas.from.model.R'))
source(here('code/Utils/glm.coefs.from.traits.R'))
source(here('code/Utils/null.pars.R'))
source(here('code/Utils/response.effect.from.pars.R'))
source(here('code/Utils/cayley.R'))

###############################################################################
###############################################################################
### Attempt things by optimizing the traits
###############################################################################
###############################################################################

# the full competition model whose coefficients get replaced with response-effect versions
model.formula <- as.formula(paste0(fecundity," ~ 0 + target + ",paste0("target:",competitors,collapse=" + ")))
X <- model.matrix(model.formula, fecundity.data)
y <- fecundity.data[,fecundity]

# convert the given starting parameters to the format that is used for optimization
par.start <- null.pars(
	targets,
	competitors,
	dimensions,
	lambdas = par.start$lambdas,
	alphas = par.start$alphas
)

# a necessary evil when to using vector parameters and mle2
parnames(dev.fun) <- names(as.list(par.start))

# shove everything through mle2 to register the starting point
optim.lowD <- mle2(
	dev.fun,
	start=par.start,
	data=list(
		family=which.family,
		X=X,
		y=y,
		targets=targets,
		competitors=competitors,
		dimensions=dimensions),
	vecpar=TRUE,
	control=list(
		# trace=5,
		maxit=100000,
		parscale=abs(par.start)
	),
	eval.only = TRUE
)

# print out the initial details prior to optimzation
message("Message: Attempt ",which.n.random," at Dimension = ",dimensions," from Deviance = ",-logLik(optim.lowD))

# optimize with mle2
optim.lowD <- mle2(
	dev.fun,
	start=par.start,
	data=list(
		family=which.family,
		X=X,
		y=y,
		targets=targets,
		competitors=competitors,
		dimensions=dimensions),
	vecpar=TRUE,
	control=list(
		# trace=5,
		maxit=100000,
		parscale=abs(par.start)
	),
	skip.hessian=TRUE
)

# print out the final details after optimization
message("Message: Attempt ",which.n.random," at Dimension = ",dimensions," to   Deviance = ",-logLik(optim.lowD))

# a necessary evil when to using vector parameters and mle2
parnames(nll.fun) <- names(as.list(par.start))

# re-evaluate with loglikelihood and not deviance for SE purposes later since we require the hessian of the nll and not the 
optim.lowD <- mle2(
	nll.fun,
	start=optim.lowD@coef,
	data=list(
		family=which.family,
		X=X,
		y=y,
		targets=targets,
		competitors=competitors,
		dimensions=dimensions),
	vecpar=TRUE,
	control=list(
		# trace=5,
		maxit=100000,
		parscale=abs(par.start)
	),
	eval.only = TRUE
)
