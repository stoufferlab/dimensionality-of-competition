
# generic model formulae
model.formula.0 <- as.formula(paste0(fecundity," ~ 0 + target"))
model.formula.1 <- as.formula(paste0(fecundity," ~ 0 + target + target:neighbors"))
model.formula.2 <- as.formula(paste0(fecundity," ~ 0 + target + ",paste0("target:",competitors,collapse=" + ")))
start.names <- colnames(model.matrix(model.formula.2,fecundity.data))
start <- rep(0,length(start.names))
names(start) <- start.names
intercepts <- grep(":",start.names,value=TRUE,invert=TRUE)

################
# LOG-LINEAR
################

# fit a model that has no competitive effects
loglin.fit.0 <- glm(
	model.formula.0,
	family=gaussian(link='log'),
	data=fecundity.data,
	control=glm.control(maxit=1000) #,trace=2)
)

# fit a model that lumps all neighbors together to get a good starting point
fecundity.data$neighbors <- rowSums(fecundity.data[,competitors])
loglin.fit.1 <- glm(
	model.formula.1,
	family=gaussian(link='log'),
	data=fecundity.data,
	control=glm.control(maxit=1000) #,trace=2)
)

# fill in intercepts before fitting the full pairwise model
start[intercepts] <- coef(loglin.fit.1)[intercepts]

# consider filling in the coefficients if we have convergence problems
# DEBUG

# fit the full model
loglin.fit.2 <- glm(
	model.formula.2,
	family=gaussian(link='log'),
	data=fecundity.data,
	start=start,
	#method=glm.fit3,
	control=list(maxit=1000) #,trace=2)
)

################
# POISSON
################

# fit a model that has no competitive effects
poisson.fit.0 <- glm(
	model.formula.0,
	family=poisson(),
	data=fecundity.data,
	control=glm.control(maxit=1000) #,trace=2)
)

# fit a model that lumps all neighbors together to get a good starting point
fecundity.data$neighbors <- rowSums(fecundity.data[,competitors])
poisson.fit.1 <- glm(
	model.formula.1,
	family=poisson(),
	data=fecundity.data,
	control=glm.control(maxit=1000) #,trace=2)
)

# fill in intercepts before fitting the full pairwise model
start[intercepts] <- coef(poisson.fit.1)[intercepts]

# consider filling in the coefficients if we have convergence problems
# DEBUG

# fit the full model
poisson.fit.2 <- glm(
	model.formula.2,
	family=poisson(),
	data=fecundity.data,
	start=start,
	#method=glm.fit3,
	control=list(maxit=1000) #,trace=2)
)

################
# GAMMA
################

# fit a model that has no competitive effects
gamma.fit.0 <- glm(
	model.formula.0,
	family=Gamma(),
	data=fecundity.data,
	control=glm.control(maxit=1000) #,trace=2)
)

# fit a model that lumps all neighbors together to get a good starting point
gamma.fit.1 <- glm(
	model.formula.1,
	family=Gamma(),
	data=fecundity.data,
	control=glm.control(maxit=1000) #,trace=2)
)

# fill in intercepts before fitting the full pairwise model
start[intercepts] <- coef(gamma.fit.1)[intercepts]

# consider filling in the coefficients if we have convergence problems
# DEBUG

# fit the full model
gamma.fit.2 <- glm(
	model.formula.2,
	family=Gamma(),
	data=fecundity.data,
	start=start,
	#method=glm.fit3,
	control=list(maxit=1000) #,trace=2)
)

################
# NEGATIVE BINOMIAL
################

# fit a model with no competitive effects
nb.fit.0 <- mvabund::manyglm(
	model.formula.0,
	data=fecundity.data,
	control=glm.control(maxit=1000) #,trace=2)
)

# fit a model that lumps all neighbors together to get a good starting point
nb.fit.1 <- mvabund::manyglm(
	model.formula.1,
	data=fecundity.data,
	control=glm.control(maxit=1000) #,trace=2)
)

# fill in intercepts before fitting the full pairwise model
start[intercepts] <- coef(nb.fit.1)[intercepts]

# consider filling in the coefficients if we have convergence problems
# DEBUG

# refit the full model
nb.fit.2 <- mvabund::manyglm(
	model.formula.2,
	data=fecundity.data,
	start=start,
	#method=glm.fit3,
	control=list(maxit=1000) #,trace=2)
)

all.models <- list(
	poisson.fit.0 = poisson.fit.0,
	poisson.fit.1 = poisson.fit.1,
	poisson.fit.2 = poisson.fit.2,
	gamma.fit.0 = gamma.fit.0,
	gamma.fit.1 = gamma.fit.1,
	gamma.fit.2 = gamma.fit.2,
	negbin.fit.0 = nb.fit.0,
	negbin.fit.1 = nb.fit.1,
	negbin.fit.2 = nb.fit.2
)

# model.results <- do.call(rbind, lapply(
# 	all.models,
# 	function(x){
# 		c(nrow(coef(su)))
# 	}
# ))