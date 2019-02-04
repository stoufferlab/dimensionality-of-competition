
# generic model formulae
model.formula.0 <- as.formula(paste0(fecundity," ~ 0 + target"))
model.formula.1 <- as.formula(paste0(fecundity," ~ 0 + target + target:neighbours_number"))
model.formula.2 <- as.formula(paste0(fecundity," ~ 0 + target + target:background:neighbours_number"))
start.names <- colnames(model.matrix(model.formula.2,fecundity.data))
start <- rep(0,length(start.names))
names(start) <- start.names
intercepts <- grep(":",start.names,value=TRUE,invert=TRUE)

# ################
# # Gaussian(Inverse)
# ################

# # a model that just fits intercepts
# gauss.fit.0 <- glm(
# 	model.formula.0,
# 	family=gaussian(link="inverse"),
# 	data=fecundity.data,
# 	control=glm.control(maxit=1000) #,trace=2)
# )

# # fit a model that lumps all neighbors together to get a good starting point
# gauss.fit.1 <- glm(
# 	model.formula.1,
# 	family=gaussian(link="inverse"),
# 	data=fecundity.data,
# 	control=glm.control(maxit=1000) #,trace=2)
# )

# # fill in intercepts before fitting the full pairwise model
# start[intercepts] <- coef(gauss.fit.1)[intercepts]

# # consider filling in the coefficients if we have convergence problems
# # DEBUG

# # fit the full model
# gauss.fit.2 <- glm(
# 	model.formula.2,
# 	family=gaussian(link="inverse"),
# 	data=fecundity.data,
# 	start=start,
# 	control=list(maxit=1000) #,trace=2)
# )

################
# POISSON
################

# a model that just fits intercepts
poisson.fit.0 <- glm(
	model.formula.0,
	family=poisson(),
	data=fecundity.data,
	control=glm.control(maxit=1000) #,trace=2)
)

# fit a model that lumps all neighbors together to get a good starting point
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
	control=list(maxit=1000) #,trace=2)
)

################
# GAMMA
################

# a model that just fits intercepts
gamma.fit.0 <- glm(
	model.formula.0,
	family=Gamma(),
	data=fecundity.data,
	control=glm.control(maxit=1000) #,trace=2)
)

# fit a model that lumps all neighbors together to get a good starting point
gamma.fit.1 <- glm(
	model.formula.1,
	family=Gamma(),,
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
	control=list(maxit=1000) #,trace=2)
)

################
# NEGATIVE BINOMIAL
################

# fit a model that lumps all neighbors together to get a good starting point
# fecundity.data$neighbors <- rowSums(fecundity.data[,competitors])
nb.fit.0 <- mvabund::manyglm(
	model.formula.0,
	data=fecundity.data,
	control=glm.control(maxit=1000) #,trace=2)
)

# fit a model that lumps all neighbors together to get a good starting point
# fecundity.data$neighbors <- rowSums(fecundity.data[,competitors])
nb.fit.1 <- mvabund::manyglm(
	model.formula.1,
	data=fecundity.data,
	control=glm.control(maxit=1000) #,trace=2)
)

# fill in intercepts to ease fitting of more complex model
start[intercepts] <- coef(nb.fit.1)[intercepts]

# consider filling in the coefficients if we have convergence problems
# DEBUG

# refit the full model
nb.fit.2 <- mvabund::manyglm(
	model.formula.2,
	data=fecundity.data,
	start=start1,
	#method=glm.fit3,
	control=list(maxit=1000) #,trace=2)
)
