
################
# POISSON
################

# fit a model that lumps all neighbors together to get a good starting point
mayfield$neighbors <- rowSums(mayfield[,competitors])
model.formula <- as.formula("seeds ~ 0 + target + target:neighbors")
poisson.fit.0 <- glm(
	model.formula,
	family=poisson(),
	data=mayfield,
	control=glm.control(maxit=1000) #,trace=2)
)

# specification of the the full pairwise model
model.formula <- as.formula(paste0("seeds ~ 0 + target + ",paste0("target:",competitors,collapse=" + ")))
startnames <- colnames(model.matrix(model.formula,mayfield))
start1 <- rep(0,length(startnames))
names(start1) <- startnames

# fill in intercepts
start1[grep(":",names(start1),value=TRUE,invert=TRUE)] <- coef(poisson.fit.0)[grep(":",names(start1),value=TRUE,invert=TRUE)]

# consider filling in the coefficients if we have convergence problems
# DEBUG

# refit the full model
poisson.fit.1 <- glm(
	model.formula,
	family=poisson(),
	data=mayfield,
	start=start1,
	#method=glm.fit3,
	control=list(maxit=1000) #,trace=2)
)

################
# GAMMA
################

# fit a model that lumps all neighbors together to get a good starting point
# mayfield$neighbors <- rowSums(mayfield[,competitors])
model.formula <- as.formula("seeds ~ 0 + target + target:neighbors")
gamma.fit.0 <- glm(
	model.formula,
	family=Gamma(),
	data=mayfield,
	control=glm.control(maxit=1000) #,trace=2)
)

# specification of the the full pairwise model
model.formula <- as.formula(paste0("seeds ~ 0 + target + ",paste0("target:",competitors,collapse=" + ")))
startnames <- colnames(model.matrix(model.formula,mayfield))
start1 <- rep(0,length(startnames))
names(start1) <- startnames

# fill in intercepts
start1[grep(":",names(start1),value=TRUE,invert=TRUE)] <- coef(gamma.fit.0)[grep(":",names(start1),value=TRUE,invert=TRUE)]

# consider filling in the coefficients if we have convergence problems
# DEBUG

# refit the full model
gamma.fit.1 <- glm(
	model.formula,
	family=Gamma(),
	data=mayfield,
	start=start1,
	#method=glm.fit3,
	control=list(maxit=1000) #,trace=2)
)