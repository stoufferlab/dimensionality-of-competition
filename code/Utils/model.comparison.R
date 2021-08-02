
# libraries used below
library(glm2)

# make sure there is a "neighbors" column
fecundity.data$neighbors <- rowSums(fecundity.data[,competitors])

# generic model formulae
model.formula.0 <- as.formula(paste0(fecundity," ~ 0 + target"))
model.formula.1 <- as.formula(paste0(fecundity," ~ 0 + target + neighbors"))
model.formula.2 <- as.formula(paste0(fecundity," ~ 0 + target + target:neighbors"))
model.formula.3 <- as.formula(paste0(fecundity," ~ 0 + target + ",paste0("target:",competitors,collapse=" + ")))

########################################
# POISSON DISTRIBUTION WITH INVERSE LINK
########################################

# a model that just fits intercepts
inverse.poisson.fit.0 <- glm(
	model.formula.0,
	family=poisson(link='inverse'),
	data=fecundity.data,
	control=glm.control(maxit=1000),
	method=glm.fit2
)

# fit a model that lumps all targets together
inverse.poisson.fit.1 <- glm(
	model.formula.1,
	family=poisson(link='inverse'),
	data=fecundity.data,
	etastart=predict(inverse.poisson.fit.0),
	control=glm.control(maxit=1000),
	method=glm.fit2
)

# use the simpler fit as starting values for the more complex model
start.names <- colnames(model.matrix(model.formula.2,fecundity.data))
start <- rep(0,length(start.names))
names(start) <- start.names
intercepts <- grep(":",start.names,value=TRUE,invert=TRUE)
start[intercepts] <- coef(inverse.poisson.fit.1)[intercepts]
for(focal in intercepts){
	start[grep(paste0(focal,":neighbors"),names(start),value=TRUE)] <- coef(inverse.poisson.fit.1)["neighbors"]
}

# fit a model that separates targets but lumps all neighbors together
inverse.poisson.fit.2 <- glm(
	model.formula.2,
	family=poisson(link='inverse'),
	data=fecundity.data,
	start=start,
	control=glm.control(maxit=1000),
	method=glm.fit2
)

# fill in intercepts and species-specific baseline interaction before fitting the full pairwise model
start.names <- colnames(model.matrix(model.formula.3,fecundity.data))
start <- rep(0,length(start.names))
names(start) <- start.names
intercepts <- grep(":",start.names,value=TRUE,invert=TRUE)
start[intercepts] <- coef(inverse.poisson.fit.2)[intercepts]
for(focal in intercepts){
	start[grep(paste0(focal,":"),names(start),value=TRUE)] <- coef(inverse.poisson.fit.2)[paste0(focal,":neighbors")]
}

# fit the full model
inverse.poisson.fit.3 <- glm(
	model.formula.3,
	family=poisson(link='inverse'),
	data=fecundity.data,
	start=start,
	control=list(maxit=1000),
	method=glm.fit2
)
