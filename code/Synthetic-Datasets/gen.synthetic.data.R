
# number of fake datasets to build
n.reps <- 25

# number of fake species
n.species <- 7

# faux competitor abundances
n <- c(1, 5, 10, 25, 50, 75, 100, 175, 250)

# this is the basic data frame to be populated
expt.design <- data.frame(
	# focal=as.factor(rep(1:7,length(n))),
	competitor=sort(as.factor(rep(seq(n.species),length(n))))
)
expt.design$abundance <- n

# add treatments across focals
raw.data <- data.frame()
for(i in seq(n.species)){
	expt.design$target <- i
	raw.data <- rbind(raw.data, expt.design)
}
raw.data$target <- as.factor(raw.data$target)

# reorder columns
raw.data <- raw.data[,c('target','competitor','abundance')]

# scrape out the model matrix
mm1 <- model.matrix(as.formula("~0 + target"), raw.data)
mm2 <- model.matrix(as.formula("~0 + target:competitor:abundance"), raw.data)

# create a parameter vector for the intrinsic growth rates
params1 <- numeric(ncol(mm1))
names(params1) <- colnames(mm1)

# create a parameter vector for the alphas
params2 <- numeric(ncol(mm2))
names(params2) <- colnames(mm2)

# generate some fake datasets
for(i in seq(n.reps)){
	# random growth rates
	r <- runif(n.species, 0, 10)
	
	# random interaction coefficients
	a <- matrix(runif(n.species**2), n.species, n.species)
	rownames(a) <- paste0("target",seq(n.species))
	colnames(a) <- paste0("competitor",seq(n.species))

	# fill in the params vector
	params1[paste0("target",seq(n.species))] <- r
	for(j in rownames(a)){
		for(k in colnames(a)){
			params2[paste(j,k,"abundance",sep=":")] <- a[j,k]
		}
	}

	# predicted biomass
	biomass.inv <- 1 / ((mm1 %*% params1) / (1 + mm2 %*% params2))
	biomass <- 1 / rnorm(length(biomass.inv), biomass.inv, 0.01*biomass.inv)

	# now we need to create the full fake data frame
	fake.data <- data.frame(biomass=biomass, target=raw.data$target)
	fake.data[,paste0("sp.",seq(n.species))] <- 0

	for(j in seq(nrow(raw.data))){
		fake.data[j, paste0("sp.",raw.data$competitor[j])] <- raw.data$abundance[j]
	}

	write.csv(fake.data, file=paste0("../../data/Synthetic-Data/synthetic_dataset_",i,".csv"))
}