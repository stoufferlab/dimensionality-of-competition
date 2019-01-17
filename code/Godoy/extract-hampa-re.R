
# extract R/E coefficients for the T treatment

# read in Oscar's data
datadir <- "../../data/Godoy/"
hampa <- read.csv(paste0(datadir, "hampa_neigbours_survey.csv"), row.names=1)

# remove the extra columns at the end
hampa <- hampa[,which(!grepl("X",colnames(hampa)))]

# turn the NAs for background into a non-existent species SOLO which will help us when using the glm function (or equivalents) for fitting
levels(hampa$background) <- c(levels(hampa$background),"SOLO")
hampa$background[which(is.na(hampa$background))] <- "SOLO"

# the data should now be primed for analysis; woohoo!

# extract out the min-aic traits
source('response.effect.from.pars.R')
targets <- levels(hampa$target)
competitors <- levels(hampa$background)

# read in the C traits
load("C.optim.lowD.Rdata")
c.min.aic <- which.min(unlist(lapply(C.optim.lowD, function(x) x$aic)))
ctraits <- response.effect.from.pars(C.optim.lowD[[c.min.aic]]$par, targets, competitors, c.min.aic)

# read in the T traits
load("T.optim.lowD.Rdata")
t.min.aic <- which.min(unlist(lapply(T.optim.lowD, function(x) x$aic)))
ttraits <- response.effect.from.pars(T.optim.lowD[[t.min.aic]]$par, targets, competitors, t.min.aic)

# write out the T traits in convenient formats for figures

# response traits
write.table(ttraits$response, "../../data/Godoy/godoy.T.traits.response.csv", quote=FALSE, col.names=FALSE, sep=" ", row.names=FALSE)

# effect traits
write.table(ttraits$effect, "../../data/Godoy/godoy.T.traits.effect.csv", quote=FALSE, col.names=FALSE, sep=" ", row.names=FALSE)

# response-effect pairs
for(d in 1:ncol(ttraits$response)){
	write.table(
		cbind(ttraits$response[,d], ttraits$effect[,d]),
		paste0("../../data/Godoy/godoy.T.traits.response.effect.",d,".csv"),
		quote=FALSE,
		col.names=FALSE,
		sep=" ",
		row.names=FALSE
	)
}

# write out fecundities in C and T treatments
write.table(
	cbind(ctraits$lambda, ttraits$lambda),
	"../../data/Godoy/godoy.CT.lambdas.csv",
	quote=FALSE,
	col.names=FALSE,
	sep=" ",
	row.names=FALSE
)

# write out alphas in C and T treatments
write.table(
	cbind(
		1/(1+(as.numeric(ctraits$response %*% t(ctraits$effect)))),
		1/(1+(as.numeric(ttraits$response %*% t(ttraits$effect))))
	),
	"../../data/Godoy/godoy.CT.alphas.csv",
	quote=FALSE,
	col.names=FALSE,
	sep=" ",
	row.names=FALSE
)

# conduct procrustes tests between species-species distance matrices

## all traits
proc.test <- vegan::protest(
	dist(cbind(ttraits$response, ttraits$effect)),
	dist(cbind(ctraits$response, ctraits$response))
)

## response traits
proc.test.response <- vegan::protest(
	dist(ttraits$response),
	dist(ctraits$response)
)

## all traits
proc.test.effect <- vegan::protest(
	dist(ttraits$effect),
	dist(ctraits$effect)
)

XX
####
# test the fits of one dataset with the other's min-aic traits
####

# extract and partition out response and effect from different treatments
# C treatment
c.parms <- C.optim.lowD[[c.min.aic]]$par
c.lambda <- c.parms[seq.int(length(targets))]
c.response <- c.parms[seq.int(length(targets)+1,length(targets)+length(targets)*c.min.aic)]
c.effect <- c.parms[seq.int(length(targets)+length(targets)*c.min.aic+1, length(targets)+length(targets)*c.min.aic+(length(competitors)-1)*c.min.aic)]

# T treatment
t.parms <- T.optim.lowD[[t.min.aic]]$par
t.lambda <- t.parms[seq.int(length(targets))]
t.response <- t.parms[seq.int(length(targets)+1,length(targets)+length(targets)*t.min.aic)]
t.effect <- t.parms[seq.int(length(targets)+length(targets)*t.min.aic+1, length(targets)+length(targets)*t.min.aic+(length(competitors)-1)*t.min.aic)]

# remove effect of lambda in response traits
c.response <- c.response / c.lambda
t.response <- t.response / t.lambda

source('dev.fun.R')
source('glm.coefs.from.traits.R')
which.family <- Gamma()
linkinv <- which.family$linkinv
dev.resids <- which.family$dev.resids
dev.resids <- function(y, mu, wt){
	-2  * wt * (log(ifelse(y == 0 | mu < 0, 1, y/mu)) - (y - mu)/mu)
}
aic <- which.family$aic
model.formula <- as.formula("fruits ~ 0 + target + target:background:neighbours_number")

# use the T data but the C parameters
thampa <- subset(hampa, treatment=="T")
x <- model.matrix(model.formula, thampa)
y <- thampa$fruits

# DEBUG: we cannot just use the parameters because we have to get rid of the lambda effect in the responses
fake.pars <- c(
	t.lambda,
	c.response * t.lambda,
	c.effect
)

t.nll <- dev.fun(
	par=fake.pars,
	dimensions=c.min.aic,
	x=x,
	y=y,
	weights=rep(1,length(y)),
	linkinv=linkinv,
	dev.resids=dev.resids,
	targets=targets,
	competitors=competitors
)
t.nll

# use the T data but the C parameters
champa <- subset(hampa, treatment=="C")
x <- model.matrix(model.formula, champa)
y <- champa$fruits

# DEBUG: we cannot just use the parameters because we have to get rid of the lambda effect in the responses
fake.pars <- c(
	c.lambda,
	t.response * c.lambda,
	t.effect
)

c.nll <- dev.fun(
	par=fake.pars,
	dimensions=t.min.aic,
	x=x,
	y=y,
	weights=rep(1,length(y)),
	linkinv=linkinv,
	dev.resids=dev.resids,
	targets=targets,
	competitors=competitors
)
c.nll

# t.aic <- =aic(y,length(y),mu,weights,optim$value) + 2*length(optim$par),

# write.table(cbind(ttraits$response, ttraits$effect), "../../data/Godoy/godoy.T.response.effect.csv", quote=FALSE, col.names=FALSE, sep=" ", row.names=FALSE)