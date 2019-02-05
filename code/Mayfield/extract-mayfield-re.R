
# extract R/E coefficients and do some stats with them

# # read in Margie's data
# datadir <- "../../data/Mayfield/"
# mayfield <- read.csv(paste0(datadir, "nhood data wTF.csv"))

# cc <- colnames(mayfield)
# cc[5] <- "target"
# colnames(mayfield) <- cc

# # remove NA and 0 
# # WARNING: ask Margie about zero fecundities
# mayfield <- subset(mayfield, !is.na(seeds) & !(seeds==0))

# # the data should now be primed for analysis; woohoo!

# # extract out the min-aic traits
# source('response.effect.from.pars.R')

# read in the best C traits
load("../../results/Mayfield/mayfield.Open.best.Rdata")
ctraits <- Mayfield.best

# read in the best T traits
load("../../results/Mayfield/mayfield.Shade.best.Rdata")
ttraits <- Mayfield.best

# write out fecundities in C and T treatments
write.table(
	cbind(ctraits$lambda, ttraits$lambda),
	"../../results/Mayfield/mayfield.CT.lambdas.csv",
	quote=FALSE,
	col.names=FALSE,
	sep=" ",
	row.names=FALSE
)

# write out all alphas in C and T treatments
# we need to make sure that all alphas match up by comparing row and column names
common.focals <- intersect(rownames(ctraits$alphas), rownames(ttraits$alphas))
common.competitors <- intersect(colnames(ctraits$alphas), colnames(ttraits$alphas))
write.table(
	cbind(
		1/(1+(as.numeric(ctraits$alphas[common.focals, common.competitors]))),
		1/(1+(as.numeric(ttraits$alphas[common.focals, common.competitors])))
	),
	"../../results/Mayfield/mayfield.CT.alphas.csv",
	quote=FALSE,
	col.names=FALSE,
	sep=" ",
	row.names=FALSE
)

# write out OBSERVED alphas in C and T treatments
which.treatment <- "Open"
source('prep.data.R')
source('model.comparison.R')
c.mm <- model.matrix(gamma.fit.2)
c.coefs <- colnames(c.mm)[colSums(c.mm > 0)>=2]

which.treatment <- "Shade"
source('prep.data.R')
source('model.comparison.R')
t.mm <- model.matrix(gamma.fit.2)
t.coefs <- colnames(t.mm)[colSums(t.mm > 0)>=2]

# common and observed interactions
eligible.alphas <- intersect(grep(":",c.coefs,value=TRUE), grep(":",t.coefs,value=TRUE))

# we need to make sure that all alphas match up by comparing row and column names
common.focals <- intersect(rownames(ctraits$alphas), rownames(ttraits$alphas))
common.competitors <- intersect(colnames(ctraits$alphas), colnames(ttraits$alphas))
alpha.table <- c()
for(i in common.focals){
	for(j in common.competitors){
		if(paste0("target",i,":",j) %in% eligible.alphas){
			alpha.table <- rbind(alpha.table, c(ctraits$alphas[i,j], ttraits$alphas[i,j]))
		}
	}
}
alpha.table <- 1 / (1 + alpha.table)
write.table(
	alpha.table,
	"../../results/Mayfield/mayfield.CT.alphas.obs.csv",
	quote=FALSE,
	col.names=FALSE,
	sep=" ",
	row.names=FALSE
)

stop()

####
# conduct procrustes tests between species-species distance matrices
####

## all traits
ttraits.fill <- t(plyr::rbind.fill(as.data.frame(t(ttraits$response)),as.data.frame(t(ttraits$effect[common.competitors,]))))
# fill in missing values with the mean of other species
for(i in 1:ncol(ttraits.fill)){
	ttraits.fill[is.na(ttraits.fill[,i]),i] <- 0 #mean(ttraits.fill[,i],na.rm=TRUE)
}
# ttraits.fill[is.na(ttraits.fill)] <- 0
ctraits.fill <- t(plyr::rbind.fill(as.data.frame(t(ctraits$response)),as.data.frame(t(ctraits$effect[common.competitors,]))))
# fill in missing values with the mean of other species
for(i in 1:ncol(ctraits.fill)){
	ctraits.fill[is.na(ctraits.fill[,i]),i] <- 0 #mean(ctraits.fill[,i],na.rm=TRUE)
}
# ctraits.fill[is.na(ctraits.fill)] <- 0

# procrustes on all traits
proc.test <- vegan::protest(
	dist(ctraits.fill),
	dist(ttraits.fill)	
)

## response traits
proc.test.response <- vegan::protest(
	dist(ctraits$response),
	dist(ttraits$response)
)

## all traits
proc.test.effect <- vegan::protest(
	dist(ctraits$effect[common.competitors,]),
	dist(ttraits$effect[common.competitors,])
)

####
# test the fits of one dataset with the other's min-aic traits
####

stop("NOT WORKING YET WITH REDUCED NUMBER OF COMPETITORS")

# extract and partition out response and effect from different treatments
# Open treatment
c.parms <- Open.optim.lowD[[c.min.aic]]$par
targets <- rownames(Open.optim.lowD[[c.min.aic]]$alphas)
competitors <- colnames(Open.optim.lowD[[c.min.aic]]$alphas)
c.lambda <- c.parms[seq.int(length(targets))]
c.response <- c.parms[seq.int(length(targets)+1,length(targets)+length(targets)*c.min.aic)]
c.effect <- c.parms[seq.int(length(targets)+length(targets)*c.min.aic+1, length(targets)+length(targets)*c.min.aic+(length(competitors))*c.min.aic)]

# Shade treatment
t.parms <- Shade.optim.lowD[[t.min.aic]]$par
targets <- rownames(Shade.optim.lowD[[t.min.aic]]$alphas)
competitors <- colnames(Shade.optim.lowD[[t.min.aic]]$alphas)
t.lambda <- t.parms[seq.int(length(targets))]
t.response <- t.parms[seq.int(length(targets)+1,length(targets)+length(targets)*t.min.aic)]
t.effect <- t.parms[seq.int(length(targets)+length(targets)*t.min.aic+1, length(targets)+length(targets)*t.min.aic+(length(competitors))*t.min.aic)]

source('dev.fun.R')
source('glm.coefs.from.traits.R')
which.family <- poisson()
linkinv <- which.family$linkinv
dev.resids <- which.family$dev.resids
aic <- which.family$aic
model.formula <- as.formula(paste0("seeds ~ 0 + target + ",paste0("target:",competitors,collapse=" + ")))

# use the T data but the C parameters
tmayfield <- subset(mayfield, light=="Open")
x <- model.matrix(model.formula, tmayfield)
y <- tmayfield$seeds

# DEBUG: we cannot just use the parameters because we have to get rid of the lambda effect in the responses
fake.pars <- c(
	t.lambda,
	c.response,
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
cmayfield <- subset(mayfield, light=="Shade")
x <- model.matrix(model.formula, cmayfield)
y <- cmayfield$seeds

# DEBUG: we cannot just use the parameters because we have to get rid of the lambda effect in the responses
fake.pars <- c(
	c.lambda,
	t.response,
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