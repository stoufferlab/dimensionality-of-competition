
library(plotrix)

# read in the Kinlock data
kinlock <- read.csv("../../data/Kinlock/Data_PlantInteractionNetworks.csv")

# remove small datasets
kinlock <- subset(kinlock, SpeciesRichness >= 3)

# convert Kinlock's RII to alphas or relative yield
kinlock.transformed <- apply(
	kinlock,
	1,
	function(x){
		res <- as.list(x)
		spp <- as.integer(x["SpeciesRichness"])
		if(x["TypeOfControl"] == "True ctrl"){
			res$RII <- matrix(
				as.numeric(strsplit(as.character(x["NetworkTrueCtrl"]),",")[[1]]),
				byrow=TRUE,
				nrow=spp,
				ncol=spp
			)
			res$alphaijNj <- (1 - res$RII)/(1 + res$RII) - 1
			res$usable <- ifelse(any(is.na(res$alphaijNj)),FALSE,TRUE)
		}else{
			res$RII <- matrix(
				as.numeric(strsplit(as.character(x["NetworkMonoCtrl"]),",")[[1]]),
				byrow=TRUE,
				nrow=spp,
				ncol=spp
			)
			res$RelativeYield <- (1 + res$RII)/(1 - res$RII)
			res$usable <- FALSE #ifelse(any(is.na(res$RelativeYield)),FALSE,TRUE)
		}
		
		return(res)
	}
)

# keep only "full" matrices
kinlock.full <- kinlock.transformed[which(sapply(kinlock.transformed,function(x) x$usable))]

# get rid of unnecessary data
kinlock.full <- lapply(
	kinlock.full,
	function(x) {list(observed = x$alphaijNj)}
)

# read in the full dimension fits of the australian data sets
australia.full <- list()
australia.full[["Open"]] <- list(observed=read.table('../../results/Australia/australia.Open.full.alphas.fit.csv'))
australia.full[["Shade"]] <- list(observed=read.table('../../results/Australia/australia.Shade.full.alphas.fit.csv'))

# read in the full dimension fits of the spanish data sets
spain.full <- list()
spain.full[["Control"]] <- list(observed=read.table('../../results/Spain/spain.Control.full.alphas.fit.csv'))
spain.full[["Treatment"]] <- list(observed=read.table('../../results/Spain/spain.Treatment.full.alphas.fit.csv'))

# combine all observed datasets together
observed.data <- c(kinlock.full, australia.full, spain.full)

dhat <- function(amat, threshold=0.95){
	SVD <- svd(amat)
	rsq <- (SVD$d**2)/sum((SVD$d**2))
	rsq.cum <- cumsum(rsq)
	dhat <- min(which(rsq.cum > threshold))
	# dhat <- igraph::dim_select(SVD$d)
	# dhat <- weighted.mean(1:length(rsq), rsq)
	return(dhat)
}

# perform an svd as appropriate
observed.data <- lapply(
	observed.data,
	function(x) {
		x$dhat <- dhat(x$observed)
		return(x)
	}
)

# number of randomizations to use
nrand <- 250

# generate a null distribution of dimensionality values for the kinlock data
null.data <- lapply(
	observed.data,
	function(x, nrand){
		x$dhat.null <- sapply(
			1:nrand,
			function(x,observed){
				rand.mat <- matrix(sample(unlist(observed)),nrow(observed),ncol(observed))
				return(dhat(rand.mat))
			},
			observed = x$observed
		)
	},
	nrand = nrand
)

# generate some quantities for the plot
spp <- sapply(observed.data, function(x) nrow(x$observed))
obs <- sapply(observed.data, function(x) x$dhat)
null.mean <- sapply(null.data, mean)
null.sd <- sapply(null.data, sd)
null.se <- null.sd / nrand

# make a pretty picture
setEPS(width=4.0, height=3.7)
postscript('../../manuscript/Supplementary/Figures/dimensionality_with_null.eps')

par(mar = c(2, 2.5, 0.5, 1.0), oma = c(2.5, 2.5, 0.75, 1.0))

cex.axis <- 1.7
padj <- 0

plot(
	NA,
	NA,
	xlim=c(1,12),
	ylim=c(1,12),
	# xlab="Species richness",
	# ylab="Dimensionality",
	pch=21,
	bg="#a6cee3",
	xaxs='i',
	yaxs='i',
	type='n',
	axes=FALSE,
	xlab='',
	ylab=''
)

# add the x axis
axis(
	1,
	at=seq(1,12,1),
	labels=c("1","","","4","","","","8","","","","12"),
	tcl=0.5,
	cex.axis=cex.axis,
	padj=padj,
	gap.axis=0
)

# add the y axis
axis(
	2,
	line=0,
	at=seq(1,12,1),
	labels=c("1","","","4","","","","8","","","","12"),
	tcl=0.5,
	cex.axis=cex.axis,
	hadj=0.75,
	padj=0.50,
	las=1,
	gap.axis=0
)

abline(0,1,lty='dotted')

# show what the prediction for the random data is
xpred <- seq(1,12,length.out=1000)

# prediction based on randomized interactions
prediction <- predict(
	glm(
		null.mean ~ 0+I(spp-1),
		family="gaussian",
		offset=rep(1,length(spp))
		# weights=1/null.sd
	),
	newdata=data.frame(spp=xpred),
	#type="response",
	se.fit=TRUE
)

polygon(
	x=c(xpred, rev(xpred)),
	y=(c(prediction$fit+prediction$se.fit, rev(prediction$fit-prediction$se.fit))),
	col=colors()[404],
	border=NA
)

lines(
	x=xpred,
	y=(prediction$fit),
	col='red' #grey(0.25)
)

# prediction for observed data
prediction <- predict(
	glm(obs ~ 0+I(spp-1), family="gaussian", offset=rep(1,length(spp))),
	newdata=data.frame(spp=xpred),
	#type="response",
	se.fit=TRUE
)

polygon(
	x=c(xpred, rev(xpred)),
	y=(c(prediction$fit+prediction$se.fit, rev(prediction$fit-prediction$se.fit))),
	col=grey(0.75),
	border=NA
)

lines(
	x=xpred,
	y=(prediction$fit),
	col=grey(0.25)
)


# segments(
# 	x0=spp,
# 	x1=spp,
# 	y0=obs,
# 	y1=null.mean,
# 	lwd=1.5,
# 	lty=2,
# 	col=grey(0.33)
# )

# segments(
# 	x0=spp,
# 	x1=spp,
# 	y0=obs,
# 	y1=null.mean,
# 	lwd=1.5,
# 	lty=2,
# 	col=grey(0.5)
# )

segments(
	x0=spp,
	x1=spp,
	y0=null.mean + null.sd,
	y1=null.mean - null.sd
)

# spread points out to show redundancy
jitteredxy <- cluster.overplot(
	spp-0.125,
	obs,
	away=c(0.25,0.25)
)

points(
	spp,
	obs,
	pch=21,
	bg="#a6cee3",
	cex=1,
	xpd=TRUE
	# col="#a6cee3"
)

points(
	spp,
	null.mean,
	# col='red',
	pch=22,
	bg='red',
	cex=1
)

mtext("Niche dimensionality", 2, line=2.75, xpd=NA, adj=0.5, cex=2)

mtext("Species richness", 1, outer=FALSE, xpd=NA, line=3.15, cex=2, adj=0.50)

dev.off()
