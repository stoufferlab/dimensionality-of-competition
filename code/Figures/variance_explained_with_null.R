
library(RColorBrewer)

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
observed.data <- c(spain.full, australia.full, kinlock.full)

# put the data in a consistent order
observed.data <- observed.data[order(sapply(observed.data, function(x) nrow(x$observed)), decreasing=TRUE)]

# function to do the svd magic
dhat <- function(amat, threshold=0.95){
	SVD <- svd(amat)
	rsq <- (SVD$d**2)/sum((SVD$d**2))
	rsq.cum <- cumsum(rsq)
	dhat <- min(which(rsq.cum > threshold))
	# dhat <- igraph::dim_select(SVD$d)
	# dhat <- weighted.mean(1:length(rsq), rsq)
	return(list(rsq=rsq, rsq.cum=rsq.cum, dhat=dhat))
}

# perform an svd as appropriate
observed.data <- lapply(
	observed.data,
	function(x) {
		return(c(x,dhat(x$observed)))
	}
)

# number of randomizations to use
nrand <- 250

# generate a null distribution of dimensionality values for the kinlock data
null.data <- lapply(
	observed.data,
	function(x, nrand){
		do.call(rbind,sapply(
			1:nrand,
			function(x,observed){
				rand.mat <- matrix(sample(unlist(observed)),nrow(observed),ncol(observed))
				return(dhat(rand.mat)$rsq)
			},
			observed = x$observed,
			simplify=FALSE
		))
	},
	nrand = nrand
)

# where to save the figure
setEPS(width=15, height=15)
postscript('../../manuscript/Supplementary/Figures/variance_explained_with_null.eps')

layout(mat = matrix(
		1:16,
		byrow=TRUE,
        nrow = 4, 
        ncol = 4
       ),
       heights = c(1),
       widths = c(2, 2, 2, 2)
)

par(mar = c(2, 2.5, 2, 2.5), oma = c(3.5, 4.5, 0.5, 0.5))

# define some sizes
cex.axis <- 2
padj <- 0.25

# let the magic happen
set.seed(123)
pal <- sample(colorRampPalette(brewer.pal(8, "Dark2"))(length(observed.data)))

for(i in 1:(length(observed.data))){
	obs <- observed.data[[i]]
	nul <- null.data[[i]]
	spp <- nrow(obs$observed)

	plot(
		NA,
		NA,
		xaxs='i',
		yaxs='i',
		axes=FALSE,
		xlim=c(-0.5,0.5+spp),
		ylim=c(0,1)
	)

	lines(
		c(0,1:spp),
		c(0,obs$rsq)
	)

	axis(
		1,
		at=-1:(spp+2),
		tcl=0.5,
		cex.axis=cex.axis,
		padj=padj
	)
	axis(
		2,
		at=seq(0,1,0.2),
		tcl=0.5,
		cex.axis=cex.axis,
		# padj=padj,
		las=1
	)

	points(
		seq(0,ncol(nul),1),
		c(0,colMeans(nul)),
		pch=22,
		bg='grey',
		type='b',
		cex=1.5,
		col=grey(0.5)
	)

	segments(
		x0=seq(0,ncol(nul),1),
		x1=seq(0,ncol(nul),1),
		y0=c(0,colMeans(nul))+c(0,apply(nul,2,sd)),
		y1=c(0,colMeans(nul))-c(0,apply(nul,2,sd)),
		col=grey(0.5)
	)



	# abline(
	# 	v = obs$dhat,
	# 	lwd=2.5,
	# 	lty="dashed"
	# )

	points(
		c(0,1:spp),
		c(0,obs$rsq),
		pch=21,
		lwd=1.5,
		bg=pal[i],
		cex=2,
		xpd=TRUE
	)

	# for(j in 1:nrow(nul)){
	# 	points(
	# 		seq(0,ncol(nul),1),
	# 		c(0,nul[j,]),
	# 		pch=22,
	# 		bg='grey',
	# 		type='l',
	# 		col=grey(0.5)
	# 	)
	# }

	l <- switch(as.character(i),
		"1" = "1-Wet",
		"2" = "1-Dry",
		"3" = "2-Sun",
		"4" = "2-Shade",
		i - 2
	)
		
	text(paste0("Dataset ",l), x=0.7*spp, y=0.8, xpd=NA, cex=2, font=2, pos=3)

	if(i == 2){
		mtext("Variation explained per niche dimension", 2, outer=TRUE, line=1.8, cex=2)
	}

	if(i == 6){
		mtext("Niche dimension", 1, outer=TRUE, line=1.8, cex=2)
	}

}

dev.off()
