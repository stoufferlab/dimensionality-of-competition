
# library(ggplot2)
library(RColorBrewer)
library(plot.matrix)

# figure out what alpha files exist

# datasets from the Kinlock collection
kinlock.dir <- "../../results/Kinlock"
kinlock.alphas <- list.files(kinlock.dir,"[.]fit[.]csv",full.names=TRUE)

# put everything together
alpha.fits <- c(
	"../../results/Spain/spain.Control.D2.alphas.fit.csv",
	"../../results/Spain/spain.Treatment.D3.alphas.fit.csv",
	"../../results/Australia/australia.Open.D1.alphas.fit.csv",
	"../../results/Australia/australia.Shade.D2.alphas.fit.csv",
	kinlock.alphas
)

alpha.orig <- c(
	"../../results/Spain/spain.Control.full.alphas.fit.csv",
	"../../results/Spain/spain.Treatment.full.alphas.fit.csv",
	"../../results/Australia/australia.Open.full.alphas.fit.csv",
	"../../results/Australia/australia.Shade.full.alphas.fit.csv",
	gsub("fit", "orig", kinlock.alphas)
)

# determine unique stems/datasets
stems <- unique(sapply(
	alpha.fits,
	function(x){
		strsplit(x,".fit")[[1]][1]
	}
))

# determine the number of species to order correctly relative to other figures
spp <- sapply(
	stems,
	function(stem,alpha.fits){
		if(grepl("spain|australia",stem)){
			100 #nlevels(read.table(grep(stem,alpha.fits,value=TRUE)[1])$row)
		}else{
			nrow(read.table(grep(stem,alpha.fits,value=TRUE)[1]))
		}
	},
	alpha.fits=alpha.fits
)

# reorder in decreasing number of species
stems <- stems[order(spp, decreasing=TRUE)]
alpha.orig <- alpha.orig[order(spp, decreasing=TRUE)]
alpha.fits <- alpha.fits[order(spp, decreasing=TRUE)]

for(i in 1:4){

setEPS(width=8, height=14)
postscript(paste0('../../manuscript/Supplementary/Figures/alphas.',i,'.eps'))

layout(mat = matrix(
		1:12,
		byrow=TRUE,
        nrow = 4, 
        ncol = 3
       ),
       heights = c(1),
       widths = c(2, 2, 0.75)
)

par(mar = c(5, 6, 4.5, 0), oma = c(0, 0, 0, 3.5))

for(j in (1+4*(i-1)):min(4*i,length(stems))){
	stem <- stems[j]
	
	orig <- read.table(alpha.orig[j])
	fit <- read.table(alpha.fits[j])

	if(j %in% c(3,4)){
		orig$Other <- NULL
		fit$Other <- NULL
	}

	orig <- as.matrix(orig)
	fit <- as.matrix(fit)

	min.alpha <- range(orig,-orig,na.rm=TRUE)[1]
	max.alpha <- range(orig,-orig,na.rm=TRUE)[2]

	# add a color scale for interactions
	pal <- c(
		rev(colorRampPalette(brewer.pal(9, "Blues"))(300)),
		(colorRampPalette(brewer.pal(9, "Reds"))(300))
	)
	breaks <- seq(min.alpha, max.alpha, length.out=length(pal))
	
	# plot the original data
	plot.matrix:::plot.matrix(
		orig,
		col=pal,
		breaks=breaks,
		key=NULL,
		xaxt='n',
		yaxt='n',
		xlab='',
		ylab='',
		main='',
		axes=FALSE,
		axis.col=NULL,
		axis.row=NULL,
		na.col='black'
	)

	if(i==1){
		title(paste0(nrow(orig),'-dimension fit'), line=1.2, cex.main=2.3, xpd=TRUE)
	}else{
		title('Observed', line=1.2, cex.main=2.3)
	}
	mtext("Effect species", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.75)
	mtext("Response species", 2, outer=FALSE, line=1., xpd=NA, cex=1.75)

	k <- switch(as.character(j),
		"1" = "1-Wet",
		"2" = "1-Dry",
		"3" = "2-Sun",
		"4" = "2-Shade",
		j - 2
	)
	mtext(paste0("Dataset ",k), 2, outer=FALSE, line=3.5, xpd=NA, cex=1.75, font=2)

	min.alpha <- range(fit,-fit,na.rm=TRUE)[1]
	max.alpha <- range(fit,-fit,na.rm=TRUE)[2]

	# add a color scale for interactions
	pal <- c(
		rev(colorRampPalette(brewer.pal(9, "Blues"))(300)),
		(colorRampPalette(brewer.pal(9, "Reds"))(300))
	)
	breaks <- seq(min.alpha, max.alpha, length.out=length(pal))

	# plot the fit data
	plot.matrix:::plot.matrix(
		fit,
		col=pal,
		breaks=breaks,
		key=NULL,
		xaxt='n',
		yaxt='n',
		xlab='',
		ylab='',
		main='',
		axes=FALSE,
		axis.col=NULL,
		axis.row=NULL
	)

	dhat <- qr(fit)$rank
	
	# title(expression(paste0('Fitted (hat(d)=',dhat,')')), line=1.2, cex.main=2.3)
	# title(expression(bold('Low dimension fit')), line=2.15, cex.main=2.1, xpd=TRUE)
	title(paste0(dhat,'-dimension fit'), line=1.2, cex.main=2.3, xpd=TRUE)
	mtext("Effect species", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.75)
	mtext("Response species", 2, outer=FALSE, line=1., xpd=NA, cex=1.75)

	# par(mar = c(5, 1.5, 4.5, 3.0))
	if(grepl("alpha",stem)){
		colorbarr <- t(matrix(breaks, length(pal), 1))
	}else{
		colorbarr <- t(matrix(rev(breaks), length(pal), 1))
	}
	image(
		colorbarr,
		col=pal,
		xaxt='n',
		yaxt='n',
		mgp=c(0.5,0.5,0.5)
	)
	if(grepl("alpha",stem)){
		text(2.5, 0.5, "Interaction strength", xpd=NA, cex=2.25, srt=270)
	}else{
		text(2.5, 0.5, "Relative performance", xpd=NA, cex=2.25, srt=270)
	}

	mtext("+", 1, outer=FALSE, line=1., xpd=NA, cex=1.75)
	mtext("-", 3, outer=FALSE, line=0.5, xpd=NA, cex=1.75)
	
}

dev.off()

}
