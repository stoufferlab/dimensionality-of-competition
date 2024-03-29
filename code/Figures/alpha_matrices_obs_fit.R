
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
	"../../results/Spain/spain.Control.alphas.regression.csv",
	"../../results/Spain/spain.Treatment.alphas.regression.csv",
	"../../results/Australia/australia.Open.alphas.regression.csv",
	"../../results/Australia/australia.Shade.alphas.regression.csv",
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

	title('Observed', line=1.2, cex.main=2.3)
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

# XX

# # add a color scale for fitness

# pal <- c(
# 	rev(colorRampPalette(brewer.pal(9, "Blues"))(88)),
# 	(colorRampPalette(brewer.pal(9, "Reds"))(212))
# )

# # plot the first axis
# plot(
# 	A1,
# 	col=pal,
# 	breaks=seq(-5, 12, length.out=length(pal)),
# 	key=NULL,
# 	xaxt='n',
# 	yaxt='n',
# 	xlab='',
# 	ylab='',
# 	main='',
# 	axes=FALSE,
# 	axis.col=NULL,
# 	axis.row=NULL
# )

# title('(B) First resource (32.6%)', line=1.2, cex.main=2.3)
# mtext("Effect species", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.75)
# mtext("Response species", 2, outer=FALSE, line=1., xpd=NA, cex=1.75)

# # par(mar = c(1, 1, 1, 1))
# plot(
# 	A2,
# 	col=pal,
# 	breaks=seq(-5, 12, length.out=length(pal)),
# 	key=NULL,
# 	xaxt='n',
# 	yaxt='n',
# 	xlab='',
# 	ylab='',
# 	main='',
# 	axes=FALSE,
# 	axis.col=NULL,
# 	axis.row=NULL
# )
# title('(C) Second resource (21.7%)', line=1.2, cex.main=2.3, xpd=NA)
# mtext("Effect species", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.75)
# mtext("Response species", 2, outer=FALSE, line=1., xpd=NA, cex=1.75)
# mtext("+", 2, outer=FALSE, line=3.7, xpd=NA, cex=3.5)

#  #0,max(max(no.comp),max(with.comp)))) # xlab = expression('s'['m']*' = 1 - w'['jm']), ylab=expression('s'['f']*' = 1 - w'['kf']))
# # image(with.comp, xlab = 'z', ylab='Env', main='With competition', col=pal, zlim=c(fmin, fmax)) #, zlim=c(0,max(max(no.comp),max(with.comp)))) # xlab = expression('s'['m']*' = 1 - w'['jm']), ylab=expression('s'['f']*' = 1 - w'['kf']))

# # pal <- brewer.pal(9, "Blues")
# #pal <- brewer.pal(9, "Greys")

# # # plot the difference
# # par(mar = c(4, 0, 5, 7))
# plot(
# 	A3,
# 	col=pal,
# 	breaks=seq(-5, 12, length.out=length(pal)),
# 	key=NULL,
# 	xaxt='n',
# 	yaxt='n',
# 	xlab='',
# 	ylab='',
# 	main='',
# 	axes=FALSE,
# 	axis.col=NULL,
# 	axis.row=NULL
# )
# title('(D) Third resource (16.4%)', line=1.2, cex.main=2.3, xpd=NA)
# mtext("Effect species", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.75)
# mtext("Response species", 2, outer=FALSE, line=1., xpd=NA, cex=1.75)
# mtext("+", 2, outer=FALSE, line=3.7, xpd=NA, cex=3.5)
# # mtext("Environment", 2, outer=FALSE, line=1., xpd=NA, cex=1.5)
# # mtext("Trait", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.5)

# # # plot the difference
# # par(mar = c(4, 0, 5, 7))
# plot(
# 	A,
# 	col=pal,
# 	breaks=seq(-5, 12, length.out=length(pal)),
# 	key=NULL,
# 	xaxt='n',
# 	yaxt='n',
# 	xlab='',
# 	ylab='',
# 	main='',
# 	axes=FALSE,
# 	axis.col=NULL,
# 	axis.row=NULL
# )
# title(main=list(c('a',' Interaction matrix')), line=1.2, cex.main=2.3)
# mtext("Effect species", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.75)
# mtext("Response species", 2, outer=FALSE, line=1., xpd=NA, cex=1.75)
# text(14.6, 5.5, "=", outer=TRUE, line=1, xpd=NA, cex=4.5)
# # mtext("Environment", 2, outer=FALSE, line=1., xpd=NA, cex=1.5)
# # mtext("Trait", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.5)


# # add a color scale for 

# # pal <- brewer.pal(9, "Blues")
# # pal <- colorRampPalette(pal)(10000)
# par(mar = c(5, 1.5, 4.5, 3.0))
# colorbarr <- t(matrix(seq(-5, 12, length.out=length(pal)), length(pal), 1))
# image(
# 	colorbarr,
# 	col=pal,
# 	xaxt='n',
# 	yaxt='n',
# 	mgp=c(0.5,0.5,0.5)
# )
# text(1.75, 0.5, "Interaction strength", outer=FALSE, line=1., xpd=NA, cex=2.25, srt=270)
# mtext("+", 1, outer=FALSE, line=1., xpd=NA, cex=1.75)
# mtext("-", 3, outer=FALSE, line=0.5, xpd=NA, cex=1.75)

# # dev.copy(device=x11)
# # dev.print()#height=3, width=7)

# dev.off()

# # # comp <- expand.grid(z,E)
# # # comp$N <- N[2]
