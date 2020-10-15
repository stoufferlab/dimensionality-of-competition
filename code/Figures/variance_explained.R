
# library(ggplot2)
library(RColorBrewer)
# library(plot.matrix)

# read in GOF by dimensionality measures
godoyC <- read.table("../../results/Godoy/godoy.C.pseudo-rsquared.csv")
godoyT <- read.table("../../results/Godoy/godoy.T.pseudo-rsquared.csv")

# these datasets were fit from Levine matrix only
wainwright.dir <- "../../results/Wainwright/"
wainwright.files <- list.files(wainwright.dir, "rsquared")
wainwright.files <- paste0(wainwright.dir, wainwright.files)

# these datasets were fit from Levine matrix only
levine.dir <- "../../results/Levine/"
levine.files <- list.files(levine.dir, "rsquared")
levine.files <- paste0(levine.dir, levine.files)

# these datasets were fit from Kinlock matrix only
kinlock.dir <- "../../results/Kinlock/"
kinlock.files <- list.files(kinlock.dir, "rsquared")
kinlock.files <- paste0(kinlock.dir, kinlock.files)

# remove Engel due to errors in the data
kinlock.files <- grep("Engel",kinlock.files,invert=TRUE,value=TRUE)
# remove Goldberg & Landa because we analyse it the other way
kinlock.files <- grep("Landa",kinlock.files,invert=TRUE,value=TRUE)

# where to save the figure
setEPS(width=15, height=15)
postscript('../../manuscript/Supplementary/Figures/variance_explained.eps')

layout(mat = matrix(
		1:36,
		byrow=TRUE,
        nrow = 6, 
        ncol = 6
       ),
       heights = c(1),
       widths = c(2, 2, 2, 2, 2, 2)
)

par(mar = c(2, 2.5, 2, 2.5), oma = c(3.5, 4.5, 0.5, 0.5))

# define some sizes
cex.axis <- 2
padj <- 0.25

# add a color scale for fitness

plot(
	c(0,1:nrow(godoyC)),
	c(0,godoyC[,2]),
	xaxs='i',
	yaxs='i',
	axes=FALSE,
	xlim=c(-0.5,0.5+nrow(godoyC)),
	ylim=c(0,1.0),
	type='l'
)
axis(
	1,
	at=seq(0,1+nrow(godoyC),2),
	tcl=0.5,
	cex.axis=cex.axis,
	padj=padj,
	gap.axis=0
)
axis(
	2,
	at=seq(0,1.0,0.2),
	tcl=0.5,
	cex.axis=cex.axis,
	# padj=padj,
	las=1
)
# abline(v=1,lwd=2.5,lty='dotted')
abline(v=3,lwd=2.5,lty='dashed')
points(
	c(0,1:nrow(godoyC)),
	c(0,godoyC[,2]),
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2,
	xpd=TRUE
)
text("Dataset 1", x=7, y=0.8, xpd=NA, cex=2, font=2, pos=3)

plot(
	c(0,1:nrow(godoyT)),
	c(0,godoyT[,2]),
	xaxs='i',
	yaxs='i',
	axes=FALSE,
	xlim=c(-0.5,0.5+nrow(godoyT)),
	ylim=c(0, 1.0),
	type='l'
)
axis(
	1,
	at=seq(0,1+nrow(godoyT),2),
	tcl=0.5,
	cex.axis=cex.axis,
	padj=padj,
	gap.axis=0
)
axis(
	2,
	at=seq(0,1,0.2),
	tcl=0.5,
	cex.axis=cex.axis,
	# padj=padj,
	las=1
)
# abline(v=1,lwd=2.5,lty='dotted')
abline(v=3,lwd=2.5,lty='dashed')
points(
	c(0,1:nrow(godoyT)),
	c(0,godoyT[,2]),
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2,
	xpd=TRUE
)
text("Dataset 2", x=7, y=0.8, xpd=NA, cex=2, font=2, pos=3)

# other data sets
rsquareds <- c(wainwright.files, levine.files, kinlock.files)

rsquareds <- sapply(rsquareds, function(x) read.table(x), simplify=FALSE)

rsquareds <- rsquareds[order(sapply(rsquareds, nrow), decreasing=TRUE)]

set.seed(123)
pal <- sample(colorRampPalette(brewer.pal(8, "Dark2"))(length(rsquareds)))

for(i in 1:(length(rsquareds))){
	d <- rsquareds[[i]]

	plot(
		c(0,1:nrow(d)),
		c(0,d[,2]),
		xaxs='i',
		yaxs='i',
		axes=FALSE,
		xlim=c(-0.5,0.5+nrow(d)),
		ylim=c(0,1),
		type='l'
	)
	axis(
		1,
		at=-1:(nrow(d)+2),
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

	abline(
		v = igraph::dim_select(d[,1]),
		lwd=2.5,
		lty="dashed"
	)

	points(
		c(0,1:nrow(d)),
		c(0,d[,2]),
		pch=21,
		lwd=1.5,
		bg=pal[i],
		cex=2,
		xpd=TRUE
	)

	text(paste0("Dataset ",i+2), x=0.7*nrow(d), y=0.8, xpd=NA, cex=2, font=2, pos=3)

	if(i == 2){
		mtext("Variance explained per dimension", 2, outer=TRUE, line=1.8, cex=2)
	}

	if(i == 6){
		mtext("Niche dimension", 1, outer=TRUE, line=1.8, cex=2)
	}

}

dev.off()
