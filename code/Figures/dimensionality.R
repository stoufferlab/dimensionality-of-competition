
library(ggplot2)
library(RColorBrewer)
# library(plot.matrix)

# read in GOF by dimensionality measures
godoyC <- read.table("../../results/Godoy/godoy.C.full.weights.csv")
godoyT <- read.table("../../results/Godoy/godoy.T.full.weights.csv")

# these datasets were fit from a matrix only
engel <- read.table("../../results/Engel/pseudo-rsquared.csv")
goldberg <- read.table("../../results/Goldberg/pseudo-rsquared.csv")
johannsson <- read.table("../../results/Johannsson/pseudo-rsquared.csv")
levine <- read.table("../../results/Levine/pseudo-rsquared.csv")
mitchley <- read.table("../../results/Mitchley/pseudo-rsquared.csv")
williams <- read.table("../../results/Williams/pseudo-rsquared.csv")
wilson <- read.table("../../results/Wilson/pseudo-rsquared.csv")

# where to save the figure
pdf('../../manuscript/Figures/AIC/dimensionality.pdf', width=12, height=10)

layout(mat = matrix(
		1:9,
		byrow=TRUE,
        nrow = 3, 
        ncol = 3
       ),
       heights = c(1),
       widths = c(2, 2, 2)
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
	ylim=c(0,0.6),
	type='l'
)
axis(
	1,
	at=c(-1,0,1:(1+nrow(godoyC))),
	tcl=0.5,
	cex.axis=cex.axis,
	padj=padj,
	gap.axis=0
)
axis(
	2,
	at=seq(0,0.6,0.2),
	tcl=0.5,
	cex.axis=cex.axis,
	# padj=padj,
	las=1
)
abline(v=1,lwd=2.5,lty='dotted')
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

plot(
	c(0,1:nrow(godoyT)),
	c(0,godoyT[,2]),
	xaxs='i',
	yaxs='i',
	axes=FALSE,
	xlim=c(-0.5,0.5+nrow(godoyT)),
	ylim=c(0, 0.6),
	type='l'
)
axis(
	1,
	at=c(-1,0,1:(1+nrow(godoyT))),
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
abline(v=1,lwd=2.5,lty='dotted')
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


rsquareds <- list(goldberg, engel, johannsson, levine, mitchley, williams, wilson)
ymax <- c(1,0.6,0.6,0.8,0.8,0.8,0.6)

pal <- brewer.pal(length(rsquareds), "Dark2")

for(i in 1:length(rsquareds)){
	d <- rsquareds[[i]]
	d[,2] <- c(
		0,
		d[2:nrow(d),2] - d[1:(nrow(d)-1),2]
	)

	plot(
		d[,1],
		d[,2],
		xaxs='i',
		yaxs='i',
		axes=FALSE,
		xlim=c(-0.5,-0.5+nrow(d)),
		ylim=c(0,ymax[i]),
		type='l'
	)
	axis(
		1,
		at=c(-1+min(d[,1]),d[,1],1+max(d[,1])),
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

	if(i==1){
		abline(v=0.9,lwd=2.5,lty='dotted')
		abline(v=1.1,lwd=2.5,lty='dashed')
	}else{
		abline(
			v = igraph::dim_select(d[2:nrow(d),2]),
			lwd=2.5,
			lty="dotted"
		)
	}

	points(
		d[,1],
		d[,2],
		pch=21,
		lwd=1.5,
		bg=pal[i],
		cex=2,
		xpd=TRUE
	)

	if(i == 2){
		mtext("Variance explained", 2, outer=TRUE, line=1.8, cex=2)
	}

	if(i == 6){
		mtext("Niche dimension", 1, outer=TRUE, line=1.8, cex=2)
	}

}

dev.off()
