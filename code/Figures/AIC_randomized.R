
library(RColorBrewer)

# find the data files for the synthetic datasets
random.dir <- "../../results/Synthetic-Datasets/"
random.files <- list.files(random.dir, "rsquared", full.names=TRUE)
random.aics <- list.files(random.dir, "AICs", full.names=TRUE)

rsquareds <- sapply(random.files, function(x) read.table(x), simplify=FALSE)
AICs <- sapply(random.aics, function(x) read.table(x), simplify=FALSE)

# where to save the figure
setEPS(width=15, height=12)
postscript('../../manuscript/Supplementary/Figures/AIC_randomized.eps')

layout(mat = matrix(
		1:25,
		byrow=TRUE,
        nrow = 5,
        ncol = 5
       ),
       heights = c(1),
       widths = c(2, 2, 2, 2, 2)
)

par(mar = c(2, 2.5, 2, 4.5), oma = c(3.5, 7.0, 0.5, 0.5))

# define some sizes
cex.axis <- 2
padj <- 0.25

set.seed(321)
pal <- sample(colorRampPalette(brewer.pal(8, "Dark2"))(length(AICs)))

for(i in 1:(length(AICs))){
	d <- AICs[[i]] #rsquareds[[i]]

	plot(
		d[,1],
		d[,2],
		xaxs='i',
		yaxs='i',
		axes=FALSE,
		xlim=c(-0.5,-0.5+nrow(d)),
		ylim=c(-3500,1000),
		type='l'
	)
	axis(
		1,
		at=-1:(nrow(d)),
		tcl=0.5,
		cex.axis=cex.axis,
		padj=padj
	)
	axis(
		2,
		at=seq(-4000,1000,1000),
		tcl=0.5,
		cex.axis=cex.axis,
	# 	# padj=padj,
		las=1
	)

	AAA <- AICs[[i]][,2]
	delAAA <- AAA - min(AAA)
	delAAA[delAAA<2] <- 0
	abline(
		v = AICs[[i]][,1][which.min(delAAA)],
		lwd=2.5,
		lty="dashed"
	)

	points(
		d[,1],
		d[,2],
		pch=21,
		lwd=1.5,
		bg=pal[i],
		cex=2,
		xpd=TRUE
	)

	text(paste0("Random ",i), x=0.4*nrow(d), y=500, xpd=NA, cex=2, font=2, pos=3)
	# text(i, x=0.5*nrow(d), y=500, xpd=NA, cex=2, font=2, pos=3)

	if(i == 2){
		mtext("AIC", 2, outer=TRUE, line=4.2, cex=2)
	}

	if(i == 6){
		mtext("Total niche dimensions", 1, outer=TRUE, line=1.8, cex=2)
	}

}

dev.off()
