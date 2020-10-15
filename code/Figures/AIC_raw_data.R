
# library(ggplot2)
library(RColorBrewer)
# library(plot.matrix)

# where to save this bastard
setEPS(width=10, height=12)
postscript('../../manuscript/Supplementary/Figures/aic_raw_data.eps')

se.factor <- 2

# layout
layout(mat = matrix(
		1:8,
		byrow=TRUE,
        nrow = 4,
        ncol = 2
       ),
       heights = c(1),
       widths = c(2, 2)
)

# I hate defining plot margins
par(mar = c(2.5, 6.25, 0.5, 1.5), oma = c(1, 4, 0, 0.0))

# read in the info for Dataset 1
AIC <- read.table("../../results/Godoy/godoy.C.AICs.csv")
R2 <- read.table("../../results/Godoy/godoy.C.pseudo-rsquared.csv")

plot(
	AIC,
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2,
	axes=FALSE,
	xlab='',
	ylab='',
	xlim=c(-1,11),
	ylim=c(8250,8500),
	# yaxs='i',
	xaxs='i',
	xpd=TRUE
)

abline(v=3,lty="dashed",lwd=2.5)

# annotate axes
axis(
	1,
	at=seq.int(-2,10,2),
	tcl=0.5,
	cex.axis=1.4,
	padj=-0.5
)
axis(
	2,
	at=seq(8200,8500,50),
	tcl=0.5,
	cex.axis=1.4,
	padj=0.5,
	las=2
)

mtext("AIC", 2, outer=FALSE, line=4., xpd=NA, cex=1.75)
mtext(paste0("Dataset ",1), 2, outer=FALSE, line=6.5, xpd=NA, cex=1.75, font=2)

plot(
	0:nrow(R2),
	c(0,R2[,2]),
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2,
	axes=FALSE,
	xlab='',
	ylab='',
	xlim=c(-1,11),
	ylim=c(0,0.4),
	yaxs='i',
	xaxs='i',
	xpd=TRUE
)

# annotate axes
axis(
	1,
	at=seq.int(-2,10,2),
	tcl=0.5,
	cex.axis=1.4,
	padj=-0.5
)
axis(
	2,
	at=seq(0,0.4,0.1),
	tcl=0.5,
	cex.axis=1.4,
	padj=0.5,
	las=2
)

mtext("Variance explained", 2, outer=FALSE, line=4., xpd=NA, cex=1.75)

# read in the info for Dataset 2
AIC <- read.table("../../results/Godoy/godoy.T.AICs.csv")
R2 <- read.table("../../results/Godoy/godoy.T.pseudo-rsquared.csv")

plot(
	AIC,
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2,
	axes=FALSE,
	xlab='',
	ylab='',
	xlim=c(-1,11),
	ylim=c(3850,4100),
	yaxs='i',
	xaxs='i',
	xpd=TRUE
)

# annotate axes
axis(
	1,
	at=seq.int(-2,10,2),
	tcl=0.5,
	cex.axis=1.4,
	padj=-0.5
)
axis(
	2,
	at=seq(3850,4100,50),
	tcl=0.5,
	cex.axis=1.4,
	padj=0.5,
	las=2
)

abline(v=3,lty="dashed",lwd=2.5)

mtext("AIC", 2, outer=FALSE, line=4., xpd=NA, cex=1.75)
mtext(paste0("Dataset ",2), 2, outer=FALSE, line=6.5, xpd=NA, cex=1.75, font=2)

plot(
	0:nrow(R2),
	c(0,R2[,2]),
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2,
	axes=FALSE,
	xlab='',
	ylab='',
	xlim=c(-1,11),
	ylim=c(0,0.4),
	yaxs='i',
	xaxs='i',
	xpd=TRUE
)

# annotate axes
axis(
	1,
	at=seq.int(-2,10,2),
	tcl=0.5,
	cex.axis=1.4,
	padj=-0.5
)
axis(
	2,
	at=seq(0,0.4,0.1),
	tcl=0.5,
	cex.axis=1.4,
	padj=0.5,
	las=2
)

mtext("Variance explained", 2, outer=FALSE, line=4., xpd=NA, cex=1.75)

# read in the info for Dataset 3
AIC <- read.table("../../results/Wainwright/wainwright.Open.AICs.csv")
R2 <- read.table("../../results/Wainwright/wainwright.Open.pseudo-rsquared.csv")

plot(
	AIC,
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2,
	axes=FALSE,
	xlab='',
	ylab='',
	xlim=c(-1,9),
	# ylim=c(1960,2080),
	yaxs='i',
	xaxs='i',
	xpd=TRUE
)

axis(
	1,
	at=seq.int(-2,10,2),
	tcl=0.5,
	cex.axis=1.4,
	padj=-0.5
)
axis(
	2,
	at=seq(1960,2080,20),
	tcl=0.5,
	cex.axis=1.4,
	padj=0.5,
	las=2
)

abline(v=1,lty="dashed",lwd=2.5)

mtext("AIC", 2, outer=FALSE, line=4., xpd=NA, cex=1.75)
mtext(paste0("Dataset ",3), 2, outer=FALSE, line=6.5, xpd=NA, cex=1.75, font=2)

plot(
	0:nrow(R2),
	c(0,R2[,2]),
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2,
	axes=FALSE,
	xlab='',
	ylab='',
	xlim=c(-1,9),
	ylim=c(0,0.4),
	yaxs='i',
	xaxs='i',
	xpd=TRUE
)

# annotate axes
axis(
	1,
	at=seq.int(-2,10,2),
	tcl=0.5,
	cex.axis=1.4,
	padj=-0.5
)
axis(
	2,
	at=seq(0,0.4,0.1),
	tcl=0.5,
	cex.axis=1.4,
	padj=0.5,
	las=2
)

mtext("Variance explained", 2, outer=FALSE, line=4., xpd=NA, cex=1.75)

# read in the info for Dataset 4
AIC <- read.table("../../results/Wainwright/wainwright.Shade.AICs.csv")
R2 <- read.table("../../results/Wainwright/wainwright.Shade.pseudo-rsquared.csv")

# #
# # interaction strengths
# #

plot(
	AIC,
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2,
	axes=FALSE,
	xlab='',
	ylab='',
	xlim=c(-1,9),
	ylim=c(1940,2040),
	yaxs='i',
	xaxs='i',
	xpd=TRUE
)

axis(
	1,
	at=seq.int(-2,10,2),
	tcl=0.5,
	cex.axis=1.4,
	padj=-0.5
)
axis(
	2,
	at=seq(1920,2060,20),
	tcl=0.5,
	cex.axis=1.4,
	padj=0.5,
	las=2
)

abline(v=1,lty="dashed",lwd=2.5)

mtext("AIC", 2, outer=FALSE, line=4., xpd=NA, cex=1.75)
mtext(paste0("Dataset ",4), 2, outer=FALSE, line=6.5, xpd=NA, cex=1.75, font=2)

plot(
	0:nrow(R2),
	c(0,R2[,2]),
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2,
	axes=FALSE,
	xlab='',
	ylab='',
	xlim=c(-1,9),
	ylim=c(0,0.4),
	yaxs='i',
	xaxs='i',
	xpd=TRUE
)

# annotate axes
axis(
	1,
	at=seq.int(-2,10,2),
	tcl=0.5,
	cex.axis=1.4,
	padj=-0.5
)
axis(
	2,
	at=seq(0,0.4,0.1),
	tcl=0.5,
	cex.axis=1.4,
	padj=0.5,
	las=2
)

mtext("Variance explained", 2, outer=FALSE, line=4., xpd=NA, cex=1.75)

dev.off()
