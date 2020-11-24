
library(RColorBrewer)
# library(plot.matrix)

# read in the response/effect pairs for the best fitting model
R1 <- read.table("../../results/Godoy/godoy.T.response.1.csv")
R2 <- read.table("../../results/Godoy/godoy.T.response.2.csv")
R3 <- read.table("../../results/Godoy/godoy.T.response.3.csv")

E1 <- read.table("../../results/Godoy/godoy.T.effect.1.csv")
E2 <- read.table("../../results/Godoy/godoy.T.effect.2.csv")
E3 <- read.table("../../results/Godoy/godoy.T.effect.3.csv")

se.factor <- 2

setEPS(width=12, height=3.5)
postscript('../../manuscript/Figures/response.effect.godoy.T.eps')

layout(mat = matrix(
		c(1, 2, 3), 
        nrow = 1, 
        ncol = 3
       ),
       heights = c(1),
       widths = c(2, 2, 2)
)

par(mar = c(5, 4.5, 4.5, 1), oma = c(0, 1.25, 0, 0.5))

# add a color scale for fitness

plot(
	R1[,1] ~ E1[,1],
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2,
	axes=FALSE,
	xlab='',
	ylab='',
	xlim=c(-0.6,0.6),
	ylim=c(-0.6,0.6),
	yaxs='i',
	xaxs='i',
	type='n'
)
title('a  First dimension', line=1.2, cex.main=2.2)
mtext("Response trait", 2, outer=FALSE, line=4.0, xpd=NA, cex=1.5)
# mtext("Trait", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.5)
mtext("Effect trait", 1, outer=FALSE, line=2.75, xpd=NA, cex=1.5)
# box(lwd=1.5)
axis(
	1,
	at=round(seq(-0.6,0.6,0.2),2),
	tcl=0.5,
	cex.axis=1.4,
	padj=-0.5
)
axis(
	2,
	at=round(seq(-0.6,0.6,0.2),2),
	tcl=0.5,
	cex.axis=1.4,
	# padj=0.5,
	las=1
)
abline(h=0,lwd=1.5,lty='dotted')
abline(v=0,lwd=1.5,lty='dotted')

points(
	R1[,1] ~ E1[,1],
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2,
	xpd=TRUE
)
segments(
	E1[,1],
	R1[,1] + se.factor*R1[,2],
	E1[,1],
	R1[,1] - se.factor*R1[,2],
	lwd=1.5
)
segments(
	E1[,1] + se.factor*E1[,2],
	R1[,1],
	E1[,1] - se.factor*E1[,2],
	R1[,1],
	lwd=1.5
)

plot(
	R2[,1] ~E2[,1],
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2,
	axes=FALSE,
	xlab='',
	ylab='',
	xlim=c(-0.3,0.3),
	ylim=c(-0.3,0.3),
	yaxs='i',
	xaxs='i'
)

title('b  Second dimension', line=1.2, cex.main=2.2)
mtext("Effect trait", 1, outer=FALSE, line=2.75, xpd=NA, cex=1.5)
# box(lwd=1.5)
axis(
	1,
	at=round(seq(-0.8,0.8,0.1),2),
	tcl=0.5,
	cex.axis=1.4,
	padj=-0.5
)
axis(
	2,
	at=round(seq(-0.8,0.8,0.1),2),
	tcl=0.5,
	cex.axis=1.4,
	# padj=0.5,
	las=1
)
abline(h=0,lwd=1.5,lty='dotted')
abline(v=0,lwd=1.5,lty='dotted')

points(
	R2[,1] ~ E2[,1],
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2,
	xpd=TRUE
)
segments(
	E2[,1],
	R2[,1] + se.factor*R2[,2],
	E2[,1],
	R2[,1] - se.factor*R2[,2],
	lwd=1.5
)
segments(
	E2[,1] + se.factor*E2[,2],
	R2[,1],
	E2[,1] - se.factor*E2[,2],
	R2[,1],
	lwd=1.5
)

plot(
	R3[,1] ~ E3[,1],
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2,
	axes=FALSE,
	xlab='',
	ylab='',
	xlim=c(-0.5,0.5),
	ylim=c(-0.2,0.2),
	yaxs='i',
	xaxs='i'
)

title('c  Third dimension', line=1.2, cex.main=2.2)
mtext("Effect trait", 1, outer=FALSE, line=2.75, xpd=NA, cex=1.5)
# box(lwd=1.5)
axis(
	1,
	at=round(seq(-0.8,0.8,0.2),2),
	tcl=0.5,
	cex.axis=1.4,
	padj=-0.5,
	las=1
)
axis(
	2,
	at=round(seq(-0.8,0.8,0.1),2),
	tcl=0.5,
	cex.axis=1.4,
	# padj=0.5,
	las=1
)
axis(
	2,
	at=c(0.5),
	label="",
	tcl=0.5,
	cex.axis=1.4,
	las=1
)
axis(
	1,
	at=c(0.5),
	label="",
	tcl=0.5,
	cex.axis=1.4,
	las=1
)
abline(h=0,lwd=1.5,lty='dotted')
abline(v=0,lwd=1.5,lty='dotted')

points(
	R3[,1] ~ E3[,1],
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2,
	xpd=TRUE
)
segments(
	E3[,1],
	R3[,1] + se.factor*R3[,2],
	E3[,1],
	R3[,1] - se.factor*R3[,2],
	lwd=1.5
)
segments(
	E3[,1] + se.factor*E3[,2],
	R3[,1],
	E3[,1] - se.factor*E3[,2],
	R3[,1],
	lwd=1.5
)

dev.off()
