
library(RColorBrewer)
# library(plot.matrix)

setEPS(width=11, height=4.)
postscript('../../manuscript/Figures/response.effect.spain.eps')

layout(mat = matrix(
		c(1, 2), 
        nrow = 1, 
        ncol = 2
       ),
       heights = 1,
       widths = c(2,2)
)

par(oma = c(0, 2.5, 0, 2.5))

par(mar = c(5, 4, 2.5, 1.5))

# Control values

all.fits <- read.table('../../results/Spain/spain.Control.fit.summary.csv')
which.fit <- which.min(subset(all.fits, dimensions==2)$AIC)
load(paste0('../../results/Spain/',rownames(subset(all.fits, dimensions==2)[which.fit,])))
assign("optim.lowD", eval(parse(text = paste0("Control.optim.lowD"))))

which.trait <- 1

source('../Utils/se.helper.R')

# define some sizes
cex.axis <- 1.7
padj <- 0 #.25

# add a color scale for fitness

plot(
	x=NA,
	y=NA,
	xlim=c(0,1),
	ylim=c(-0.2,1),
	type='n',
	axes=FALSE,
	xlab='',
	ylab='',
	xaxs='i',
	yaxs='i'
	# asp=1
)
# abline(h=0,lwd=1.5,lty='dashed',col='grey')
# abline(v=0,lwd=1.5,lty='dashed',col='grey')

mtext('a  Dataset 1-Wet', 3, line=0.85, cex=2.2)
text(-0.25,0.4,"Response trait", adj=0.5, xpd=NA, cex=2, srt=90)
# mtext("Trait", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.5)
mtext("Effect trait", 1, outer=FALSE, line=2.75, xpd=NA, cex=2)
# box(lwd=1.5)
axis(
	1,
	at=round(seq(-01,01,0.2),2),
	tcl=0.5,
	cex.axis=1.4,
	padj=-0.5,
	lwd=1.5
)
axis(
	2,
	at=round(seq(-01,01,0.2),2),
	tcl=0.5,
	cex.axis=1.4,
	# padj=0.5,
	las=1,
	lwd=1.5
)

points(
	x=apply(effects, 1, quantile, probs=c(0.5)),
	y=apply(responses, 1, quantile, probs=c(0.5)),
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2,
	xpd=TRUE
)

quants <- 0.25
segments(
	apply(effects, 1, quantile, probs=c(quants)),
	apply(responses, 1, quantile, probs=c(0.5)),
	apply(effects, 1, quantile, probs=c(1-quants)),
	apply(responses, 1, quantile, probs=c(0.5))
)
segments(
	apply(effects, 1, quantile, probs=c(0.5)),
	apply(responses, 1, quantile, probs=c(quants)),
	apply(effects, 1, quantile, probs=c(0.5)),
	apply(responses, 1, quantile, probs=c(1-quants)),
)

x1 <- grconvertX(apply(effects, 1, quantile, probs=c(0.5)), to="ndc")
y1 <- grconvertY(apply(responses, 1, quantile, probs=c(0.5)), to="ndc")

# Treatment values

all.fits <- read.table('../../results/Spain/spain.Treatment.fit.summary.csv')
which.fit <- which.min(subset(all.fits, dimensions==3)$AIC)
load(paste0('../../results/Spain/',rownames(subset(all.fits, dimensions==3)[which.fit,])))
assign("optim.lowD", eval(parse(text = paste0("Treatment.optim.lowD"))))

which.trait <- 1

source('../Utils/se.helper.R')

# define some sizes
cex.axis <- 1.7
padj <- 0 #.25

# add a color scale for fitness

# par(mar = c(5, 4.5, 4.5, 0.5), oma = c(0, 1.25, 0, 5.5))

par(mar = c(5, 1.5, 2.5, 4))

plot(
	x=NA,
	y=NA,
	xlim=c(0,1),
	ylim=c(-0.2,1),
	type='n',
	axes=FALSE,
	xlab='',
	ylab='',
	xaxs='i',
	yaxs='i'
	# asp=1
)
# abline(h=0,lwd=1.5,lty='dashed',col='grey')
# abline(v=0,lwd=1.5,lty='dashed',col='grey')

mtext('b  Dataset 1-Dry', 3, line=0.85, cex=2.2)
text(1.25,0.4,"Response trait", adj=0.5, xpd=NA, cex=2, srt=270)
# mtext("Trait", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.5)
mtext("Effect trait", 1, outer=FALSE, line=2.75, xpd=NA, cex=2)
# box(lwd=1.5)
axis(
	1,
	at=round(seq(-01,01,0.2),2),
	tcl=0.5,
	cex.axis=1.4,
	padj=-0.5,
	lwd=1.5
)
axis(
	4,
	at=round(seq(-01,01,0.2),2),
	tcl=0.5,
	cex.axis=1.4,
	# padj=0.5,
	las=1,
	lwd=1.5
)

points(
	x=apply(effects, 1, quantile, probs=c(0.5)),
	y=apply(responses, 1, quantile, probs=c(0.5)),
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2,
	xpd=TRUE
)

quants <- 0.25
segments(
	apply(effects, 1, quantile, probs=c(quants)),
	apply(responses, 1, quantile, probs=c(0.5)),
	apply(effects, 1, quantile, probs=c(1-quants)),
	apply(responses, 1, quantile, probs=c(0.5))
)
segments(
	apply(effects, 1, quantile, probs=c(0.5)),
	apply(responses, 1, quantile, probs=c(quants)),
	apply(effects, 1, quantile, probs=c(0.5)),
	apply(responses, 1, quantile, probs=c(1-quants)),
)

x1 <- grconvertX(x1, from="ndc")
y1 <- grconvertY(y1, from="ndc")

x2 <- apply(effects, 1, quantile, probs=c(0.5))
y2 <- apply(responses, 1, quantile, probs=c(0.5))

par(xpd=NA)
segments(x1, y1, x2, y2, col='grey32', lty='dotted')

dev.off()
