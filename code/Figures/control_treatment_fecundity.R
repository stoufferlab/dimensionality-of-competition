
# library(ggplot2)
library(RColorBrewer)
# library(plot.matrix)

# where to save this bastard
setEPS(width=5.0, height=6.33)
postscript('../../manuscript/Supplementary/Figures/reshuffle_fecundities.eps')

se.factor <- 2

# layout
layout(mat = matrix(
		c(1, 2), 
        nrow = 2,
        ncol = 1
       ),
       heights = c(1),
       widths = c(2)
)

# I hate defining plot margins
par(mar = c(3, 7.25, 2.5, 2.0), oma = c(1, 0, 0, 0.0))

######################
# godoy
######################

# read in the lambdas from both conditions
lambda1 <- read.table("../../results/Godoy/godoy.C.lambdas.csv")
lambda2 <- read.table("../../results/Godoy/godoy.T.lambdas.csv")

# plot the lambda values
plot(
	lambda1$lambdas.mean,
	lambda2$lambdas.mean,
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2,
	axes=FALSE,
	xlab='',
	ylab='',
	xlim=c(1,1000),
	ylim=c(1,100),
	yaxs='i',
	xaxs='i',
	xpd=TRUE,
	log='xy',
	type='n'
)

# add deming regression line of best fit to guide eyes
source('deming_v00.R')
x <- log(lambda1$lambdas.mean)
y <- log(lambda2$lambdas.mean)
xnew <- log(seq(0.001,2000,length.out=1000))
p <- t(sapply(
	xnew,
	function(xn) deming.ci.xnew(x,y,lambda=1,xnew=xn)
))
xnew <- exp(xnew)
p <- as.data.frame(exp(p))
# p <- as.data.frame(predict(
# 	m,
# 	newdata=data.frame(x=x),
# 	interval="confidence"
# ))
polygon(
	c(xnew,rev(xnew)),
	c(p$lwr,rev(p$upr)),
	col='white',
	border=gray(0.5),
	lwd=2,
	lty=5
)
lines(
	xnew,
	p$estimate,
	lty=1,
	lwd=1.5
)

# add axes
axis(
	1,
	at=c(1,10,100,1000),
	tcl=0.5,
	cex.axis=1.4,
	padj=-0.5
)
for(oo in c(0,1,2,3,4)){
	axis(
		1,
		at=seq(2,10,1)*(10**oo),
		labels=FALSE,
		tcl=0.25	
	)
}
axis(
	2,
	at=c(1,10,100,1000),
	tcl=0.5,
	cex.axis=1.4,
	padj=0.5,
	las=2
)
for(oo in c(0,1,2,3,4)){
	axis(
		2,
		at=seq(2,10,1)*(10**oo),
		labels=FALSE,
		tcl=0.25	
	)
}

# abline(
# 	lm(lambda2$lambdas.mean~lambda1$lambdas.mean),
# 	lwd=1.5
# )

# replot points
points(
	lambda1$lambdas.mean,
	lambda2$lambdas.mean,
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=1.4
)

# add error bars
segments(
	lambda1$lambdas.mean,
	lambda2$lambdas.mean+se.factor*lambda2$lambdas.se,
	lambda1$lambdas.mean,
	lambda2$lambdas.mean-se.factor*lambda2$lambdas.se,
	lwd=1.5
)
segments(
	lambda1$lambdas.mean+se.factor*lambda1$lambdas.se,
	lambda2$lambdas.mean,
	lambda1$lambdas.mean-se.factor*lambda1$lambdas.se,
	lambda2$lambdas.mean,
	lwd=1.5
)

text("a", x=1, y=100, xpd=NA, cex=2, font=2, adj=c(-1,1.3))
mtext("Intrinsic number of fruits", 2, outer=FALSE, line=5.75, xpd=NA, cex=1.3)
mtext("(Simulated drought)", 2, outer=FALSE, line=4.25, xpd=NA, cex=1.3)
# mtext("(treatment)", 2, outer=FALSE, line=3.25, xpd=NA, cex=1.5)
# mtext("Trait", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.5)
mtext("Intrinsic number of fruits", 1, outer=FALSE, line=1.75, xpd=NA, cex=1.3)
mtext("(Control)", 1, outer=FALSE, line=3.00, xpd=NA, cex=1.3)
# box(lwd=1.5)

##################
# wainwright
##################

lambda1 <- read.table("../../results/Wainwright/wainwright.Open.lambdas.csv")
lambda2 <- read.table("../../results/Wainwright/wainwright.Shade.lambdas.csv")

# plot the lambda values
plot(
	lambda1$lambdas.mean,
	lambda2$lambdas.mean,
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2,
	axes=FALSE,
	xlab='',
	ylab='',
	xlim=c(1,10000),
	ylim=c(1,10000),
	yaxs='i',
	xaxs='i',
	xpd=TRUE,
	log='xy',
	type='n'
)

# add deming regression line of best fit to guide eyes
source('deming_v00.R')
x <- log(lambda1$lambdas.mean)
y <- log(lambda2$lambdas.mean)
xnew <- log(seq(0.001,11000,length.out=1000))
p <- t(sapply(
	xnew,
	function(xn) deming.ci.xnew(x,y,lambda=1,xnew=xn)
))
xnew <- exp(xnew)
p <- as.data.frame(exp(p))
# p <- as.data.frame(predict(
# 	m,
# 	newdata=data.frame(x=x),
# 	interval="confidence"
# ))
polygon(
	c(xnew,rev(xnew)),
	c(p$lwr,rev(p$upr)),
	col='white',
	border=gray(0.5),
	lwd=2,
	lty=5
)
lines(
	xnew,
	p$estimate,
	lty=1,
	lwd=1.5
)


# add axes
axis(
	1,
	at=c(1,10,100,1000,10000),
	tcl=0.5,
	cex.axis=1.4,
	padj=-0.5
)
for(oo in c(0,1,2,3,4,5)){
	axis(
		1,
		at=seq(2,10,1)*(10**oo),
		labels=FALSE,
		tcl=0.25	
	)
}
axis(
	2,
	at=c(1,10,100,1000,10000),
	tcl=0.5,
	cex.axis=1.4,
	padj=0.5,
	las=2
)
for(oo in c(0,1,2,3,4)){
	axis(
		2,
		at=seq(2,10,1)*(10**oo),
		labels=FALSE,
		tcl=0.25	
	)
}

# abline(
# 	lm(lambda2$lambdas.mean~lambda1$lambdas.mean),
# 	lwd=1.5
# )

# replot points
points(
	lambda1$lambdas.mean,
	lambda2$lambdas.mean,
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=1.4,
	xpd=TRUE
)

# add error bars
segments(
	lambda1$lambdas.mean,
	lambda2$lambdas.mean+se.factor*lambda2$lambdas.se,
	lambda1$lambdas.mean,
	lambda2$lambdas.mean-se.factor*lambda2$lambdas.se,
	lwd=1.5
)
segments(
	lambda1$lambdas.mean+se.factor*lambda1$lambdas.se,
	lambda2$lambdas.mean,
	lambda1$lambdas.mean-se.factor*lambda1$lambdas.se,
	lambda2$lambdas.mean,
	lwd=1.5
)

text("b", x=1, y=10000, xpd=NA, cex=2, font=2, adj=c(-1,1.3))
mtext("Intrinsic seed set", 2, outer=FALSE, line=5.75, xpd=NA, cex=1.3)
mtext("(Artifically shaded)", 2, outer=FALSE, line=4.25, xpd=NA, cex=1.3)
# mtext("(treatment)", 2, outer=FALSE, line=3.25, xpd=NA, cex=1.5)
# mtext("Trait", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.5)
mtext("Intrinsic seed set", 1, outer=FALSE, line=1.75, xpd=NA, cex=1.3)
mtext("(Control)", 1, outer=FALSE, line=3.00, xpd=NA, cex=1.3)
# box(lwd=1.5)
dev.off()
