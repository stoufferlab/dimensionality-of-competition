
# library(ggplot2)
library(RColorBrewer)
# library(plot.matrix)

# where to save this bastard
setEPS(width=10, height=3.5)
postscript('../../manuscript/Figures/reshuffle_alphas.eps')

se.factor <- 2

# layout
layout(mat = matrix(
		c(1, 2), 
        nrow = 1,
        ncol = 2
       ),
       heights = c(1),
       widths = c(2, 2)
)

# I hate defining plot margins
par(mar = c(2.5, 6.25, 0.5, 1.5), oma = c(1, 0, 0, 0.0))

# read in the response/effect pairs for the best fitting model
alpha1 <- read.table("../../results/Godoy/godoy.C.alphas.fit.csv")
alpha2 <- read.table("../../results/Godoy/godoy.T.alphas.fit.csv")

# #
# # interaction strengths
# #

# plot(
# 	(alpha1$alpha),
# 	(alpha2$alpha),
# 	pch=21,
# 	lwd=1.5,
# 	bg="#a6cee3",
# 	cex=2,
# 	axes=FALSE,
# 	xlab='',
# 	ylab='',
# 	xlim=c(-2,10),
# 	ylim=c(-1,3),
# 	yaxs='i',
# 	xaxs='i',
# 	xpd=TRUE
# )

# # polygon(
# # 	c(-5,0,0,-5),
# # 	c(-5,-5,10,10),
# # 	col=gray(0.85),
# # 	border=NA
# # )
# # polygon(
# # 	c(-1,-1,10,10),
# # 	c(-1,0,0,-1),
# # 	col=gray(0.85),
# # 	border=NA
# # )

# # lines(x=c(0,10),y=c(0,0),lty="dotted",lwd=1.5)
# # lines(x=c(0,0),y=c(0,4),lty="dotted",lwd=1.5)


# # annotate axes
# axis(
# 	1,
# 	at=seq.int(-2,10,2),
# 	tcl=0.5,
# 	cex.axis=1.4,
# 	padj=-0.5
# )
# axis(
# 	2,
# 	at=-1:3,
# 	tcl=0.5,
# 	cex.axis=1.4,
# 	padj=0.5,
# 	las=2
# )

# # # add line of best fit to guide eyes
# # x <- alpha1$alpha
# # y <- alpha2$alpha
# # xnew <- seq(-0.5,10,length.out=1000)
# # p <- t(sapply(
# # 	xnew,
# # 	function(xn) deming.ci.xnew(x,y,lambda=0.07,xnew=xn)
# # ))
# # p <- as.data.frame(p)
# # polygon(
# # 	c(xnew,rev(xnew)),
# # 	c(p$lwr,rev(p$upr)),
# # 	col='grey',
# # 	border=NA
# # )
# # lines(
# # 	xnew,
# # 	p$estimate,
# # 	lty=2,
# # 	lwd=1.5
# # )

# # replot points
# points(
# 	(alpha1$alpha),
# 	(alpha2$alpha),
# 	pch=21,
# 	lwd=1.5,
# 	bg="#a6cee3",
# 	cex=2
# )

# # add error bars
# segments(
# 	(alpha1$alpha),
# 	(alpha2$alpha+se.factor*alpha2$alphas.se.se),
# 	(alpha1$alpha),
# 	(alpha2$alpha-se.factor*alpha2$alphas.se.se),
# 	lwd=1.5
# )
# segments(
# 	(alpha1$alpha+se.factor*alpha1$alphas.se.se),
# 	(alpha2$alpha),
# 	(alpha1$alpha-se.factor*alpha1$alphas.se.se),
# 	(alpha2$alpha),
# 	lwd=1.5
# )

# text("b", x=-2, y=3, xpd=NA, cex=2, font=2, adj=c(-1,1.3))
# mtext("Interaction strength (T)", 2, outer=FALSE, line=3.75, xpd=NA, cex=1.3)
# # mtext("(treatment)", 2, outer=FALSE, line=3.25, xpd=NA, cex=1.5)
# # mtext("Trait", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.5)
# mtext("Interaction strength (C)", 1, outer=FALSE, line=2.75, xpd=NA, cex=1.3)
# # box(lwd=1.5)

#
# interaction outcomes
#

plot(
	1/(1+1*alpha1$alpha),
	1/(1+1*alpha2$alpha),
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2,
	axes=FALSE,
	xlab='',
	ylab='',
	xlim=c(0.075,2),
	ylim=c(0.075,2),
	yaxs='i',
	xaxs='i',
	xpd=TRUE,
	log='xy'
)

# polygon(
# 	c(1,2,2,1),
# 	c(0,0,2,2),
# 	col=gray(0.85),
# 	border=NA
# )
# polygon(
# 	c(0,0,2,2),
# 	c(1,2,2,1),
# 	col=gray(0.85),
# 	border=NA
# )
# lines(x=c(1,1),y=c(0,1),lty="dotted",lwd=1.5)
# lines(x=c(0,1),y=c(1,1),lty="dotted",lwd=1.5)


# annotate axes
axis(
	1,
	at=c(0.05,0.1,0.25,0.5,1.0,2), #seq(0,5,0.5), at=seq(0,1.5,0.2),
	tcl=0.5,
	cex.axis=1.4,
	padj=-0.5
)
axis(
	2,
	at=c(0.05,0.1,0.25,0.5,1.0,2.0),
	tcl=0.5,
	cex.axis=1.4,
	padj=0.5,
	las=2
)


# # add line of best fit to guide eyes
# y <- I(1/(1+1*alpha2$alpha))
# x <- I(1/(1+1*alpha1$alpha))
# xnew <- seq(0,1.2,length.out=1000)
# p <- t(sapply(
# 	xnew,
# 	function(xn) deming.ci.xnew(x,y,lambda=1,xnew=xn)
# ))
# p <- as.data.frame(p)
# polygon(
# 	c(xnew,rev(xnew)),
# 	c(p$lwr,rev(p$upr)),
# 	col='grey',
# 	border=NA
# )
# lines(
# 	xnew,
# 	p$estimate,
# 	lty=1,
# 	lwd=1.5
# )

# add horizontal line since no correlation
# abline(
# 	lm(I(1/(1+1*alpha2$alpha))~I(1/(1+1*alpha1$alpha))),
# 	lty=2,
# 	lwd=1.5
# )

# replot points
points(
	1/(1+1*alpha1$alpha),
	1/(1+1*alpha2$alpha),
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2
)

# add error bars
segments(
	1/(1+1*alpha1$alpha),
	1/(1+1*alpha2$alpha+se.factor*alpha2$alphas.se.se),
	1/(1+1*alpha1$alpha),
	1/(1+1*alpha2$alpha-se.factor*alpha2$alphas.se.se),
	lwd=1.5,
	xpd=TRUE
)
segments(
	1/(1+1*alpha1$alpha+se.factor*alpha1$alphas.se.se),
	1/(1+1*alpha2$alpha),
	1/(1+1*alpha1$alpha-se.factor*alpha1$alphas.se.se),
	1/(1+1*alpha2$alpha),
	lwd=1.5,
	xpd=TRUE
)

# segments(
# 	lambda1[,1]+se.factor*lambda1[,2],
# 	lambda2[,1],
# 	lambda1[,1]-se.factor*lambda1[,2],
# 	lambda2[,1],
# 	lwd=1.5
# )

# label the plot and axes
text("a", x=0.075, y=2, xpd=NA, cex=2, font=2, adj=c(-1,1.3))
mtext("Per capita impact", 2, outer=FALSE, line=5.0, xpd=NA, cex=1.3)
mtext("in simulated drought", 2, outer=FALSE, line=3.75, xpd=NA, cex=1.3)
mtext("Per capita impact in control", 1, outer=FALSE, line=2, xpd=NA, cex=1.3)


# read in the response/effect pairs for the best fitting model
alpha1 <- read.table("../../results/Wainwright/wainwright.Open.alphas.fit.csv")
alpha2 <- read.table("../../results/Wainwright/wainwright.Shade.alphas.fit.csv")

# #
# # interaction strengths
# #

# # # exclude the "other" neighbor category
# # alpha1 <- subset(alpha1, col!="Other")
# # alpha2 <- subset(alpha2, col!="Other")

# # only include interactions that were actually observed in both treatments
# alpha1 <- subset(alpha1, alpha1$observed | alpha2$observed)
# alpha2 <- subset(alpha2, alpha1$observed | alpha2$observed)

# alpha1$interaction <- interaction(alpha1$row, alpha1$col)
# alpha2$interaction <- interaction(alpha2$row, alpha2$col)

# alpha1 <- subset(alpha1, interaction %in% unique(alpha2$interaction))
# alpha2 <- subset(alpha2, interaction %in% unique(alpha1$interaction))

# plot(
# 	(alpha1$alpha),
# 	(alpha2$alpha),
# 	pch=21,
# 	lwd=1.5,
# 	bg="#a6cee3",
# 	cex=2,
# 	axes=FALSE,
# 	xlab='',
# 	ylab='',
# 	xlim=c(-1,2),
# 	ylim=c(-1,2),
# 	yaxs='i',
# 	xaxs='i',
# 	xpd=TRUE,
# )

# # polygon(
# # 	c(-5,0,0,-5),
# # 	c(-5,-5,10,10),
# # 	col=gray(0.85),
# # 	border=NA
# # )
# # polygon(
# # 	c(-1,-1,10,10),
# # 	c(-1,0,0,-1),
# # 	col=gray(0.85),
# # 	border=NA
# # )

# # lines(x=c(0,10),y=c(0,0),lty="dotted",lwd=1.5)
# # lines(x=c(0,0),y=c(0,4),lty="dotted",lwd=1.5)


# # annotate axes
# axis(
# 	1,
# 	at=seq(-1,2,0.5),
# 	tcl=0.5,
# 	cex.axis=1.4,
# 	padj=-0.5
# )
# axis(
# 	2,
# 	at=seq(-1,2,0.5),
# 	tcl=0.5,
# 	cex.axis=1.4,
# 	padj=0.5,
# 	las=2
# )

# # # add line of best fit to guide eyes
# # x <- alpha1$alpha
# # y <- alpha2$alpha
# # xnew <- seq(-0.5,10,length.out=1000)
# # p <- t(sapply(
# # 	xnew,
# # 	function(xn) deming.ci.xnew(x,y,lambda=0.07,xnew=xn)
# # ))
# # p <- as.data.frame(p)
# # polygon(
# # 	c(xnew,rev(xnew)),
# # 	c(p$lwr,rev(p$upr)),
# # 	col='grey',
# # 	border=NA
# # )
# # lines(
# # 	xnew,
# # 	p$estimate,
# # 	lty=2,
# # 	lwd=1.5
# # )

# # replot points
# points(
# 	(alpha1$alpha),
# 	(alpha2$alpha),
# 	pch=21,
# 	lwd=1.5,
# 	bg="#b2df8a",
# 	cex=2
# )

# # add error bars
# segments(
# 	(alpha1$alpha),
# 	(alpha2$alpha+se.factor*alpha2$alphas.se.se),
# 	(alpha1$alpha),
# 	(alpha2$alpha-se.factor*alpha2$alphas.se.se),
# 	lwd=1.5
# )
# segments(
# 	(alpha1$alpha+se.factor*alpha1$alphas.se.se),
# 	(alpha2$alpha),
# 	(alpha1$alpha-se.factor*alpha1$alphas.se.se),
# 	(alpha2$alpha),
# 	lwd=1.5
# )

# text("b", x=-1, y=2, xpd=NA, cex=2, font=2, adj=c(-1,1.3))
# mtext("Interaction strength (T)", 2, outer=FALSE, line=3.75, xpd=NA, cex=1.3)
# # mtext("(treatment)", 2, outer=FALSE, line=3.25, xpd=NA, cex=1.5)
# # mtext("Trait", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.5)
# mtext("Interaction strength (C)", 1, outer=FALSE, line=2.75, xpd=NA, cex=1.3)
# # box(lwd=1.5)

#
# interaction outcomes
#

plot(
	1/(1+1*alpha1$alpha),
	1/(1+1*alpha2$alpha),
	lwd=1.5,
	axes=FALSE,
	xlab='',
	ylab='',
	xlim=c(0.01,10),
	ylim=c(0.01,10),
	yaxs='i',
	xaxs='i',
	xpd=TRUE,
	log='xy',
	type='n'
)

# polygon(
# 	c(1,2,2,1),
# 	c(0,0,2,2),
# 	col=gray(0.85),
# 	border=NA
# )
# polygon(
# 	c(0,0,2,2),
# 	c(1,2,2,1),
# 	col=gray(0.85),
# 	border=NA
# )
# lines(x=c(1,1),y=c(0,1),lty="dotted",lwd=1.5)
# lines(x=c(0,1),y=c(1,1),lty="dotted",lwd=1.5)


# annotate axes
axis(
	1,
	at=c(0.01,0.1,1.0,10), #seq(0,1.5,0.2),
	tcl=0.5,
	cex.axis=1.4,
	padj=-0.5
)
axis(
	2,
	at=c(0.01,0.1,1.0,10), #seq(0,5,0.5),
	tcl=0.5,
	cex.axis=1.4,
	padj=0.5,
	las=2
)

# replot points
points(
	1/(1+1*alpha1$alpha),
	1/(1+1*alpha2$alpha),
	pch=21,
	lwd=1.5,
	bg="#b2df8a",
	cex=2
)

# add error bars
segments(
	1/(1+1*alpha1$alpha),
	1/(1+1*alpha2$alpha+se.factor*alpha2$alphas.se.se),
	1/(1+1*alpha1$alpha),
	1/(1+1*alpha2$alpha-se.factor*alpha2$alphas.se.se),
	lwd=1.5,
	xpd=TRUE
)
segments(
	1/(1+1*alpha1$alpha+se.factor*alpha1$alphas.se.se),
	1/(1+1*alpha2$alpha),
	1/(1+1*alpha1$alpha-se.factor*alpha1$alphas.se.se),
	1/(1+1*alpha2$alpha),
	lwd=1.5,
	xpd=TRUE
)

# segments(
# 	lambda1[,1]+se.factor*lambda1[,2],
# 	lambda2[,1],
# 	lambda1[,1]-se.factor*lambda1[,2],
# 	lambda2[,1],
# 	lwd=1.5
# )

text("b", x=0.01, y=10, xpd=NA, cex=2, font=2, adj=c(-1,1.3))
mtext("Per capita impact", 2, outer=FALSE, line=5.0, xpd=NA, cex=1.3)
mtext("in artificial shade", 2, outer=FALSE, line=3.75, xpd=NA, cex=1.3)
# mtext("(treatment)", 2, outer=FALSE, line=3.25, xpd=NA, cex=1.5)
# mtext("Trait", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.5)
mtext("Per capita impact in control", 1, outer=FALSE, line=2, xpd=NA, cex=1.3)
# mtext("in first environment", 1, outer=FALSE, line=2.85, xpd=NA, cex=1.3)
# box(lwd=1.5)

dev.off()
