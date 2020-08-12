
library(ggplot2)
library(RColorBrewer)
# library(plot.matrix)

# where to save this bastard
pdf('../../manuscript/Figures/Reshuffle/reshuffle.pdf', width=4.5, height=8)

se.factor <- 2

# layout
layout(mat = matrix(
		c(1, 2, 3), 
        nrow = 3,
        ncol = 1
       ),
       heights = c(1),
       widths = c(2, 2, 2)
)

# I hate defining plot margins
par(mar = c(3, 6.25, 2.5, 4.5), oma = c(1, 0, 0, 0.0))

# read in the response/effect pairs for the best fitting model
lambda1 <- read.table("../../results/Godoy/godoy.C.lambdas.csv")
lambda2 <- read.table("../../results/Godoy/godoy.T.lambdas.csv")
alpha1 <- read.table("../../results/Godoy/godoy.C.alphas.csv")
alpha2 <- read.table("../../results/Godoy/godoy.T.alphas.csv")

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
	log='xy'
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
	cex=2
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

text("a", x=1, y=100, xpd=NA, cex=2, font=2, adj=c(-1,1.3), hadj=2)
mtext("Intrinsic fecundity (T)", 2, outer=FALSE, line=3.75, xpd=NA, cex=1.3)
# mtext("(treatment)", 2, outer=FALSE, line=3.25, xpd=NA, cex=1.5)
# mtext("Trait", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.5)
mtext("Intrinsic fecundity (C)", 1, outer=FALSE, line=2.75, xpd=NA, cex=1.3)
# box(lwd=1.5)



#
# interaction strengths
#

plot(
	(alpha1$alpha),
	(alpha2$alpha),
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2,
	axes=FALSE,
	xlab='',
	ylab='',
	xlim=c(-2,10),
	ylim=c(-1,3),
	yaxs='i',
	xaxs='i',
	xpd=TRUE
)

# polygon(
# 	c(-5,0,0,-5),
# 	c(-5,-5,10,10),
# 	col=gray(0.85),
# 	border=NA
# )
# polygon(
# 	c(-1,-1,10,10),
# 	c(-1,0,0,-1),
# 	col=gray(0.85),
# 	border=NA
# )

# lines(x=c(0,10),y=c(0,0),lty="dotted",lwd=1.5)
# lines(x=c(0,0),y=c(0,4),lty="dotted",lwd=1.5)


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
	at=-1:3,
	tcl=0.5,
	cex.axis=1.4,
	padj=0.5,
	las=2
)

# # add line of best fit to guide eyes
# x <- alpha1$alpha
# y <- alpha2$alpha
# xnew <- seq(-0.5,10,length.out=1000)
# p <- t(sapply(
# 	xnew,
# 	function(xn) deming.ci.xnew(x,y,lambda=0.07,xnew=xn)
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
# 	lty=2,
# 	lwd=1.5
# )

# replot points
points(
	(alpha1$alpha),
	(alpha2$alpha),
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2
)

# add error bars
segments(
	(alpha1$alpha),
	(alpha2$alpha+se.factor*alpha2$alphas.se.se),
	(alpha1$alpha),
	(alpha2$alpha-se.factor*alpha2$alphas.se.se),
	lwd=1.5
)
segments(
	(alpha1$alpha+se.factor*alpha1$alphas.se.se),
	(alpha2$alpha),
	(alpha1$alpha-se.factor*alpha1$alphas.se.se),
	(alpha2$alpha),
	lwd=1.5
)

text("b", x=-2, y=3, xpd=NA, cex=2, font=2, adj=c(-1,1.3), hadj=2)
mtext("Interaction strength (T)", 2, outer=FALSE, line=3.75, xpd=NA, cex=1.3)
# mtext("(treatment)", 2, outer=FALSE, line=3.25, xpd=NA, cex=1.5)
# mtext("Trait", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.5)
mtext("Interaction strength (C)", 1, outer=FALSE, line=2.75, xpd=NA, cex=1.3)
# box(lwd=1.5)

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
	xlim=c(0,1.2),
	ylim=c(0,1.2),
	yaxs='i',
	xaxs='i',
	xpd=TRUE
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
	at=seq(0,1.5,0.2),
	tcl=0.5,
	cex.axis=1.4,
	padj=-0.5
)
axis(
	2,
	at=seq(0,1.5,0.2),
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

text("c", x=0, y=1.2, xpd=NA, cex=2, font=2, adj=c(-1,1.3), hadj=2)
mtext("Per capita impact (T)", 2, outer=FALSE, line=3.75, xpd=NA, cex=1.3)
# mtext("(treatment)", 2, outer=FALSE, line=3.25, xpd=NA, cex=1.5)
# mtext("Trait", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.5)
mtext("Per capita impact (C)", 1, outer=FALSE, line=2.75, xpd=NA, cex=1.3)
# box(lwd=1.5)


# # read in the response/effect pairs for the best fitting model
# lambda1 <- read.table("../../results/Mayfield/mayfield.Open.lambdas.csv")
# lambda2 <- read.table("../../results/Mayfield/mayfield.Shade.lambdas.csv")
# alpha1 <- read.table("../../results/Mayfield/mayfield.Open.alphas.csv")
# alpha2 <- read.table("../../results/Mayfield/mayfield.Shade.alphas.csv")

# # plot the lambda values
# plot(
# 	lambda1$lambdas.mean,
# 	lambda2$lambdas.mean,
# 	pch=21,
# 	lwd=1.5,
# 	bg="#a6cee3",
# 	cex=2,
# 	axes=FALSE,
# 	xlab='',
# 	ylab='',
# 	xlim=c(1,10000),
# 	ylim=c(1,1000),
# 	yaxs='i',
# 	xaxs='i',
# 	xpd=TRUE,
# 	log='xy'
# )

# # add axes
# axis(
# 	1,
# 	at=c(1,10,100,1000,10000),
# 	tcl=0.5,
# 	cex.axis=1.4,
# 	padj=-0.5
# )
# for(oo in c(0,1,2,3,4)){
# 	axis(
# 		1,
# 		at=seq(2,10,1)*(10**oo),
# 		labels=FALSE,
# 		tcl=0.25	
# 	)
# }

# axis(
# 	2,
# 	at=c(1,10,100,1000),
# 	tcl=0.5,
# 	cex.axis=1.4,
# 	padj=0.5,
# 	las=2
# )
# for(oo in c(0,1,2,3,4)){
# 	axis(
# 		2,
# 		at=seq(2,10,1)*(10**oo),
# 		labels=FALSE,
# 		tcl=0.25	
# 	)
# }

# # add line of best fit to guide eyes
# abline(
# 	lm(lambda2$lambdas.mean~lambda1$lambdas.mean),
# 	lwd=1.5
# )

# # replot points
# points(
# 	lambda1$lambdas.mean,
# 	lambda2$lambdas.mean,
# 	pch=21,
# 	lwd=1.5,
# 	bg="#a6cee3",
# 	cex=2
# )

# # add error bars
# segments(
# 	lambda1$lambdas.mean,
# 	lambda2$lambdas.mean+se.factor*lambda2$lambdas.se,
# 	lambda1$lambdas.mean,
# 	lambda2$lambdas.mean-se.factor*lambda2$lambdas.se,
# 	lwd=1.5
# )
# segments(
# 	lambda1$lambdas.mean+se.factor*lambda1$lambdas.se,
# 	lambda2$lambdas.mean,
# 	lambda1$lambdas.mean-se.factor*lambda1$lambdas.se,
# 	lambda2$lambdas.mean,
# 	lwd=1.5
# )

# mtext("Intrinsic fecundity", 2, outer=FALSE, line=4.75, xpd=NA, cex=1.5)
# mtext("(treatment)", 2, outer=FALSE, line=3.25, xpd=NA, cex=1.5)
# # mtext("Trait", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.5)
# mtext("Intrinsic fecundity (control)", 1, outer=FALSE, line=2.75, xpd=NA, cex=1.5)
# # box(lwd=1.5)

# # figure out which interactions make sense to compare
# alpha1 <- subset(alpha1, observed & common)
# alpha2 <- subset(alpha2, observed & common)

# interactions1 <- unique(interaction(alpha1$row,alpha1$col,sep=":"))
# interactions2 <- unique(interaction(alpha2$row,alpha2$col,sep=":"))

# rownames(alpha1) <- interactions1
# rownames(alpha2) <- interactions2

# shared.interactions <- intersect(interactions1,interactions2)

# alpha1 <- alpha1[shared.interactions,]
# alpha2 <- alpha2[shared.interactions,]

# plot(
# 	(alpha1$alpha),
# 	(alpha2$alpha),
# 	pch=21,
# 	lwd=1.5,
# 	bg="#a6cee3",
# 	cex=2,
# 	# axes=FALSE,
# 	xlab='',
# 	ylab='',
# 	# xlim=c(-5,5),
# 	# ylim=c(-5,5),
# 	yaxs='i',
# 	xaxs='i',
# 	xpd=TRUE
# )

# # annotate axes
# axis(
# 	1,
# 	at=round(seq(0,2,0.2),1),
# 	tcl=0.5,
# 	cex.axis=1.4,
# 	padj=-0.5
# )
# axis(
# 	2,
# 	at=round(seq(0,2,0.2),1),
# 	tcl=0.5,
# 	cex.axis=1.4,
# 	padj=0.5,
# 	las=2
# )

# # add horizontal line since no correlation
# abline(
# 	lm(I(1/(1+1*alpha2$alpha))~I(1/(1+1*alpha1$alpha))),
# 	lty=2,
# 	lwd=1.5
# )

# # replot points
# points(
# 	1/(1+1*alpha1$alpha),
# 	1/(1+1*alpha2$alpha),
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
# 	lwd=1.5,
# 	xpd=TRUE
# )
# segments(
# 	(alpha1$alpha+se.factor*alpha1$alphas.se.se),
# 	(alpha2$alpha),
# 	(alpha1$alpha-se.factor*alpha1$alphas.se.se),
# 	(alpha2$alpha),
# 	lwd=1.5,
# 	xpd=TRUE
# )

# # segments(
# # 	lambda1[,1]+se.factor*lambda1[,2],
# # 	lambda2[,1],
# # 	lambda1[,1]-se.factor*lambda1[,2],
# # 	lambda2[,1],
# # 	lwd=1.5
# # )

# mtext("Interaction outcome", 2, outer=FALSE, line=4.75, xpd=NA, cex=1.5)
# mtext("(treatment)", 2, outer=FALSE, line=3.25, xpd=NA, cex=1.5)
# # mtext("Trait", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.5)
# mtext("Interaction outcome (control)", 1, outer=FALSE, line=2.75, xpd=NA, cex=1.5)
# # box(lwd=1.5)






dev.off()




# d <- T2
# plot(
# 	d[,1] ~ d[,2],
# 	pch=21,
# 	lwd=1.5,
# 	bg="#a6cee3",
# 	cex=2,
# 	axes=FALSE,
# 	xlab='',
# 	ylab='',
# 	xlim=c(-0.75,0.75),
# 	ylim=c(-0.75,0.75),
# 	yaxs='i',
# 	xaxs='i'
# )

# mtext("Effect trait", 1, outer=FALSE, line=2.75, xpd=NA, cex=1.5)
# # box(lwd=1.5)
# axis(
# 	1,
# 	at=round(seq(-0.8,0.8,0.2),2),
# 	tcl=0.5,
# 	cex.axis=1.4,
# 	padj=-0.5
# )
# axis(
# 	2,
# 	at=round(seq(-0.8,0.8,0.2),2),
# 	tcl=0.5,
# 	cex.axis=1.4,
# 	padj=0.5
# )

# d <- T3
# plot(
# 	d[,1] ~ d[,2],
# 	pch=21,
# 	lwd=1.5,
# 	bg="#a6cee3",
# 	cex=2,
# 	axes=FALSE,
# 	xlab='',
# 	ylab='',
# 	xlim=c(-0.75,0.75),
# 	ylim=c(-0.75,0.75),
# 	yaxs='i',
# 	xaxs='i'
# )

# mtext("Effect trait", 1, outer=FALSE, line=2.75, xpd=NA, cex=1.5)
# # box(lwd=1.5)
# axis(
# 	1,
# 	at=round(seq(-0.8,0.8,0.2),2),
# 	tcl=0.5,
# 	cex.axis=1.4,
# 	padj=-0.5
# )
# axis(
# 	2,
# 	at=round(seq(-0.8,0.8,0.2),2),
# 	tcl=0.5,
# 	cex.axis=1.4,
# 	padj=0.5
# )
