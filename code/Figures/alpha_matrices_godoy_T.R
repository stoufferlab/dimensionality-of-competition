
# library(ggplot2)
library(RColorBrewer)
library(plot.matrix)

# f = a + b1*z + b2*N + b3*E + c1*z*z + c2*N*N + c3*E*E + c4*z*N + c5*z*E + c6*N*E + d*z*N*E

A1 <- as.matrix(read.table("../../results/Godoy/godoy.T.alphas.1.csv"))
A2 <- as.matrix(read.table("../../results/Godoy/godoy.T.alphas.2.csv"))
A3 <- as.matrix(read.table("../../results/Godoy/godoy.T.alphas.3.csv"))
A <- A1 + A2 + A3 #as.matrix(read.table("../../results/Godoy/godoy.T.alphas.csv"))

rownames(A) <- colnames(A) <- letters[1:nrow(A)]
rownames(A1) <- colnames(A1) <- letters[1:nrow(A1)]
rownames(A2) <- colnames(A2) <- letters[1:nrow(A2)]
rownames(A3) <- colnames(A3) <- letters[1:nrow(A3)]

# A1 <- matrix(kronecker(1:10,1:10),10) / 100

# get a baseline matrix
A0 <- matrix(mean(A),nrow(A1),ncol(A1))

# make the first dimension relative
A1 <- A1 - A0

weights <- c(0.9,0.9)
hmm <- weights[1]*rowMeans(A1) + weights[2]*colMeans(A1)
# hmm <- 0.5*(apply(A,1,max) + apply(A,2,max))
# # # hmm <- 0.5*(apply(A,))
hmm.order <- (names(sort(rank(hmm),decreasing=TRUE))) #order(hmm, decreasing=TRUE)

A <- A[hmm.order,]
A <- A[,hmm.order]

A1 <- A1[hmm.order,]
A1 <- A1[,hmm.order]

A2 <- A2[hmm.order,]
A2 <- A2[,hmm.order]
A3 <- A3[hmm.order,]
A3 <- A3[,hmm.order]
# A1 <- A1[hmm.order,rev(hmm.order)]
# A2 <- A2[hmm.order,rev(hmm.order)]
# A3 <- A3[hmm.order,rev(hmm.order)]

fmax <- max((c(A1,A2,A3,A)))
fmin <- min((c(A1,A2,A3,A)))

setEPS(width=17, height=8)
postscript('../../manuscript/Figures/alphas.godoy.T.eps')

layout(mat = rbind(
		c(5,6,1,2),
		c(5,6,3,4)
       ),
       heights = c(2, 2),
       widths = c(2, 0.3, 1, 1)
)

par(mar = c(5, 6, 4.5, 0), oma = c(0, 0, 0, 0.75))

# layout.show(6)

# add a color scale for fitness

pal <- c(
	rev(colorRampPalette(brewer.pal(9, "Blues"))(100)),
	(colorRampPalette(brewer.pal(9, "Reds"))(200))
)

# plot the mean overall
plot(
	A0,
	col=pal,
	breaks=seq(-1.5, 3, length.out=length(pal)),
	key=NULL,
	xaxt='n',
	yaxt='n',
	xlab='',
	ylab='',
	main='',
	axes=FALSE,
	axis.col=NULL,
	axis.row=NULL,
	asp=1
)

title('b  Mean interaction', line=1.2, cex.main=2.3)
mtext("Effect species", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.75)
mtext("Response species", 2, outer=FALSE, line=0.0, xpd=NA, cex=1.75)


# plot the first axis
plot(
	A1,
	col=pal,
	breaks=seq(-1.5, 3, length.out=length(pal)),
	key=NULL,
	xaxt='n',
	yaxt='n',
	xlab='',
	ylab='',
	main='',
	axes=FALSE,
	axis.col=NULL,
	axis.row=NULL,
	asp=1
)

title('c  First dimension (15.7%)', line=1.2, cex.main=2.3)
mtext("Effect species", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.75)
mtext("Response species", 2, outer=FALSE, line=0.0, xpd=NA, cex=1.75)

# par(mar = c(1, 1, 1, 1))
plot(
	A2,
	col=pal,
	breaks=seq(-1.5, 3, length.out=length(pal)),
	key=NULL,
	xaxt='n',
	yaxt='n',
	xlab='',
	ylab='',
	main='',
	axes=FALSE,
	axis.col=NULL,
	axis.row=NULL,
	asp=1
)
title('d  Second dimension (5.6%)', line=1.2, cex.main=2.3, xpd=NA)
mtext("Effect species", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.75)
mtext("Response species", 2, outer=FALSE, line=0.0, xpd=NA, cex=1.75)
# mtext("+", 2, outer=FALSE, line=3.7, xpd=NA, cex=3.5)

 #0,max(max(no.comp),max(with.comp)))) # xlab = expression('s'['m']*' = 1 - w'['jm']), ylab=expression('s'['f']*' = 1 - w'['kf']))
# image(with.comp, xlab = 'z', ylab='Env', main='With competition', col=pal, zlim=c(fmin, fmax)) #, zlim=c(0,max(max(no.comp),max(with.comp)))) # xlab = expression('s'['m']*' = 1 - w'['jm']), ylab=expression('s'['f']*' = 1 - w'['kf']))

# pal <- brewer.pal(9, "Blues")
#pal <- brewer.pal(9, "Greys")

# # plot the difference
# par(mar = c(4, 0, 5, 7))
plot(
	A3,
	col=pal,
	breaks=seq(-1.5, 3, length.out=length(pal)),
	key=NULL,
	xaxt='n',
	yaxt='n',
	xlab='',
	ylab='',
	main='',
	axes=FALSE,
	axis.col=NULL,
	axis.row=NULL,
	asp=1
)
title('e  Third dimension (3.0%)', line=1.2, cex.main=2.3, xpd=NA)
mtext("Effect species", 1, outer=FALSE, line=1.5, xpd=NA, cex=1.75)
mtext("Response species", 2, outer=FALSE, line=0.0, xpd=NA, cex=1.75)
# mtext("+", 2, outer=FALSE, line=3.7, xpd=NA, cex=3.5)

# # plot the difference
# par(mar = c(4, 0, 5, 7))
plot(
	A,
	col=pal,
	breaks=seq(-1.5, 3, length.out=length(pal)),
	key=NULL,
	xaxt='n',
	yaxt='n',
	xlab='',
	ylab='',
	main='',
	axes=FALSE,
	axis.col=NULL,
	axis.row=NULL,
	asp=1
)
title('a  Interaction matrix', line=1.2, cex.main=3)
mtext("Effect species", 1, outer=FALSE, line=3, xpd=NA, cex=3)
mtext("Response species", 2, outer=FALSE, line=0.8, xpd=NA, cex=3)
# text(14.6, 5.5, "=", outer=TRUE, line=1, xpd=NA, cex=4.5)

# add a color scale for 

# pal <- brewer.pal(9, "Blues")
# pal <- colorRampPalette(pal)(10000)
par(mar = c(5, 1.5, 4.5, 3.0))
colorbarr <- t(matrix(seq(-1.5, 3, length.out=length(pal)), length(pal), 1))
image(
	colorbarr,
	col=pal,
	xaxt='n',
	yaxt='n',
	mgp=c(0.5,0.5,0.5)
)
text(1.75, 0.5, "Interaction strength", xpd=NA, cex=3.5, srt=270)
mtext("+", 1, outer=FALSE, line=1.5, xpd=NA, cex=2.25)
mtext("-", 3, outer=FALSE, line=0.5, xpd=NA, cex=2.25)

# dev.copy(device=x11)
# dev.print()#height=3, width=7)

dev.off()

# # comp <- expand.grid(z,E)
# # comp$N <- N[2]
