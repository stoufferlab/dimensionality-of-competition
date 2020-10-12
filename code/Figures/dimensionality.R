
library(RColorBrewer)
library(plotrix)

# read in GOF by dimensionality measures
godoyC <- "../../results/Godoy/godoy.C.pseudo-rsquared.csv"
godoyT <- "../../results/Godoy/godoy.T.pseudo-rsquared.csv"

# these datasets were fit from Levine matrix only
levine.dir <- "../../results/Levine/"
levine.files <- list.files(levine.dir, "rsquared")
levine.files <- paste0(levine.dir, levine.files)

# these datasets were fit from Kinlock matrix only
kinlock.dir <- "../../results/Kinlock/"
kinlock.files <- list.files(kinlock.dir, "rsquared")
kinlock.files <- paste0(kinlock.dir, kinlock.files)

# remove Engel due to errors in the data and we have it from Levine
kinlock.files <- grep("Engel",kinlock.files,invert=TRUE,value=TRUE)
# remove Goldberg & Landa because we have it from Levine
kinlock.files <- grep("Landa",kinlock.files,invert=TRUE,value=TRUE)

# put all files together
all.files <- c(godoyC, godoyT, goldberg, levine.files, kinlock.files)

dd <- do.call(rbind,sapply(
	all.files,
	function(f) {
		d <- read.table(f)
		return(cbind(nrow(d),igraph::dim_select(d[,1])))
	},
	simplify = FALSE
))

# replace Godoy values with the AIC estimate
dd[1,2] <- 3
dd[2,2] <- 3

# where to save the figure
h <- 5
setEPS(width=5.5, height=5)
postscript('../../manuscript/Figures/dimensionality.eps')

par(mar = c(2, 2.5, 0.5, 1.0), oma = c(2.5, 2.5, 0.5, 1.0))

# define some sizes
cex.axis <- 2
padj <- 0 #.25

# add a color scale for fitness

plot(
	dd,
	xlim=c(0,10),
	ylim=c(0,10),
	type='n',
	axes=FALSE,
	xlab='',
	ylab='',
	xaxs='i',
	yaxs='i'
	# asp=1
)

# add the x axis
axis(
	1,
	at=seq(0,10,2),
	tcl=0.5,
	cex.axis=cex.axis,
	padj=padj,
	gap.axis=0
)

# add the y axis
axis(
	2,
	at=seq(0,10,2),
	tcl=0.5,
	cex.axis=cex.axis,
	# padj=padj,
	las=1
)

# add a 45 degree line
abline(0,1,lwd=2.5,lty='dashed')
abline(h=1,lwd=2.5,lty='dotted')

# spread points out to show redundancy
xy <- cluster.overplot(dd[,1],dd[,2])

# plot the points
points(
	xy$x,
	xy$y,
	pch=21,
	lwd=1.5,
	bg="#a6cee3",
	cex=2,
	xpd=TRUE
)

mtext("Niche dimensionality", 2, outer=TRUE, line=0.5, cex=2)

mtext("Number of species", 1, outer=TRUE, line=1.0, cex=2)


dev.off()
