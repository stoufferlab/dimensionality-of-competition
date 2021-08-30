
library(RColorBrewer)
library(plotrix)

# read in GOF by dimensionality measures
godoyC <- "../../results/Spain/spain.Control.pseudo-rsquared.csv"
godoyT <- "../../results/Spain/spain.Treatment.pseudo-rsquared.csv"

wainwrightOpen <- "../../results/Australia/australia.Open.pseudo-rsquared.csv"
wainwrightShade <- "../../results/Australia/australia.Shade.pseudo-rsquared.csv"

# # these datasets were fit from Levine matrix only
# levine.dir <- "../../results/Levine/"
# levine.files <- list.files(levine.dir, "rsquared")
# levine.files <- paste0(levine.dir, levine.files)

# these datasets were fit from Kinlock matrix only
kinlock.dir <- "../../results/Kinlock/"
kinlock.files <- list.files(kinlock.dir, "rsquared")
kinlock.files <- paste0(kinlock.dir, kinlock.files)

# remove Engel due to errors in the data and we have it from Levine
# remove Goldberg & Landa because we have it from Levine
# kinlock.files <- grep("Engel|Landa",kinlock.files,invert=TRUE,value=TRUE)

# put all files together
all.files <- c(
	godoyC,
	godoyT,
	wainwrightOpen,
	wainwrightShade,
	kinlock.files
)

dd <- do.call(rbind,sapply(
	all.files,
	function(f) {
		d <- read.table(f)
		return(cbind(
			nrow(d),
			igraph::dim_select(d[,1]),
			min(which(d[,3] > 0.95)),
			d[1,2],
			d[igraph::dim_select(d[,1]),3],
			d[min(which(d[,3] > 0.95)),3]
		))
	},
	simplify = FALSE
))

dd <- as.data.frame(dd)
colnames(dd) <- c(
	"species",
	"dimselect",
	"ninetyfivepercent",
	"varexp.first",
	"varexp.dimselect",
	"varexp.ninetyfivepercent"
)
rownames(dd) <- all.files

# # replace Godoy values with the AIC estimate
# dd[1,2] <- 3
# dd[2,2] <- 3

# # replace Wainwright values with the AIC estimate
# dd[3,2] <- 1
# dd[4,2] <- 1

# where to save the figure
h <- 5
setEPS(width=5.5, height=3.7)
postscript('../../manuscript/Figures/dimensionality.eps')

par(mar = c(2, 2.5, 0.5, 1.0), oma = c(2.5, 2.5, 0.75, 1.0))

# define some sizes
cex.axis <- 1.7
padj <- 0 #.25

# add a color scale for fitness

plot(
	x=NA,
	y=NA,
	xlim=c(1,12),
	ylim=c(1,12),
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
	at=c(1,4,8,12),
	tcl=0.5,
	cex.axis=cex.axis,
	padj=padj,
	gap.axis=0
)

# add the y axis
axis(
	2,
	at=c(1,4,8,12),
	# labels=c("","1","4","8","12"),
	tcl=0.5,
	cex.axis=cex.axis,
	# padj=padj,
	las=1
)

# add a 45 degree line
abline(0,1,lwd=2.5,lty='dotted')
#abline(h=1,lwd=2.5,lty='dotted')

xpred <- seq(1,12,length.out=1000)

prediction <- predict(
	glm(ninetyfivepercent ~ 0+I(species-1), family="poisson", data=dd),
	newdata=data.frame(species=xpred),
	#type="response",
	se.fit=TRUE
)

polygon(
	x=c(xpred, rev(xpred)),
	y=exp(c(prediction$fit+prediction$se.fit, rev(prediction$fit-prediction$se.fit))),
	col=grey(0.75),
	border=NA
)

lines(
	x=xpred,
	y=exp(prediction$fit),
	col=grey(0.25)
)

# spread points out to show redundancy
xy <- cluster.overplot(
	dd[,"species"],
	dd[,"ninetyfivepercent"],
	away=c(0.25,0.25)
)

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

mtext("Niche dimensionality", 2, line=3.15, xpd=NA, adj=0.5, cex=2)

mtext("Species richness", 1, outer=FALSE, xpd=NA, line=3.15, cex=2, adj=0.50)



dev.off()
