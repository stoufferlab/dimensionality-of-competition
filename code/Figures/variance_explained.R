
# library(ggplot2)
library(RColorBrewer)
# library(plot.matrix)

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

# read in the data sets
rsquareds <- sapply(all.files, function(x) read.table(x), simplify=FALSE)

rsquareds <- rsquareds[order(sapply(rsquareds, nrow), decreasing=TRUE)]

# where to save the figure
setEPS(width=15, height=15)
postscript('../../manuscript/Supplementary/Figures/variance_explained.eps')

layout(mat = matrix(
		1:16,
		byrow=TRUE,
        nrow = 4, 
        ncol = 4
       ),
       heights = c(1),
       widths = c(2, 2, 2, 2)
)

par(mar = c(2, 2.5, 2, 2.5), oma = c(3.5, 4.5, 0.5, 0.5))

# define some sizes
cex.axis <- 2
padj <- 0.25



# wainwrights <- sapply(wainwright.files, function(x) read.table(x), simplify=FALSE)

# rsquareds <- c(wainwrights, rsquareds)

set.seed(123)
pal <- sample(colorRampPalette(brewer.pal(8, "Dark2"))(length(rsquareds)))

for(i in 1:(length(rsquareds))){
	d <- rsquareds[[i]]

	plot(
		c(0,1:nrow(d)),
		c(0,d[,2]),
		xaxs='i',
		yaxs='i',
		axes=FALSE,
		xlim=c(-0.5,0.5+nrow(d)),
		ylim=c(0,1),
		type='l'
	)
	axis(
		1,
		at=-1:(nrow(d)+2),
		tcl=0.5,
		cex.axis=cex.axis,
		padj=padj
	)
	axis(
		2,
		at=seq(0,1,0.2),
		tcl=0.5,
		cex.axis=cex.axis,
		# padj=padj,
		las=1
	)

	abline(
		v = min(which(d[,3]>0.95)),
		lwd=2.5,
		lty="dashed"
	)

	points(
		c(0,1:nrow(d)),
		c(0,d[,2]),
		pch=21,
		lwd=1.5,
		bg=pal[i],
		cex=2,
		xpd=TRUE
	)

	l <- switch(as.character(i),
		"1" = "1-Wet",
		"2" = "1-Dry",
		"3" = "2-Sun",
		"4" = "2-Shade",
		i - 2
	)
		
	text(paste0("Dataset ",l), x=0.7*nrow(d), y=0.8, xpd=NA, cex=2, font=2, pos=3)

	if(i == 2){
		mtext("Niche variation explained per niche dimension", 2, outer=TRUE, line=1.8, cex=2)
	}

	if(i == 6){
		mtext("Niche dimension", 1, outer=TRUE, line=1.8, cex=2)
	}

}

dev.off()
