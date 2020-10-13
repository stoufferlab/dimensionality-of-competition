
# library(ggplot2)
library(RColorBrewer)
# library(plot.matrix)

# figure out what response/effect files exist

# godoy Control which is not in main text figure
godoy.dir <- "../../results/Godoy"
godoy.resp <- grep("godoy.C",list.files(godoy.dir,"response[.][[:digit:]][.]csv",full.names=TRUE),value=TRUE)
godoy.effe <- grep("godoy.C",list.files(godoy.dir,"effect[.][[:digit:]][.]csv",full.names=TRUE),value=TRUE)

# Wainwright data
wainwright.dir <- "../../results/Wainwright"
wainwright.resp <- list.files(wainwright.dir,"response[.][[:digit:]][.]csv",full.names=TRUE)
wainwright.effe <- list.files(wainwright.dir,"effect[.][[:digit:]][.]csv",full.names=TRUE)

# datasets from the Levine collection
levine.dir <- "../../results/Levine"
levine.resp <- list.files(levine.dir,"response[.][[:digit:]][.]csv",full.names=TRUE)
levine.effe <- list.files(levine.dir,"effect[.][[:digit:]][.]csv",full.names=TRUE)

# datasets from the Kinlock collection
kinlock.dir <- "../../results/Kinlock"
kinlock.resp <- list.files(kinlock.dir,"response[.][[:digit:]][.]csv",full.names=TRUE)
kinlock.effe <- list.files(kinlock.dir,"effect[.][[:digit:]][.]csv",full.names=TRUE)

# remove Engel and Landa studies since they are part of Levine collection
kinlock.resp <- grep("Landa|Engel",kinlock.resp,invert=TRUE,value=TRUE)
kinlock.effe <- grep("Landa|Engel",kinlock.effe,invert=TRUE,value=TRUE)

# put everything together
response.files <- c(godoy.resp, wainwright.resp, levine.resp, kinlock.resp)
effect.files <- c(godoy.effe, wainwright.effe, levine.effe, kinlock.effe)

# determine unique stems/datasets
stems <- unique(sapply(
	response.files,
	function(x){
		strsplit(x,".response")[[1]][1]
	}
))

# determine the number of species to order correctly relative to other figures
spp <- sapply(
	stems,
	function(stem,response.files){
		nrow(read.table(grep(stem,response.files,value=TRUE)[1]))
	},
	response.files=response.files
)

# reorder in decreasing number of species
stems <- stems[order(spp, decreasing=TRUE)]

# if there are SEs how wide should we plot them?
se.factor <- 2


for(i in 1:9){

	setEPS(width=10, height=15)
	postscript(paste0('../../manuscript/Figures/Supps/response.effect.',i,'.eps'))

	layout(mat = matrix(
			1:12,
			byrow=TRUE,
	        nrow = 4, 
	        ncol = 3
	       ),
	       heights = c(1),
	       widths = c(1, 1, 1)
	)

	par(mar = c(5, 4.5, 4.5, 1), oma = c(0, 3.75, 0, 0.5), xpd=TRUE)

	for(j in (1+4*(i-1)):min(4*i,length(stems))){
		stem <- stems[j]

		resp <- grep(stem,response.files,value=TRUE)
		effe <- grep(stem,effect.files,value=TRUE)

		for(k in 1:length(resp)){
			r.traits <- read.table(resp[k])
			e.traits <- read.table(effe[k])
			e.traits <- e.traits[grep("Other",rownames(e.traits),invert=TRUE,value=TRUE),,drop=FALSE]

			# check for less intuitive sign structure and reverse signs if needed
			if(sum((r.traits[,1]<0) + (e.traits[,1]<0))/(nrow(r.traits)+nrow(e.traits)) > 0.5){
				r.traits <- -r.traits
				e.traits <- -e.traits
			}
			
			# DEBUG check for SEs
			par(xpd=TRUE)
			plot(
				e.traits[,1],
				r.traits[,1],
				xlim=c(-1,1),
				ylim=c(-1,1),
				xlab='',
				ylab='',
				pch=21,
				lwd=1.5,
				bg="#a6cee3",
				cex=2,
				yaxs='i',
				xaxs='i',
				axes=FALSE
			)
			par(xpd=FALSE)

			axis(
				1,
				at=round(seq(-1,1,0.5),2),
				tcl=0.5,
				cex.axis=1.4,
				padj=-0.5
			)
			axis(
				2,
				at=round(seq(-1,1,0.5),2),
				tcl=0.5,
				cex.axis=1.4,
				# padj=0.5,
				las=1
			)

			if(ncol(e.traits)>1){
				segments(
					e.traits[,1],
					r.traits[,1] + se.factor*r.traits[,2],
					e.traits[,1],
					r.traits[,1] - se.factor*r.traits[,2],
					lwd=1.5
				)
				segments(
					e.traits[,1] + se.factor*e.traits[,2],
					r.traits[,1],
					e.traits[,1] - se.factor*e.traits[,2],
					r.traits[,1],
					lwd=1.5
				)
			}

			abline(h=0,lwd=1.5,lty='dotted')
			abline(v=0,lwd=1.5,lty='dotted')

			if(k==1){
				mtext("Response trait", 2, outer=FALSE, line=3.25, xpd=NA, cex=1.5)
				mtext(paste0("Dataset ",j), 2, outer=FALSE, line=5.5, xpd=NA, cex=1.75, font=2)
			}
			mtext("Effect trait", 1, outer=FALSE, line=2.75, xpd=NA, cex=1.5)

		}

		if(length(resp)<3){
			for(k in (length(resp)+1):3){
				plot(
					0,0,
					type='n',
					axes=FALSE,
					xlab='',
					ylab='',
					xaxs='i',
					yaxs='i'
				)

				axis(
					1,
					at=round(seq(-1,1,0.5),2),
					tcl=0.5,
					cex.axis=1.4,
					padj=-0.5
				)
				axis(
					2,
					at=round(seq(-1,1,0.5),2),
					tcl=0.5,
					cex.axis=1.4,
					# padj=0.5,
					las=1
				)

				if(k==1) mtext("Response trait", 2, outer=FALSE, line=3.25, xpd=NA, cex=1.5)
				mtext("Effect trait", 1, outer=FALSE, line=2.75, xpd=NA, cex=1.5)

				text(0,0,"NA",cex=3)

			}
		}
		# break
	}

	dev.off()
}
