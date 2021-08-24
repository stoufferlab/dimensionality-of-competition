
# library(ggplot2)
library(RColorBrewer)
# library(plot.matrix)

# figure out what response/effect files exist

# godoy Control which is not in main text figure
godoy.dir <- "../../results/Spain"
godoy.resp <- grep("godoy.C",list.files(godoy.dir,"response[.][[:digit:]][.]csv",full.names=TRUE),value=TRUE)
godoy.effe <- grep("godoy.C",list.files(godoy.dir,"effect[.][[:digit:]][.]csv",full.names=TRUE),value=TRUE)

# Wainwright data
wainwright.dir <- "../../results/Australia"
wainwright.resp <- list.files(wainwright.dir,"response[.][[:digit:]][.]csv",full.names=TRUE)
wainwright.effe <- list.files(wainwright.dir,"effect[.][[:digit:]][.]csv",full.names=TRUE)

# # datasets from the Levine collection
# levine.dir <- "../../results/Levine"
# levine.resp <- list.files(levine.dir,"response[.][[:digit:]][.]csv",full.names=TRUE)
# levine.effe <- list.files(levine.dir,"effect[.][[:digit:]][.]csv",full.names=TRUE)

# datasets from the Kinlock collection
kinlock.dir <- "../../results/Kinlock"
kinlock.resp <- list.files(kinlock.dir,"response[.][[:digit:]][.]csv",full.names=TRUE)
kinlock.effe <- list.files(kinlock.dir,"effect[.][[:digit:]][.]csv",full.names=TRUE)

# # remove Engel and Landa studies since they are part of Levine collection
# kinlock.resp <- grep("Landa|Engel",kinlock.resp,invert=TRUE,value=TRUE)
# kinlock.effe <- grep("Landa|Engel",kinlock.effe,invert=TRUE,value=TRUE)

# put everything together
response.files <- c(
	"spain.control.response",
	"spain.treatment.response",
	"australia.open.response",
	"australia.shade.response",
	kinlock.resp
)
effect.files <- c(
	"spain.control.effect",
	"spain.treatment.effect",
	"australia.open.effect", 
	"australia.shade.effect",
	kinlock.effe
)

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
		if(grepl("spain|australia",stem)){
			100 #nlevels(read.table(grep(stem,alpha.fits,value=TRUE)[1])$row)
		}else{
			nrow(read.table(grep(stem,response.files,value=TRUE)[1]))
		}
	},
	response.files=response.files
)

# reorder in decreasing number of species
stems <- stems[order(spp, decreasing=TRUE)]

# if there are SEs how wide should we plot them?
se.factor <- 2

for(i in 1:4){

	setEPS(width=10, height=15)
	postscript(paste0('../../manuscript/Supplementary/Figures/response.effect.',i,'.eps'))

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
		if(j<5){
			for(which.trait in 1:3){
				if(j==1){
					all.fits <- read.table('../../results/Spain/spain.Control.fit.summary.csv')
					which.fit <- which.min(subset(all.fits, dimensions==2)$AIC)
					load(paste0('../../results/Spain/',rownames(subset(all.fits, dimensions==2)[which.fit,])))
					assign("optim.lowD", eval(parse(text = paste0("Control.optim.lowD"))))
				}else if(j==2){
					all.fits <- read.table('../../results/Spain/spain.Treatment.fit.summary.csv')
					which.fit <- which.min(subset(all.fits, dimensions==3)$AIC)
					load(paste0('../../results/Spain/',rownames(subset(all.fits, dimensions==3)[which.fit,])))
					assign("optim.lowD", eval(parse(text = paste0("Treatment.optim.lowD"))))
				}else if(j==3){
					all.fits <- read.table('../../results/Australia/australia.Open.fit.summary.csv')
					which.fit <- which.min(subset(all.fits, dimensions==1)$AIC)
					load(paste0('../../results/Australia/',rownames(subset(all.fits, dimensions==1)[which.fit,])))
					assign("optim.lowD", eval(parse(text = paste0("Open.optim.lowD"))))
				}else{
					all.fits <- read.table('../../results/Australia/australia.Shade.fit.summary.csv')
					which.fit <- which.min(subset(all.fits, dimensions==2)$AIC)
					load(paste0('../../results/Australia/',rownames(subset(all.fits, dimensions==2)[which.fit,])))
					assign("optim.lowD", eval(parse(text = paste0("Shade.optim.lowD"))))
				}
				if(which.trait <= optim.lowD@data$dimensions){
					source('../Utils/se.helper.R')
					if("Other" %in% rownames(effects)){
						effects <- effects[rownames(effects) != "Other",]
					}
					par(xpd=TRUE)
					plot(
						apply(effects, 1, quantile, probs=c(0.50)),
						apply(responses, 1, quantile, probs=c(0.50)),
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
					abline(h=0,lwd=1.5,lty='dotted')
					abline(v=0,lwd=1.5,lty='dotted')

					if(which.trait==1){
						mtext("Response trait", 2, outer=FALSE, line=3.25, xpd=NA, cex=1.5)
						l <- switch(as.character(j),
							"1" = "1-Wet",
							"2" = "1-Dry",
							"3" = "2-Sun",
							"4" = "2-Shade",
							j - 2
						)
						mtext(paste0("Dataset ",l), 2, outer=FALSE, line=5.5, xpd=NA, cex=1.75, font=2)
					}
					mtext("Effect trait", 1, outer=FALSE, line=2.75, xpd=NA, cex=1.5)

				}else{
					plot(
						0,0,
						type='l',
						axes=FALSE,
						xlab='',
						ylab='',
						xaxs='i',
						yaxs='i'
					)
				}
			}
				
		}else{
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

				# DEBUG ME
				# if(ncol(e.traits)>1){
				# 	segments(
				# 		e.traits[,1],
				# 		r.traits[,1] + se.factor*r.traits[,2],
				# 		e.traits[,1],
				# 		r.traits[,1] - se.factor*r.traits[,2],
				# 		lwd=1.5
				# 	)
				# 	segments(
				# 		e.traits[,1] + se.factor*e.traits[,2],
				# 		r.traits[,1],
				# 		e.traits[,1] - se.factor*e.traits[,2],
				# 		r.traits[,1],
				# 		lwd=1.5
				# 	)
				# }

				abline(h=0,lwd=1.5,lty='dotted')
				abline(v=0,lwd=1.5,lty='dotted')

				if(k==1){
					mtext("Response trait", 2, outer=FALSE, line=3.25, xpd=NA, cex=1.5)
					l <- switch(as.character(j),
						"1" = "1-Wet",
						"2" = "1-Dry",
						"3" = "2-Sun",
						"4" = "2-Shade",
						j - 2
					)
					mtext(paste0("Dataset ",l), 2, outer=FALSE, line=5.5, xpd=NA, cex=1.75, font=2)
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

					# axis(
					# 	1,
					# 	at=round(seq(-1,1,0.5),2),
					# 	tcl=0.5,
					# 	cex.axis=1.4,
					# 	padj=-0.5
					# )
					# axis(
					# 	2,
					# 	at=round(seq(-1,1,0.5),2),
					# 	tcl=0.5,
					# 	cex.axis=1.4,
					# 	# padj=0.5,
					# 	las=1
					# )

					# if(k==1) mtext("Response trait", 2, outer=FALSE, line=3.25, xpd=NA, cex=1.5)
					# mtext("Effect trait", 1, outer=FALSE, line=2.75, xpd=NA, cex=1.5)

					# text(0,0,"NA",cex=3)

				}
			}
			# break
		}
	}
	dev.off()
}
