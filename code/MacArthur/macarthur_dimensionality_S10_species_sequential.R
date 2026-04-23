
library(ecodist)
library(plot.matrix)
library(RColorBrewer)
library(tidyverse)
library(zoo)

# read in relevant functions
source('lib/macarthur_functions.R')

# number of consumers
S <- 10

# number of resources
d <- 10

# number of steps
n_vals <- 100

# start with no variation
cr_parms <- random_cr_model(S, d, w_var = FALSE, c_var = FALSE, l_var = FALSE)

# cr_parms$w <- 0.5 + matrix(rnorm(10,0,0.05), S, d, byrow=TRUE)

# immediately make all niches different
cr_parms$c <- diag(S)
# cr_parms$c <- matrix(runif(S*S,0,0.05),S,S)
# diag(cr_parms$c) <- 1
# cr_parms$c <- sweep(
# 			cr_parms$c,
# 			1,
# 			rowSums(cr_parms$c),
# 			'/'
# 		)

# ondiag <- 0.95
# cfill <- (1 - ondiag) / (ondiag * (d-1))
# cr_parms$c <- matrix(cfill, S, d)
# diag(cr_parms$c) <- 1
# cr_parms$c <- sweep(
# 			cr_parms$c,
# 			1,
# 			rowSums(cr_parms$c),
# 			'/'
# 		)

cntr <- 1
res <- list()
for(sp in 2:S){
	for(val in seq(1,0,length.out=n_vals)){
		these_parms <- cr_parms
		these_parms$c[sp,] <- cr_parms$c[1,] * (1-val) + cr_parms$c[sp,] * val

		var_dat <- estimate_cr_variation(these_parms)

		res[[cntr]] <- var_dat

		res[[cntr]]$cr_parms <- these_parms
		res[[cntr]]$species <- sp
		res[[cntr]]$species_val <- val
		res[[cntr]]$species_ang <- matlib::angle(
			these_parms$c[sp,],
			these_parms$c[1,]
		)

		A <- cr_model_A_matrix(these_parms)$A

		Asub <- A[c(1,sp),c(1,sp)]
		res[[cntr]]$nicheoverlap <- sqrt(Asub[1,2] * Asub[2,1] / Asub[1,1] / Asub[2,2])

		res[[cntr]]$bray <- mean(ecodist::bcdist(these_parms$c))

		cntr <- cntr+1
	}
	cr_parms <- these_parms
}

# extract out the information we want for plotting
plot_data <- do.call(
	rbind,
	lapply(
		res,
		function(x){
			dimens <- min(which(x$d_tot_var > 0.95))
			res <- c(
				species = x$species,
				species_val = x$species_val,
				species_ang = x$species_ang,
				rsquared = x$d_tot_var[x$species - 1],
				nichediff = 1 - x$nicheoverlap,
				bray = x$bray,
				dimens = dimens
			)
			return(res)
		}
	)
) %>% as_tibble()

plot_data$step <- 1:length(res)

which.steps <- c(75,375,690)

p1 <- plot_data |>
	filter(is.finite(dimens)) |>
	# mutate(species_val = resource_val / 0.5) |>
	# mutate(species = 10 - species + 1 ) |>	
	ggplot(aes(
		x = step,
		y = dimens,
		# color = factor(letters[1 + 10 - species]),
		# group = species
	)) +
	theme_classic() +
	geom_line() +
	geom_vline(xintercept = which.steps, linetype = 'dotted') +
	scale_y_continuous(name = 'Inferred dimensionality', breaks = 1:10) +
	# scale_x_continuous(name = 'Position along species similarity tree', breaks = NULL, labels = NULL) +
	scale_x_continuous(name = '', breaks = which.steps, labels = c('a','b','c')) +
	theme(
		axis.title.x = element_text(margin = margin_auto(10))
	)
	# scale_color_discrete(
	# 	guide = guide_legend(
	# 		title = "Branch"
	# 	)
	# )

# plot(dimens ~ step, plot_data)

segs <- as.data.frame(rbind(
	c(0,10,9,10),
	c(0,9,1,10),
	c(0,8,1,8),
	c(1,8,2,10),
	c(0,7,2,7),
	c(2,7,3,10),
	c(0,6,3,6),
	c(3,6,4,10),
	c(0,5,4,5),
	c(4,5,5,10),
	c(0,4,5,4),
	c(5,4,6,10),
	c(0,3,6,3),
	c(6,3,7,10),
	c(0,2,7,2),
	c(7,2,8,10),
	c(0,1,8,1),
	c(8,1,9,10)
))

colnames(segs) <- c('x','y','xend','yend')

res_x <- range(plot_data$step)
res_seg <- c(min(segs$x),max(segs$xend))

segs$x <- res_x[1] + (segs$x - res_seg[1])/(res_seg[2] - res_seg[1]) * (res_x[2] - res_x[1])
segs$xend <- res_x[1] + (segs$xend - res_seg[1])/(res_seg[2] - res_seg[1]) * (res_x[2] - res_x[1])

segs$y <- 1 + 10 - segs$y
segs$yend <- 1 + 10 - segs$yend

p2 <- segs |>
	ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
	# theme_void() +
	theme_classic() +
	# theme(panel.background = element_blank()) +
	geom_segment() +
	geom_point(aes(x = res_x[1] + 1/9*(res_x[2] - res_x[1]), y = 1), shape = 21, color = 'black', fill = 'red', size = 5) +
	geom_point(aes(x = res_x[1] + 2/9*(res_x[2] - res_x[1]), y = 1), shape = 22, color = 'black', fill = 'blue', size = 5) +
	geom_point(aes(x = res_x[1] + 3/9*(res_x[2] - res_x[1]), y = 1), shape = 23, color = 'black', fill = 'white', size = 5) +
	scale_y_reverse(name = 'Species', breaks = 1:10) +
	scale_x_continuous(name = 'Position along species similarity tree', breaks = NULL, labels = NULL) +
	theme(
		axis.title.x = element_text(color = 'white'),
		# axis.title.y = element_text(color = 'white'),
		axis.line = element_line(color = 'white',linewidth = 0),
		# axis.text = element_text(color = 'white'),
		axis.ticks = element_line(color = 'white'),
		axis.ticks.length = unit(0,'cm')
	)

library(ggpubr)

p <- ggarrange(
	p2, p1,
	nrow = 2
)

ggsave(
	'macarthur_S10_species_sequential.pdf',
	p,
	width = 4,
	height = 6
)

pdf(
	'macarthur_S10_species_sequential_mats.pdf',
	width=10,
	height=8
)

layout(mat = rbind(
		c(1,2,3,4),
		c(5,6,7,8)
       ),
       widths = c(1.5, 1.5, 1.5, 0.5),
       heights = c(0.5,0.5)
)

par(mar = c(1.5, 1.5, 1.5, 1), oma = c(0, 1.75, 0, 3.75))

c.list <- lapply(
	res[which.steps],
	function(x){
		x$cr_parms$c
	}
)
A.list <- lapply(
	res[which.steps],
	function(x){
		cr_model_A_matrix(x$cr_parms)$A
	}
)

# limits of resource utilization
fmin <- min(unlist(c.list))
fmax <- max(unlist(c.list))

pal <- c(
	# rev(colorRampPalette(brewer.pal(9, "Blues"))(1000*abs(fmin)/(fmax - fmin))),
	(colorRampPalette(brewer.pal(9, "Reds"))(1000*abs(fmax)/(fmax - fmin)))
)

# plot the full matrices
sapply(
	seq_along(c.list),
	function(i,c.list){
		plot(
			c.list[[i]],
			col=pal,
			breaks=seq(fmin, fmax, length.out=length(pal)),
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
		text(0, 11.25, letters[i], xpd=NA, cex=2.5)
		if(i == 1){
			mtext('Species', 2, xpd=NA, cex=1.5, padj=-0.4)
			mtext('Resource', 1, xpd=NA, cex=1.5, padj=-2.4)
		}
		# text(3, 1.5, "=", xpd=NA, cex=4.5)
	},
	c.list = c.list
)

# plot the color bar
par(mar = c(6, 0.5, 6, 1.5))
colorbarr <- t(matrix(seq(fmin, fmax, length.out=length(pal)), length(pal), 1))
image(
	colorbarr,
	col=pal,
	xaxt='n',
	yaxt='n',
	mgp=c(0.5,0.5,0.5)
)
text(2.0, 0.5, "Resource utlization", xpd=NA, cex=2.0, srt=270)

mtext(paste0("High"), 1, outer=FALSE, line=1.3, xpd=NA, cex=1.5, adj=1)
mtext(paste0("Low "), 3, outer=FALSE, line=0.5, xpd=NA, cex=1.5, adj=1)


# limits of resource utilization
fmin <- min(unlist(A.list))
fmax <- max(unlist(A.list))

pal <- c(
	# rev(colorRampPalette(brewer.pal(9, "Blues"))(1000*abs(fmin)/(fmax - fmin))),
	(colorRampPalette(brewer.pal(9, "Reds"))(1000*abs(fmax)/(fmax - fmin)))
)

par(mar = c(1.5, 1.5, 1.5, 1))

# plot the full matrices
sapply(
	seq_along(A.list),
	function(i,A.list){
		plot(
			A.list[[i]],
			col=pal,
			breaks=seq(fmin, fmax, length.out=length(pal)),
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
		text(0, 11.25, letters[i], xpd=NA, cex=2.5)
		if(i == 1){
			mtext('Species', 2, xpd=NA, cex=1.5, padj=-0.4)
			mtext('Species', 1, xpd=NA, cex=1.5, padj=-2.4)
		}
		# text(3, 1.5, "=", xpd=NA, cex=4.5)
	},
	A.list = A.list
)

# plot the color bar
par(mar = c(6, 0.5, 6, 1.5))
colorbarr <- t(matrix(seq(fmin, fmax, length.out=length(pal)), length(pal), 1))
image(
	colorbarr,
	col=pal,
	xaxt='n',
	yaxt='n',
	mgp=c(0.5,0.5,0.5)
)
text(2.0, 0.5, "Interaction strength", xpd=NA, cex=2.0, srt=270)

mtext(paste0("Weak"), 1, outer=FALSE, line=1.3, xpd=NA, cex=1.5, adj=1)
mtext(paste0("Strong"), 3, outer=FALSE, line=0.5, xpd=NA, cex=1.5, adj=1)


dev.off()
