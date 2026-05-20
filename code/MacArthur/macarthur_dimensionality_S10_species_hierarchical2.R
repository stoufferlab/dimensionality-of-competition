
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

# immediately make all niches different
cr_parms$c <- diag(S)

# steps for combining species together
merges <- list(
	list(
		from = c(2),
		to = c(1)
	),
	list(
		from = c(10),
		to = c(9)
	),
	list(
		from = c(6),
		to = c(5)
	),
	list(
		from = c(8),
		to = c(7)
	),
	list(
		from = c(4),
		to = c(3)
	),
	list(
		from = c(3,4),
		to = c(1,2)
	),
	list(
		from = c(9,10),
		to = c(7,8)
	),
	list(
		from = c(5,6),
		to = c(1,2,3,4)
	),
	list(
		from = c(7,8,9,10),
		to = c(1,2,3,4,5,6)
	)
)

# container
cntr <- 1
res <- list()

# hierarchically merge species together into complexes
for(i in 1:length(merges)){
	from_sp <- merges[[i]]$from
	to_sp <- merges[[i]]$to
	for(val in seq(1,0.5,length.out=n_vals)){
		these_parms <- cr_parms

		# to sp
		for(j in to_sp){
			these_parms$c[j,] <- cr_parms$c[j,] * val + cr_parms$c[from_sp[1],] * (1-val)
		}

		# from sp
		for(j in from_sp){
			these_parms$c[j,] <- cr_parms$c[to_sp[1],] * (1-val) + cr_parms$c[j,] * val
		}

		var_dat <- estimate_cr_variation(these_parms)

		res[[cntr]] <- var_dat

		res[[cntr]]$cr_parms <- these_parms
		res[[cntr]]$merge <- i
		res[[cntr]]$species_val <- val

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
			# estimate the dimensionality
			dimens <- min(which(x$d_tot_var > 0.95))

			res <- c(
				merge = x$merge,
				species_val = x$species_val,
				dimens = dimens
			)
			return(res)
		}
	)
) %>% as_tibble()

plot_data$step <- 1:length(res)

# points during process to highlight in additional panels
which.steps <- c(75,375,690)

p1 <- plot_data |>
	filter(is.finite(dimens)) |>
	ggplot(aes(x = step, y = dimens)) +
	theme_classic() +
	geom_line() +
	geom_vline(xintercept = which.steps, linetype = 'dotted') +
	scale_y_continuous(name = expression('Inferred dimensionality, '*italic(hat(d))), breaks = 1:10, limits=c(1,10)) +
	scale_x_continuous(name = '', breaks = which.steps, labels = c('x','y','z')) +
	theme(
		axis.title.x = element_text(margin = margin_auto(10)),
		plot.tag = element_text(face = 'bold')
	) +
	labs(tag = 'b')


segs <- as.data.frame(rbind(
	c(0,10,1,9.5),
	c(0,9,1,9.5),
	# c(0,9,1,10),
	c(0,8,4,8),
	c(4,8,5,7.5),
	c(0,7,4,7),
	c(4,7,5,7.5),
	c(0,6,2,6),
	c(2,6,3,5.5),
	c(0,5,2,5),
	c(2,5,3,5.5),
	# c(2,5,3,6),
	c(0,4,3,4),
	c(3,4,4,3.5),
	c(0,3,3,3),
	c(3,3,4,3.5),
	c(0,2,1,2),
	c(1,2,2,1.5),
	c(0,1,1,1),
	c(1,1,2,1.5),
	c(2,1.5,6,1.5),
	c(3,5.5,7,5.5),
	c(1,9.5,5,9.5),
	c(4,3.5,6,3.5),
	c(5,9.5,6,8.5),
	c(6,8.5,7,8.5),
	c(5,7.5,6,8.5),
	c(6,1.5,7,2.5),
	c(6,3.5,7,2.5),
	c(7,8.5,8,7.5),
	c(7,5.5,8,7.5),
	c(7,2.5,8,2.5),
	c(8,7.5,9,5.5),
	c(8,2.5,9,5.5)
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
	theme_classic() +
	geom_segment() +
	geom_point(aes(x = res_x[1] + 1/9*(res_x[2] - res_x[1]), y = 1.5), shape = 21, color = 'black', fill = 'red', size = 5) +
	geom_point(aes(x = res_x[1] + 2/9*(res_x[2] - res_x[1]), y = 9.5), shape = 22, color = 'black', fill = 'blue', size = 5) +
	geom_point(aes(x = res_x[1] + 3/9*(res_x[2] - res_x[1]), y = 5.5), shape = 23, color = 'black', fill = 'white', size = 5) +
	scale_y_reverse(name = expression('Species, '*italic(i)), breaks = 1:10) +
	scale_x_continuous(name = 'Position along species similarity tree', breaks = NULL, labels = NULL) +
	theme(
		axis.title.x = element_text(color = 'white'),
		axis.line = element_line(color = 'white',linewidth = 0),
		axis.ticks = element_line(color = 'white'),
		axis.ticks.length = unit(0,'cm'),
		plot.tag = element_text(face = 'bold')
	) +
	labs(tag = 'a')

library(ggpubr)

p <- ggarrange(
	p2, p1,
	nrow = 2
)

ggsave(
	'macarthur_S10_species_hierarchical2.pdf',
	p,
	width = 4,
	height = 6
)

pdf(
	'macarthur_S10_species_hierarchical2_mats.pdf',
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
	'white',
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
		text(
			5,
			11.25,
			paste0("at point ",letters[i+23]),
			xpd=NA,
			cex=2.0
		)
		if(i == 1){
			mtext('Species', 2, xpd=NA, cex=1.5, padj=-0.4)
			mtext('Resource', 1, xpd=NA, cex=1.5, padj=-2.4)
			mtext(expression(bold(c)*' Resource utilization matrices, '*italic(C)*"      "), 3, xpd=NA, cex=2, adj=0, at = 0, line = -1)
		}
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
text(2.0, 0.5, expression("Resource utlization, "*italic(c[ik])), xpd=NA, cex=2.0, srt=270)

mtext(paste0("Low "), 1, outer=FALSE, line=1.3, xpd=NA, cex=1.5, adj=1)
mtext(paste0("High "), 3, outer=FALSE, line=0.5, xpd=NA, cex=1.5, adj=1)


# limits of resource utilization
fmin <- min(unlist(A.list))
fmax <- max(unlist(A.list))

pal <- c(
	'white',
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
		text(
			5,
			11.25,
			paste0("at point ",letters[i+23]),
			xpd=NA,
			cex=2.
		)
		if(i == 1){
			mtext('Species', 2, xpd=NA, cex=1.5, padj=-0.4)
			mtext('Species', 1, xpd=NA, cex=1.5, padj=-2.4)
			mtext(expression(bold(d)*' Interaction matrices, '*italic(A)), 3, xpd=NA, cex=2, adj=0, at = 0, line = -1)
		}
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
text(2., 0.5, expression("Interaction strength, "*italic(a[ij])), xpd=NA, cex=2.0, srt=270)

mtext(paste0("Weak"), 1, outer=FALSE, line=1.3, xpd=NA, cex=1.5, adj=1)
mtext(paste0("Strong"), 3, outer=FALSE, line=0.5, xpd=NA, cex=1.5, adj=1)


dev.off()
