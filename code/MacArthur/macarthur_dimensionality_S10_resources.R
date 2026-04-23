
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
n_vals <- 1000

# start with no variation
cr_parms <- random_cr_model(S, d, w_var = FALSE, c_var = FALSE, l_var = FALSE)

# make all species have unique resource utilization
cr_parms$c <- diag(S)

cntr <- 1
res <- list()
for(resource in d:2){
	for(val in seq(0.5,0.0,length.out=n_vals)){
		cr_parms$K_div_rho[resource] <- val

		var_dat <- estimate_cr_variation(cr_parms)

		res[[cntr]] <- var_dat
		res[[cntr]]$cr_parms <- cr_parms
		res[[cntr]]$resource <- resource
		res[[cntr]]$resource_val <- val

		cntr <- cntr+1
	}
}

# extract out the information we want for plotting
plot_data <- do.call(
	rbind,
	lapply(
		res,
		function(x){
			dimens <- min(which(x$d_tot_var > 0.95))
			res <- c(
				resource = x$resource,
				resource_val = x$resource_val,
				rsquared = x$d_tot_var[x$resource - 1],
				dimens = dimens
			)
			return(res)
		}
	)
) %>% as_tibble()

plot_data$step <- 1:nrow(plot_data)

which.steps <- c(1,4500,9000)

p1 <- plot_data |>
	filter(is.finite(dimens)) |>
	mutate(resource_val = resource_val / 0.5) |>
	mutate(resource = factor(resource, levels = rev(sort(unique(resource))))) |>	
	ggplot(aes(x = step, y = dimens)) +
	theme_classic() +
	geom_line() +
	# ylim(c(1,10)) +
	geom_vline(xintercept = which.steps, linetype = 'dotted') +
	scale_y_continuous(name = 'Inferred dimensionality', breaks = 1:10) +
	# scale_x_continuous(name = "Extent focal resource is limiting", breaks = NULL, labels = NULL) +
	scale_x_continuous(name = '', breaks = which.steps, labels = c('a','b','c')) +
	scale_color_discrete(
		guide = guide_legend(
			title = "Focal resource"
		)
	) +
	theme(
		axis.title.x = element_text(color = 'white'),
		# axis.title.y = element_text(color = 'white'),
		# axis.line = element_line(linewidth = 0),
		# axis.text = element_text(color = 'white'),
		axis.ticks = element_line(color = 'white'),
		axis.ticks.length = unit(0,'cm')
	)

p2 <- ggplot() +
	theme_classic() +
	geom_line(aes(x = seq(1,9000,length.out=100), y = 1, alpha = 1, linewidth=2)) +
	geom_line(aes(x = seq(1,8000,length.out=100), y = 2, alpha = 1, linewidth=2)) +
	geom_line(aes(x = seq(8000,9000,length.out=100), y = 2, alpha = seq(1,0,length.out=100), linewidth=2)) +
	geom_line(aes(x = seq(1,7000,length.out=100), y = 3, alpha = 1, linewidth=2)) +
	geom_line(aes(x = seq(7000,8000,length.out=100), y = 3, alpha = seq(1,0,length.out=100), linewidth=2)) +
	geom_line(aes(x = seq(1,6000,length.out=100), y = 4, alpha = 1, linewidth=2)) +
	geom_line(aes(x = seq(6000,7000,length.out=100), y = 4, alpha = seq(1,0,length.out=100), linewidth=2)) +
	geom_line(aes(x = seq(1,5000,length.out=100), y = 5, alpha = 1, linewidth=2)) +
	geom_line(aes(x = seq(5000,6000,length.out=100), y = 5, alpha = seq(1,0,length.out=100), linewidth=2)) +
	geom_line(aes(x = seq(1,4000,length.out=100), y = 6, alpha = 1, linewidth=2)) +
	geom_line(aes(x = seq(4000,5000,length.out=100), y = 6, alpha = seq(1,0,length.out=100), linewidth=2)) +
	geom_line(aes(x = seq(1,3000,length.out=100), y = 7, alpha = 1, linewidth=2)) +
	geom_line(aes(x = seq(3000,4000,length.out=100), y = 7, alpha = seq(1,0,length.out=100), linewidth=2)) +
	geom_line(aes(x = seq(1,2000,length.out=100), y = 8, alpha = 1, linewidth=2)) +
	geom_line(aes(x = seq(2000,3000,length.out=100), y = 8, alpha = seq(1,0,length.out=100), linewidth=2)) +
	geom_line(aes(x = seq(1,1000,length.out=100), y = 9, alpha = 1, linewidth=2)) +
	geom_line(aes(x = seq(1000,2000,length.out=100), y = 9, alpha = seq(1,0,length.out=100), linewidth=2)) +
	# geom_line(aes(x = seq(1,8000,length.out=100), y = 2, alpha = 1)) +
	geom_line(aes(x = seq(1,1000,length.out=100), y = 10, alpha = seq(1,0,length.out=100), linewidth=2)) +
	geom_point(aes(x = 1000, y = 10), shape = 21, color = 'black', fill = 'red', size = 5) +
	geom_point(aes(x = 3000, y = 8), shape = 23, color = 'black', fill = 'blue', size = 5) +
	geom_point(aes(x = 2000, y = 9), shape = 22, color = 'black', fill = 'white', size = 5) +
	scale_y_reverse(name = 'Resource', breaks = 1:10) +
	scale_x_continuous(name = 'Position along species similarity tree', breaks = NULL, labels = NULL) +
	theme(
		legend.position = "none",
		axis.title.x = element_text(color = 'white'),
		# axis.title.y = element_text(color = 'white'),
		axis.line = element_line(color = 'white',linewidth = 0),
		# axis.text = element_text(color = 'white'),
		axis.ticks = element_line(color = 'white')
	)

library(ggpubr)

p <- ggarrange(
	p2, p1,
	nrow = 2
)

# save combined plot
ggsave(
	'macarthur_S10_resources.pdf',
	p,
	width = 4,
	height = 6
)

# make a second plot of the interaction matrices to guide the reader

pdf(
	'macarthur_S10_resources_mats.pdf',
	width=10,
	height=4
)

layout(mat = cbind(
		matrix(1:3,1,3,byrow=TRUE),
		c(4)
       ),
       widths = c(1.5, 1.5, 1.5, 0.5),
       heights = rep(0.25,4)
)

par(mar = c(1.5, 1.5, 1.5, 1), oma = c(0, 1.75, 0, 3.75))

c.list <- lapply(
	res[c(1,4500,9000)],
	function(x){
		x$cr_parms$c
	}
)
A.list <- lapply(
	res[c(1,4500,9000)],
	function(x){
		cr_model_A_matrix(x$cr_parms)$A
	}
)

# limits of resource utilization
fmin <- 0
fmax <- max(A.list[[1]])

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
			mtext('Species', 1, xpd=NA, cex=1.5, padj=-2.4)
		}
		# text(3, 1.5, "=", xpd=NA, cex=4.5)
	},
	c.list = A.list
)

# plot the color bar
par(mar = c(6.0, 0.5, 6, 1.5))
colorbarr <- t(matrix(seq(fmin, fmax, length.out=length(pal)), length(pal), 1))
image(
	colorbarr,
	col=pal,
	xaxt='n',
	yaxt='n',
	mgp=c(0.5,0.5,0.5)
)
text(2.00, 0.5, "Interaction strength", xpd=NA, cex=2.0, srt=270)

mtext(paste0("Weak"), 1, outer=FALSE, line=1.3, xpd=NA, cex=1.5, adj=1)
mtext(paste0("Strong"), 3, outer=FALSE, line=0.5, xpd=NA, cex=1.5, adj=1)

dev.off()

# plot_data |>
# 	filter(is.finite(dimens)) |>
# 	mutate(resource_val = resource_val / 0.5) |>
# 	mutate(resource = factor(resource, levels = rev(sort(unique(resource))))) |>
# 	ggplot(aes(x = resource_val, y = rsquared, color = resource, group = resource)) +
# 	theme_classic() +
# 	geom_line() +
# 	# ylim(c(1,10)) +
# 	scale_y_continuous(name = 'Total variation captured by leading dimensions') +
# 	scale_x_continuous(name = "Extent focal resource is limiting") +
# 	geom_hline(yintercept = 0.95, linetype = 'dotted') +
# 	scale_color_discrete(
# 		guide = guide_legend(
# 			title = "Focal resource"
# 		)
# 	)

