
library(ecodist)
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

# immediately make all niches different
cr_parms$c <- diag(S)

cntr <- 1
res <- list()
for(resource in d:2){
	for(val in seq(0.5,0,length.out=n_vals)){
		cr_parms$K_div_rho[resource] <- val

		var_dat <- estimate_cr_variation(cr_parms)

		res[[cntr]] <- var_dat

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

p1 <- plot_data |>
	filter(is.finite(dimens)) |>
	mutate(resource_val = resource_val / 0.5) |>
	mutate(resource = factor(resource, levels = rev(sort(unique(resource))))) |>	
	ggplot(aes(x = step, y = dimens)) +
	theme_classic() +
	geom_line() +
	ylim(c(1,10)) +
	scale_y_continuous(name = 'Inferred dimensionality', breaks = 1:10) +
	scale_x_continuous(name = "Extent focal resource is limiting", breaks = NULL, labels = NULL) +
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
		axis.ticks = element_line(color = 'white')
	)

# segs <- as.data.frame(rbind(
# 	c(0,10,9,10),
# 	c(0,9,1,10),
# 	c(0,8,5,8),
# 	c(5,8,6,10),
# 	c(0,7,4,7),
# 	c(4,7,5,8),
# 	c(0,6,7,6),
# 	c(7,6,8,10),
# 	c(0,5,2,5),
# 	c(2,5,3,6),
# 	c(0,4,8,4),
# 	c(8,4,9,10),
# 	c(0,3,3,3),
# 	c(3,3,4,4),
# 	c(0,2,6,2),
# 	c(6,2,7,4),
# 	c(0,1,1,1),
# 	c(1,1,2,2)
# ))

# colnames(segs) <- c('x','y','xend','yend')

res_x <- range(plot_data$step)
# res_seg <- c(min(segs$x),max(segs$xend))

# segs$x <- res_x[1] + (segs$x - res_seg[1])/(res_seg[2] - res_seg[1]) * (res_x[2] - res_x[1])
# segs$xend <- res_x[1] + (segs$xend - res_seg[1])/(res_seg[2] - res_seg[1]) * (res_x[2] - res_x[1])

# segs$y <- 1 + 10 - segs$y
# segs$yend <- 1 + 10 - segs$yend

p2 <- ggplot() +
	# theme_void() +
	theme_classic() +
	# theme(panel.background = element_blank()) +
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
		axis.line = element_line(linewidth = 0),
		# axis.text = element_text(color = 'white'),
		axis.ticks = element_line(color = 'white')
	)

# plot(p2)

library(ggpubr)

p <- ggarrange(
	p2, p1,
	nrow = 2
)

print(p)

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

