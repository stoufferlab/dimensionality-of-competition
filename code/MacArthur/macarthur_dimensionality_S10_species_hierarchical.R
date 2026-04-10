
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
n_vals <- 100

# start with no variation
cr_parms <- random_cr_model(S, d, w_var = FALSE, c_var = FALSE, l_var = FALSE)

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
		to = c(1)
	),
	list(
		from = c(9,10),
		to = c(7)
	),
	list(
		from = c(5,6),
		to = c(1)
	),
	list(
		from = c(7,8,9,10),
		to = c(1)
	)
)

# container
cntr <- 1
res <- list()

# hierarchically merge species together into complexes
for(i in 1:length(merges)){
	from_sp <- merges[[i]]$from
	to_sp <- merges[[i]]$to
	for(val in seq(1,0,length.out=n_vals)){
		these_parms <- cr_parms
		for(j in from_sp){
			these_parms$c[j,] <- cr_parms$c[to_sp,] * (1-val) + cr_parms$c[j,] * val
		}

		var_dat <- estimate_cr_variation(these_parms)

		res[[cntr]] <- var_dat

		res[[cntr]]$cr_parms <- these_parms
		# res[[cntr]]$species <- sp
		res[[cntr]]$merge <- i
		res[[cntr]]$species_val <- val

		A <- cr_model_A_matrix(these_parms)$A

		Asub <- A[c(to_sp,from_sp[1]),c(to_sp,from_sp[1])]
		res[[cntr]]$nicheoverlap <- sqrt(Asub[1,2] * Asub[2,1] / Asub[1,1] / Asub[2,2])

		res[[cntr]]$pianka <- sum(these_parms$c[to_sp,] * these_parms$c[from_sp[1],]) / sqrt(sum(these_parms$c[to_sp,]**2) * sum(these_parms$c[from_sp[1],]**2))

		res[[cntr]]$bray <- mean(ecodist::bcdist(these_parms$c))
		# res[[cntr]]$species_ang <- matlib::angle(
		# 	these_parms$c[sp,],
		# 	these_parms$c[1,]
		# )

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

			# return(dimens)
			res <- c(
				merge = x$merge,
				species_val = x$species_val,
				# species_ang = x$species_ang,
				nichediff = 1 - x$nicheoverlap, #ifelse(dimens == 1, NA, x$d_tot_var[dimens - 1]),
				pianka = 1 - x$pianka,
				bray = x$bray,
				dimens = dimens
			)
			return(res)
		}
	)
) %>% as_tibble()

plot_data$step <- 1:length(res)

p1 <- plot_data |>
	filter(is.finite(dimens)) |>
	# mutate(species_val = resource_val / 0.5) |>
	# mutate(species = 10 - species + 1 ) |>	
	ggplot(aes(x = step, y = dimens)) + #, group = factor(letters[merge]), color = factor(letters[merge]))) +#, color = fct_rev(factor(species)), group = species)) +
	theme_classic() +
	geom_line() +
	# ylim(c(1,10)) +
	scale_y_continuous(name = 'Dimensionality', breaks = 1:10) +
	scale_x_continuous(name = 'Position along similarity tree', breaks = NULL, labels = NULL) +
	theme(
		axis.title.x = element_text(margin = margin_auto(10))
	)
	# scale_color_discrete(
	# 	guide = guide_legend(
	# 		title = "Branch"
	# 	)
	# )


segs <- as.data.frame(rbind(
	c(0,10,9,10),
	c(0,9,1,10),
	c(0,8,5,8),
	c(5,8,6,10),
	c(0,7,4,7),
	c(4,7,5,8),
	c(0,6,7,6),
	c(7,6,8,10),
	c(0,5,2,5),
	c(2,5,3,6),
	c(0,4,8,4),
	c(8,4,9,10),
	c(0,3,3,3),
	c(3,3,4,4),
	c(0,2,6,2),
	c(6,2,7,4),
	c(0,1,1,1),
	c(1,1,2,2)
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
	geom_point(aes(x = res_x[1] + 5/9*(res_x[2] - res_x[1]), y = 3), shape = 21, color = 'black', fill = 'blue', size = 5) +
	geom_point(aes(x = res_x[1] + 6/9*(res_x[2] - res_x[1]), y = 1), shape = 21, color = 'black', fill = 'white', size = 5) +
	scale_y_reverse(name = 'Species', breaks = 1:10) +
	scale_x_continuous(name = 'Position along tree', breaks = NULL, labels = NULL) +
	theme(
		axis.title.x = element_text(color = 'white'),
		# axis.title.y = element_text(color = 'white'),
		axis.line = element_line(linewidth = 0),
		# axis.text = element_text(color = 'white'),
		axis.ticks = element_line(color = 'white')
	)

library(ggpubr)

p <- ggarrange(
	p2, p1,
	nrow = 2
)

print(p)



# plot(dimens ~ step, plot_data)

# plot_data |>
# 	filter(is.finite(dimens)) |>
# 	# filter(species > 8) |>
# 	# mutate(species_val = resource_val / 0.5) |>
# 	# mutate(species = 10 - species + 1 ) |>	
# 	ggplot(aes(x = species_val, y = rsquared, color = fct_rev(factor(merge)), group = merge)) +
# 	theme_classic() +
# 	geom_line() +
# 	# ylim(c(0.5,1)) +
# 	geom_hline(yintercept = 0.95, linetype = 'dotted') +
# 	# scale_y_continuous(name = 'Total variation captured by leading dimensions', limits = c(0.5,1)) +
# 	scale_x_continuous(name = "Extent focal species shows niche differences") +
# 	scale_color_discrete(
# 		guide = guide_legend(
# 			title = "Focal species"
# 		)
# 	)
