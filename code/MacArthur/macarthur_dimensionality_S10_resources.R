
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

plot_data |>
	filter(is.finite(dimens)) |>
	mutate(resource_val = resource_val / 0.5) |>
	mutate(resource = factor(resource, levels = rev(sort(unique(resource))))) |>	
	ggplot(aes(x = resource_val, y = dimens, color = resource, group = resource)) +
	theme_classic() +
	geom_line() +
	ylim(c(1,10)) +
	scale_y_continuous(name = 'Inferred dimensionality', breaks = 1:10) +
	scale_x_continuous(name = "Extent focal resource is limiting") +
	scale_color_discrete(
		guide = guide_legend(
			title = "Focal resource"
		)
	)

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

