
library(ecodist)
library(tidyverse)

# read in relevant functions
source('lib/macarthur_functions.R')

# number of consumers
S <- 2

# number of resources
d <- 2

# number of randomisations
n_rand <- 1E6
n_chunk <- 25

# setEPS()
pdf('../../manuscript/Supplementary/Figures/macarthur_independent.pdf',width=10.64, height=3.77)

# three column plot
par(mfrow=c(1,3))
par(mar = c(2, 2.5, 2, 2.5), oma = c(3.5, 4.5, 0.5, 0.5))
cex.axis <- 1.0
padj <- 0.25

# only variation in parameters governing resource dynamics (K / rho vector)
res <- vector("list", n_rand)
for(i in 1:n_rand){
	cr_parms <- random_cr_model(S, d, w_var = FALSE, c_var = FALSE, l_var = TRUE)

	var_dat <- estimate_cr_variation(cr_parms)

	res[[i]] <- var_dat
}

# extract out the information we want for plotting
plot_data <- do.call(
	rbind,
	lapply(
		res,
		function(x){
			return(c(x = x$l_var, y = x$d_var_exp[1]))
		}
	)
)

plot_data <-
	plot_data %>%
	as_tibble() %>%
    dplyr::arrange(x) %>%
    mutate(x_grp = as.integer(cut(x, breaks = seq(0,1,length.out=n_chunk)))) %>%
    group_by(x_grp) %>%
    summarise(
    	x = median(x),
    	y_min = quantile(y, probs=c(0.025)),
    	y_max = quantile(y, probs=c(0.975)),
    	y = mean(y)
    )

# generate the first plot
plot(
	plot_data$x,
	plot_data$y,
	ylim=c(0.5,1),
	xlim=c(0,1.0),
	axes=FALSE
)

mtext('Variance explained by first niche dimension', 2, line=2.8, outer=FALSE, cex=1.)
mtext('Variation in resource dynamics parameters', 1, outer=FALSE, line=2.8, cex=1.)

points(plot_data$x,plot_data$y,col='black',pch=21,bg='black')

abline(h=0.95,lty=2,lwd=1.5,col='red')
axis(
	4,
	c(0.5,1),
	labels = c(
		expression(italic(hat(d))*"=2"),
		expression(italic(hat(d))*"=1")
	),
	cex.axis=cex.axis
)
	axis(
		1,
		# at=-1:(nrow(d)+2),
		tcl=-0.5,
		cex.axis=cex.axis,
		padj=padj
	)
	axis(
		2,
		# at=seq(0,1,0.2),
		tcl=-0.5,
		cex.axis=cex.axis,
		# padj=padj,
		las=1
	)

title(expression("Random variation in only "*italic(theta)), cex.main = 2)

# only variation in resource value parameters (w matrix)
res <- vector("list", n_rand)
for(i in 1:n_rand){
	cr_parms <- random_cr_model(S, d, w_var = TRUE, c_var = FALSE, l_var = FALSE)

	var_dat <- estimate_cr_variation(cr_parms)

	res[[i]] <- var_dat
}

# extract out the information we want for plotting
plot_data <- do.call(
	rbind,
	lapply(
		res,
		function(x){
			return(c(x = x$w_var, y = x$d_var_exp[1]))
		}
	)
)

plot_data <-
	plot_data %>%
	as_tibble() %>%
    dplyr::arrange(x) %>%
    mutate(x_grp = as.integer(cut(x, breaks = seq(0,1,length.out=n_chunk)))) %>%
    group_by(x_grp) %>%
    summarise(
    	x = median(x),
    	y_min = quantile(y, probs=c(0.025)),
    	y_max = quantile(y, probs=c(0.975)),
    	y = mean(y)
    )

# generate the second plot
plot(
	plot_data$x,
	plot_data$y,
	ylim=c(0.5,1),
	xlim=c(0,1.0),
	axes=FALSE
)

mtext('Variation in resource value parameters', 1, outer=FALSE, line=2.8, cex=1.)

points(plot_data$x,plot_data$y,col='black',pch=21,bg='black')

abline(h=0.95,lty=2,lwd=1.5,col='red')
axis(
	4,
	c(0.5,1),
	labels = c(
		expression(italic(hat(d))*"=2"),
		expression(italic(hat(d))*"=1")
	),
	cex.axis=cex.axis
)
axis(
	1,
	# at=-1:(nrow(d)+2),
	tcl=-0.5,
	cex.axis=cex.axis,
	padj=padj
)
axis(
	2,
	# at=seq(0,1,0.2),
	tcl=-0.5,
	cex.axis=cex.axis,
	# padj=padj,
	las=1
)

title(expression("Random variation in only "*italic(W)), cex.main = 2)

# only variation in resource utilization parameters (c matrix)
res <- vector("list", n_rand)
for(i in 1:n_rand){
	cr_parms <- random_cr_model(S, d, w_var = FALSE, c_var = TRUE, l_var = FALSE)

	var_dat <- estimate_cr_variation(cr_parms)

	res[[i]] <- var_dat
}

# extract out the information we want for plotting
plot_data <- do.call(
	rbind,
	lapply(
		res,
		function(x){
			return(c(x = x$c_var, y = x$d_var_exp[1]))
		}
	)
)

# generate the third plot
plot_data <-
	plot_data %>%
	as_tibble() %>%
    dplyr::arrange(x) %>%
    mutate(x_grp = as.integer(cut(x, breaks = seq(0,1,length.out=n_chunk)))) %>%
    group_by(x_grp) %>%
    summarise(
    	x = median(x),
    	y_min = quantile(y, probs=c(0.025)),
    	y_max = quantile(y, probs=c(0.975)),
    	y = mean(y),
    	n = n()
    )

# generate the third plot
plot(
	plot_data$x,
	plot_data$y,
	ylim=c(0.5,1),
	xlim=c(0,1),
	axes=FALSE
)

mtext('Variation in resource utilization parameters', 1, outer=FALSE, line=2.8, cex=1.)

# polygon(
# 	x = c(plot_data$x,rev(plot_data$x)),
# 	# y = c(plot_data$y_min,rev(plot_data$y_max)),
# 	y=c(plot_data$y_max, rev(plot_data$y_min)),
# 	border = NA,
# 	col = 'grey'
# )

points(plot_data$x,plot_data$y,col='black',pch=21,bg='black')

abline(h=0.95,lty=2,lwd=1.5,col='red')
axis(
	4,
	c(0.5,1),
	labels = c(
		expression(italic(hat(d))*"=2"),
		expression(italic(hat(d))*"=1")
	),
	cex.axis=cex.axis
)
axis(
	1,
	# at=seq(0,1,15),
	tcl=-0.5,
	cex.axis=cex.axis,
	padj=padj
)
axis(
	2,
	# at=seq(0,1,0.2),
	tcl=-0.5,
	cex.axis=cex.axis,
	# padj=padj,
	las=1
)

title(expression("Random variation in only "*italic(C)), cex.main = 2)

dev.off()
