
library(ecodist)
library(tidyverse)
# library(zoo)

# read in relevant functions
source('lib/macarthur_functions.R')

# number of consumers
S <- 2

# number of resources
d <- 2

# number of randomisations
n_rand <- 1E6
n_chunk <- 25

# what type of variation do we impose?
title_exp <- expression("Random "*italic(theta)*"; Random "*italic(W)) #*"; Orthogonal "*italic(C))

res <- vector("list", n_rand)
for(i in 1:n_rand){
	cr_parms <- random_cr_model(S, d, w_var = TRUE, c_var = FALSE, l_var = TRUE)

	# force C to be orthogonal
	# cr_parms$c <- diag(S)

	cr_parms$c <- matrix(c(0.95,0.05,0.05,0.95),2,2)

	var_dat <- estimate_cr_variation(cr_parms)

	res[[i]] <- var_dat
}

# extract out the information we want for plotting
plot_data <- do.call(
	rbind,
	lapply(
		res,
		function(x){
			return(c(xl = x$l_var, xw = x$w_var, xc = x$c_var, y = x$d_var_exp[1]))
		}
	)
) %>% as_tibble

# grid_data <-
# 	plot_data %>%
#     mutate(l_grp = as.integer(cut(xl, breaks = seq(0,1,length.out=n_chunk)))) %>%
#     mutate(w_grp = as.integer(cut(xw, breaks = seq(0,1,length.out=n_chunk)))) %>%
#     group_by(l_grp,w_grp) %>%
#     summarise(
#     	l_min = seq(0,1,length.out=n_chunk)[unique(l_grp)],
#     	l_max = seq(0,1,length.out=n_chunk)[unique(l_grp)+1],
#     	w_min = seq(0,1,length.out=n_chunk)[unique(w_grp)],
#     	w_max = seq(0,1,length.out=n_chunk)[unique(w_grp)+1],
#     	# y_min = quantile(y, probs=c(0.025)),
#     	# y_max = quantile(y, probs=c(0.975)),
#     	y = sum(y>0.95) / length(y), #mean(y),
#     	n = n(),
#     	.groups = 'keep'
#     )

# pp <-
# 	grid_data %>%
# 	ggplot() +
# 	geom_rect(aes(xmin = l_min, xmax = l_max, ymin = w_min, ymax = w_max, fill = y))

# print(pp)

# stop()

# plot stuff
pdf(
	'../../manuscript/Supplementary/Figures/macarthur_RRO.pdf',
	width=7.90,
	height=3.77
)

# three column plot
par(mfrow=c(1,2))
par(mar = c(2, 2.5, 2, 2.5), oma = c(3.5, 4.5, 0.5, 0.5))
cex.axis <- 1.0
padj <- 0.25


# generate the first plot

# extract out the information we want for plotting
this_plot_data <-
	plot_data %>%
	mutate(x = xl) %>%
    dplyr::arrange(x) %>%
	mutate(x_grp = as.integer(cut(x, breaks = seq(0,1,length.out=n_chunk)))) %>%
    group_by(x_grp) %>%
    summarise(
    	x = median(x),
    	y_min = quantile(y, probs=c(0.025)),
    	y_max = quantile(y, probs=c(0.975)),
    	y = sum(y>0.95) / length(y) #mean(y)
    )

plot(
	this_plot_data$x,
	this_plot_data$y,
	ylim=c(0,1),
	xlim=c(0,1),
	axes=FALSE
)

mtext(expression('Proportion of '*italic(hat(d))*" = 1 interaction matrices"), 2, line=2.8, outer=FALSE, cex=1.)
mtext('Variation in resource dynamics parameters', 1, outer=FALSE, line=2.8, cex=1.)

# polygon(
# 	x = c(plot_data$x,rev(plot_data$x)),
# 	y=c(plot_data$y_max, rev(plot_data$y_min)),
# 	border = NA,
# 	col = 'grey'
# )

points(this_plot_data$x,this_plot_data$y,col='black',pch=21,bg='black')

# abline(h=0.95,lty=2,lwd=1.5,col='red')
axis(
	4,
	c(0,1),
	labels = c(
		expression(italic(hat(d))*" = 2"),
		expression(italic(hat(d))*" = 1")
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

# extract out the information we want for plotting
this_plot_data <-
	plot_data %>%
	mutate(x = xw) %>%
    dplyr::arrange(x) %>%
	mutate(x_grp = as.integer(cut(x, breaks = seq(0,1,length.out=n_chunk)))) %>%
    group_by(x_grp) %>%
    summarise(
    	x = median(x),
    	y_min = quantile(y, probs=c(0.025)),
    	y_max = quantile(y, probs=c(0.975)),
    	y = sum(y>0.95) / length(y) #mean(y)
    )

# generate the second plot
plot(
	this_plot_data$x,
	this_plot_data$y,
	ylim=c(0,1),
	xlim=c(0,1),
	axes=FALSE
)

mtext('Variation in resource value parameters', 1, outer=FALSE, line=2.8, cex=1.)

mtext(title_exp, 3, outer=TRUE, line=-1.5) #, outer=TRUE, cex=2, xpd=TRUE)

# polygon(
# 	x = c(plot_data$x,rev(plot_data$x)),
# 	y=c(plot_data$y_max, rev(plot_data$y_min)),
# 	border = NA,
# 	col = 'grey'
# )

points(this_plot_data$x,this_plot_data$y,col='black',pch=21,bg='black')

# abline(h=0.95,lty=2,lwd=1.5,col='red')
axis(
	4,
	c(0,1),
	labels = c(
		expression(italic(hat(d))*" = 2"),
		expression(italic(hat(d))*" = 1")
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


# # extract out the information we want for plotting
# plot_data <- do.call(
# 	rbind,
# 	lapply(
# 		res,
# 		function(x){
# 			return(c(x = x$c_var, y = x$d_var_exp[1]))
# 		}
# 	)
# )

# plot_data <-
# 	plot_data %>%
# 	as_tibble() %>%
#     dplyr::arrange(x) %>%
#     mutate(x_grp = 1) %>%
#     group_by(x_grp) %>%
#     summarise(
#     	x = median(x),
#     	y_min = quantile(y, probs=c(0.025)),
#     	y_max = quantile(y, probs=c(0.975)),
#     	y = mean(y)
#     )

# # generate the third plot
# plot(
# 	plot_data$x,
# 	plot_data$y,
# 	ylim=c(0.5,1),
# 	xlim=c(0,90),
# 	axes=FALSE
# )

# mtext('Variation in resource utilization parameters', 1, outer=FALSE, line=2.8, cex=1.)

# polygon(
# 	x = c(plot_data$x,rev(plot_data$x)),
# 	# y = c(plot_data$y_min,rev(plot_data$y_max)),
# 	y=c(plot_data$y_max, rep(0.5,nrow(plot_data))),
# 	border = NA,
# 	col = 'grey'
# )
# polygon(
# 	x = c(plot_data$x,rev(plot_data$x)),
# 	# y = c(plot_data$y_min,rev(plot_data$y_max)),
# 	y=c(plot_data$y_min, rep(0.5,nrow(plot_data))),
# 	border = NA,
# 	col = 'white'
# )

# points(plot_data$x,plot_data$y,col='black')

# abline(h=0.95,lty=2,lwd=1.5,col='red')
# axis(
# 	4,
# 	c(0.5,1),
# 	labels = c(
# 		expression(italic(d)*"=2"),
# 		expression(italic(d)*"=1")
# 	),
# 	cex.axis=cex.axis
# )
# axis(
# 	1,
# 	at=seq(0,90,15),
# 	tcl=-0.5,
# 	cex.axis=cex.axis,
# 	padj=padj
# )
# axis(
# 	2,
# 	# at=seq(0,1,0.2),
# 	tcl=-0.5,
# 	cex.axis=cex.axis,
# 	# padj=padj,
# 	las=1
# )

dev.off()
