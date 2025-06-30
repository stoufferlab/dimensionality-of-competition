
library(plot.matrix)
library(RColorBrewer)
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

pdf(
	'../../manuscript/Supplementary/Figures/macarthur_RIO.pdf',
	width=4.45,
	height=3.77
)

par(mfrow=c(1,1))
par(mar = c(2, 2.5, 2, 2.5), oma = c(3.5, 4.5, 0.5, 0.5))
cex.axis <- 1.0
padj <- 0.25

# what type of variation do we impose?
title_exp <- expression("Random "*italic(theta)*"; Identical "*italic(W)) #*"; Orthogonal "*italic(C))

res <- vector("list", n_rand)
for(i in 1:n_rand){
	cr_parms <- random_cr_model(S, d, w_var = FALSE, c_var = FALSE, l_var = TRUE)
	cr_parms$c <- matrix(c(0.95,0.05,0.05,0.95),2,2)

	var_dat <- estimate_cr_variation(cr_parms)

	res[[i]] <- var_dat
	res[[i]]$cr_parms <- cr_parms
	res[[i]]$A <- cr_model_A_matrix(cr_parms)
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
	mutate(idx = row_number()) %>%
    dplyr::arrange(x) %>%
    mutate(x_grp = as.integer(cut(x, breaks = seq(0,1,length.out=n_chunk)))) %>%
    group_by(x_grp) %>%
    summarise(
    	x = median(x),
    	which_y_min = idx[which.min(y)],
    	which_y_max = idx[which.max(y)],
    	rand_y = sample(idx,1),
    	y_min = quantile(y, probs=c(0.025)),
    	y_max = quantile(y, probs=c(0.975)),
    	y = sum(y>0.95) / length(y)

    )

# par(mar = c(5, 6, 4.5, 1.0), oma = c(0, 0, 0, 0.75))
res.list <- c(
	subset(plot_data, x_grp == 1)$which_y_min,
	# subset(plot_data, x_grp == 1)$which_y_max,
	subset(plot_data, x_grp == floor(n_chunk/2+1))$which_y_min,
	subset(plot_data, x_grp == floor(n_chunk/2+1))$which_y_max,
	subset(plot_data, x_grp == n_chunk - 1)$which_y_min
	# subset(plot_data, x_grp == n_chunk - 1)$which_y_max
)

# generate the first plot
plot(
	plot_data$x,
	plot_data$y,
	ylim=c(0,1),
	xlim=c(0,1.0),
	axes=FALSE,
	xlab = '',
	ylab = ''
)

mtext(expression('Proportion of '*italic(hat(d))*" = 1 interaction matrices"), 2, line=2.8, outer=FALSE, cex=1.)
mtext('Variation in resource dynamics parameters', 1, outer=FALSE, line=2.8, cex=1.)

mtext(title_exp, 3, line=0.5) #, outer=TRUE, cex=2, xpd=TRUE)

points(plot_data$x,plot_data$y,col='black',pch=21,bg='black')

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

sapply(
	seq_along(res.list),
	function(i,res.list,res){
		text(
			res[[res.list[i]]]$l_var,
			ifelse(res[[res.list[i]]]$d_var_exp[1]>0.95, 1.0, 0.0),
			letters[i],
			cex = 1.5,
			xpd = NA
		)
	},
	res = res,
	res.list = res.list
)

dev.off()

pdf(
	'../../manuscript/Supplementary/Figures/macarthur_RIO_mats.pdf',
	width=6.45,
	height=6.45
)

layout(mat = cbind(
		matrix(1:12,4,3),
		13
       ),
       widths = c(1.5, 1.5, 1.5, 0.5),
       heights = 1
)

par(mar = c(1.5, 1.5, 1.5, 1), oma = c(0.75, 1.75, 0, 1.75))

A.list <- lapply(
	res[res.list],
	function(x){ x$A$A }
)
A1.list <- lapply(
	A.list,
	function(A){
		S <- svd(A)
		S$u[,1,drop=FALSE] %*% diag(x=S$d[1],1,1) %*% t(S$v[,1,drop=FALSE])
	}
)
A2.list <- lapply(
	A.list,
	function(A){
		S <- svd(A)
		S$u[,2,drop=FALSE] %*% diag(x=S$d[2],1,1) %*% t(S$v[,2,drop=FALSE])
	}
)

# limits of decomposed CR alphas
fmin <- -0.5
fmax <- 0.5

pal <- c(
	rev(colorRampPalette(brewer.pal(9, "Blues"))(1000*abs(fmin)/(fmax - fmin))),
	(colorRampPalette(brewer.pal(9, "Reds"))(1000*abs(fmax)/(fmax - fmin)))
)

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
		text(0, 2.25, letters[i], xpd=NA, cex=2.5)
		text(3, 1.5, "=", xpd=NA, cex=4.5)
	},
	A.list = A.list
)

# plot the first dimension
lapply(
	A1.list,
	function(A){
		plot(
			A,
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
		text(3, 1.5, "+", xpd=NA, cex=4.5)
	}
)

# plot the second dimension
lapply(
	A2.list,
	function(A){
		plot(
			A,
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
	}
)


# plot the color bar
par(mar = c(3, 0.5, 3, 1.5))
colorbarr <- t(matrix(seq(fmin, fmax, length.out=length(pal)), length(pal), 1))
image(
	colorbarr,
	col=pal,
	xaxt='n',
	yaxt='n',
	mgp=c(0.5,0.5,0.5)
)
text(2.25, 0.5, "Interaction strength", xpd=NA, cex=3.5, srt=270)
mtext(paste0("+0.5"), 1, outer=FALSE, line=1.3, xpd=NA, cex=1.5, adj=1)
mtext(paste0("-0.5"), 3, outer=FALSE, line=0.5, xpd=NA, cex=1.5, adj=1)

dev.off()
