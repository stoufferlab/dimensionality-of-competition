
library(deSolve)

cohen_watkinson <- function(t, N, params){
	N_next <- with(params,
		N * ((1-germination)*survival + germination * fecundity/(1 + alpha %*% (germination * N)))
	)
	return(list(N_next))
}

# construct two rank-one interaction matrices
alphas <- list(
	wet = 1 * outer(c(2,1)*sqrt(1/5),c(1,1)*sqrt(1/2)),
	dry = 1 * outer(c(1,2)*sqrt(1/5),c(1,1)*sqrt(1/2))	
)

# and different fecundities
fecundities <- list(
	wet = c(15,15),
	dry = c(15,15)
)

# the plant species are otherwise ecologically equivalent
params <- list(
	germination = c(0.5,0.5),
	survival = c(0.8,0.8)
)

# initial condition for simulation in "dry" conditions
N0 <- c(A=50,B=50)

# "dry" parameters
params$alpha <- alphas[["dry"]]
params$fecundity <- fecundities[["dry"]]

# simulate for 50 years
pop_dyn_dry <- ode(
	N0,
	times = 1:50,
	func = cohen_watkinson,
	parms = params,
	method='iteration'
)

# initial condition for simulation in "wet" conditions
N0 <- c(A=50,B=50)

# "wet" parameters
params$alpha <- alphas[["wet"]]
params$fecundity <- fecundities[["wet"]]

# simulate for 50 years
pop_dyn_wet <- ode(
	N0,
	times = 1:50,
	func = cohen_watkinson,
	parms = params,
	method='iteration'
)

# alternate between "prolonged" wet and dry periods
N0 <- c(A=50,B=50)
years_per_interval <- 5
for(i in 1:10){
	if(i %% 2){
		params$alpha <- alphas[["dry"]]
		params$fecundity <- fecundities[["dry"]]
		interval_type <- "dry"
	}else{
		params$alpha <- alphas[["wet"]]
		params$fecundity <- fecundities[["wet"]]
		interval_type <- "wet"
	}
	if(i==1){
		pop_dyn <- ode(
			N0,
			seq((i-1)*years_per_interval,i*years_per_interval,1),
			cohen_watkinson,
			params,
			method='iteration'
		)
		pop_dyn_regular_fluctuations <- data.frame(pop_dyn, interval_type = interval_type)
	}else{
		Ninitial <- as.numeric(pop_dyn_regular_fluctuations[nrow(pop_dyn_regular_fluctuations),c("A","B")])
		names(Ninitial) <- c("A","B")
		pop_dyn <- ode(
			Ninitial,
			seq((i-1)*years_per_interval,i*years_per_interval,1),
			cohen_watkinson,
			params,
			method='iteration'
		)
		pop_dyn <- pop_dyn[2:nrow(pop_dyn),,drop=FALSE]
		pop_dyn <- data.frame(pop_dyn, interval_type = interval_type)
		pop_dyn_regular_fluctuations <- rbind(pop_dyn_regular_fluctuations, pop_dyn)
	}
}

# alternate randomly between wet and dry periods
set.seed(926541178)
N0 <- c(A=50,B=50)
years_per_interval <- 1
for(i in 1:50){
	if(runif(1) < 0.5){
		params$alpha <- alphas[["dry"]]
		params$fecundity <- fecundities[["dry"]]
		interval_type <- "dry"
	}else{
		params$alpha <- alphas[["wet"]]
		params$fecundity <- fecundities[["wet"]]
		interval_type <- "wet"
	}
	if(i==1){
		pop_dyn <- ode(
			N0,
			seq((i-1)*years_per_interval,i*years_per_interval,1),
			cohen_watkinson,
			params,
			method='iteration'
		)
		pop_dyn_irregular_fluctuations <- data.frame(pop_dyn, interval_type = interval_type)
	}else{
		Ninitial <- as.numeric(pop_dyn_irregular_fluctuations[nrow(pop_dyn_irregular_fluctuations),c("A","B")])
		names(Ninitial) <- c("A","B")
		pop_dyn <- ode(
			Ninitial,
			seq((i-1)*years_per_interval,i*years_per_interval,1),
			cohen_watkinson,
			params,
			method='iteration'
		)
		pop_dyn <- pop_dyn[2:nrow(pop_dyn),,drop=FALSE]
		pop_dyn <- data.frame(pop_dyn, interval_type = interval_type)
		pop_dyn_irregular_fluctuations <- rbind(pop_dyn_irregular_fluctuations, pop_dyn)
	}
}

# make a figure of the predicted population dynamics
setEPS(width=7, height=7)
postscript('../../manuscript/Figures/cohen_watkinson.eps')

layout(mat = matrix(
		c(1, 2, 3, 4), 
        nrow = 4, 
        ncol = 1
       ),
       heights = rep(2.4, 4),
       widths = rep(2, 4)
)

par(oma = c(3, 2.5, 0, 2.5))
par(mar = c(1, 4, 2.5, 1.5))

# define some sizes
cex.axis <- 1.5
padj <- 0 #.25

# prediction under only dry conditions
lapply(
	list(pop_dyn_dry,pop_dyn_wet,pop_dyn_regular_fluctuations,pop_dyn_irregular_fluctuations),
	function(pop_dyn){
		plot(
			pop_dyn[,"time"],
			pop_dyn[,"A"],
			type='n',
			ylim=c(0, 100),
			xlab="",
			ylab="",
			pch=21,
			axes=FALSE,
			xaxs='i',
			yaxs='i'
		)
		# if the type of interval varies
		if("interval_type" %in% colnames(pop_dyn)){
			for(i in 1:(nrow(pop_dyn)-1)){
				if(pop_dyn$interval_type[i] == 'wet'){
					polygon(c(i-1,i-2,i-2,i-1),c(0,0,400,400),col='grey87',lty=0)
				}
			}
		}
		# add the x axis
		axis(
			1,
			at=c(0,50),
			labels=FALSE,
			tcl=0,
			cex.axis=cex.axis,
			padj=padj,
			gap.axis=0
		)
		# add the y axis
		axis(
			2,
			at=seq(0,100,50),
			tcl=0,
			cex.axis=cex.axis,
			padj=padj,
			gap.axis=0
		)
		# add some tick marks
		rug(x = seq(5,45,10), ticksize=0.075, side=1)
		rug(x = seq(10,50,10), ticksize=0.15, side=1)
		rug(x = seq(25,75,25), ticksize=0.075, side=2)
		rug(x = seq(50,100,50), ticksize=0.15, side=2)
		# y axis label
		mtext("Population size", 2, outer=FALSE, line=3.25, xpd=NA, cex=1.3)
		# population dynamics
		A_col <- "#A50F15"
		B_col <- "#08519C"
		lines(pop_dyn[,"time"],pop_dyn[,"A"],lty=1,col=A_col)
		points(pop_dyn[,"time"],pop_dyn[,"A"],pch=21,col=A_col,bg=A_col,xpd=TRUE)
		lines(pop_dyn[,"time"],pop_dyn[,"B"],lty=1,col=B_col)
		points(pop_dyn[,"time"],pop_dyn[,"B"],pch=22,col=B_col,bg=B_col,xpd=TRUE)
	}
)
axis(
	1,
	at=seq(0,50,10),
	# labels=FALSE,
	tcl=0,
	cex.axis=cex.axis,
	padj=padj,
	gap.axis=0
)
mtext("Years", 1, outer=FALSE, line=2.75, xpd=NA, cex=1.3)

text(49.5,505,"Dry conditions", adj=1, xpd=NA, cex=2, srt=0)
text(49.5,375,"Wet conditions", adj=1, xpd=NA, cex=2, srt=0)
text(49.5,225,"Regular fluctuations", adj=1, xpd=NA, cex=2, srt=0)
text(49.5,90,"Irregular fluctuations", adj=1, xpd=NA, cex=2, srt=0)

dev.off()