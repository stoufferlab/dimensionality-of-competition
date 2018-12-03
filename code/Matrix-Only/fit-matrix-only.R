
# data files are here
datadir <- "../../data/"

# matrix-only datasets are
datasets <- c(
	"Engel/engel-2008.csv",
	# "Goldberg/freckleton-2001.csv",
	"Johannsson/johannsson-1991.csv",
	"Levine/levine-2009-invasion-growth.csv",
	"Mitchley/mitchley-1986.csv",
	"Williams/williams-1962.csv",
	"Wilson/wilson-1986.csv"
)

par(mfrow=c(3,2))
for(dd in datasets){
	# d <- read.csv(paste0(datadir,dd))
	# colnames(d)[1] <- "target"
	# d <- reshape2::melt(d, id.vars = "target", variable.name = "competitor")

	# read in the data
	d <- read.csv(paste0(datadir, dd), row.names=1)

	# normalize by row means (as a faux intrinsic growth)
	d <- d / rowMeans(d)

	d <- log(d)

	# perform an svd
	SSS <- svd(d)

	# singular value like plot
	# svals <- SSS$d
	# plot(0:length(svals), c(0,cumsum(svals**2) / sum(svals**2))) #, ylim=c(0.7,1))

	# pseudo rsq like plot
	nll <- sum((d - 1)^2)
	rsq <- sapply(
		1:length(SSS$d),
		function(x,d,SSS){
			if(x == 1){
				dd <- SSS$u[,1,drop=FALSE] %*% SSS$d[1] %*% t(SSS$v[,1,drop=FALSE])
			}else{
				dd <- SSS$u[,1:x,drop=FALSE] %*% diag(SSS$d[1:x]) %*% t(SSS$v[,1:x,drop=FALSE])
			}
			return(sum((d - dd)^2))
		},
		d=d,
		SSS=SSS
	)
	rsq <- c(0, 1 - rsq / nll)
	plot(0:length(SSS$d), rsq, ylim=c(0.0,1))
	abline(h=0.90, lty=3, col='red')

	kkk <- data.frame(d=0:length(SSS$d), rsq=rsq)
	write.table(
		kkk,
		paste0(datadir, strsplit(dd,'/')[[1]][1], "/pseudo-rsquared.csv"),
		quote=FALSE,
		col.names=FALSE,
		sep=" ",
		row.names=FALSE
	)

	# break
}
