
# read in Margie's data
datadir <- "../../data/Mayfield/"
mayfield <- read.csv(paste0(datadir, "nhood data wTF.csv"))

# relabel "focal" as "target"
cc <- colnames(mayfield)
cc[5] <- "target"
colnames(mayfield) <- cc

# these columns correspond to neighbor densities
competitors <- colnames(mayfield)[8:39]

# remove NA and 0 
# WARNING: ask Margie about zero fecundities
mayfield <- subset(mayfield, !is.na(seeds) & !(seeds==0))

# let's try just one treatment for simplicity in testing regime
mayfield <- subset(mayfield, light=="Shade")

# the data should now be primed for analysis; woohoo!

# run the model comparison code
source('model.comparison.R')

# we need to remove species that don't appear in this treatment when we fit the low-dimensional versions
competitors <- names(which(colSums(mayfield[,competitors])>0))

# run the dimensionality fitting code
source('fit.machine.R')

# save the fits and write out a table of the AICs
Shade.optim.lowD <- optim.lowD
save(Shade.optim.lowD, file="../../results/Mayfield/Shade.optim.lowD.Rdata", ascii=TRUE)

Shade.AICs <- cbind(0:length(optim.lowD), c(summary(null.fit)$aic, unlist(lapply(optim.lowD, function(x){x$aic}))))
write.table(Shade.AICs, "../../results/Mayfield/mayfield.Shade.AICs.csv", quote=FALSE, col.names=FALSE, sep=" ", row.names=FALSE)
