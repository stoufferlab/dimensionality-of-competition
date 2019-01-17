
# read in Margie's data
datadir <- "../../data/Mayfield/"
mayfield <- read.csv(paste0(datadir, "nhood data wTF.csv"))

cc <- colnames(mayfield)
cc[5] <- "target"
colnames(mayfield) <- cc

competitors <- colnames(mayfield)[8:39]

# remove NA and 0 
# WARNING: ask Margie about zero fecundities
mayfield <- subset(mayfield, !is.na(seeds) & !(seeds==0))

# let's try just one treatment for simplicity in testing regime
mayfield <- subset(mayfield, light=="Open")

# we need to remove species that don't appear in this treatment
competitors <- names(which(colSums(mayfield[,competitors])>0))

# the data should now be primed for analysis; woohoo!

# run the fitting code
source('fit.machine.R')

# save the fits and write out a table of the AICs
Open.optim.lowD <- optim.lowD
save(Open.optim.lowD, file="Open.optim.lowD.Rdata")

Open.AICs <- cbind(0:length(optim.lowD), c(summary(null.fit)$aic, unlist(lapply(optim.lowD, function(x){x$aic}))))
write.table(Open.AICs, "../../data/Mayfield/mayfield.Open.AICs.csv", quote=FALSE, col.names=FALSE, sep=" ", row.names=FALSE)
