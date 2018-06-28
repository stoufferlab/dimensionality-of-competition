
# read in Margie's data
datadir <- "../../data/Mayfield/"
mayfield <- read.csv(paste0(datadir, "nhood data wTF.csv"))

cc <- colnames(mayfield)
cc[5] <- "target"
colnames(mayfield) <- cc

competitors <- colnames(mayfield)[9:39]

# remove NA and 0 
# WARNING: ask Margie about zero fecundities
mayfield <- subset(mayfield, !is.na(seeds) & !(seeds==0))

# the data should now be primed for analysis; woohoo!

# run the fitting code
source('fit.machine.R')

# save the fits and write out a table of the AICs
all.optim.lowD <- optim.lowD
save(all.optim.lowD, file="all.optim.lowD.Rdata")

all.AICs <- cbind(0:length(optim.lowD), c(summary(null.fit)$aic, unlist(lapply(optim.lowD, function(x){x$aic}))))
write.table(all.AICs, "../../data/Mayfield/mayfield.all.AICs.csv", quote=FALSE, col.names=FALSE, sep=" ", row.names=FALSE)