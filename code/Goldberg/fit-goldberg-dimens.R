
# read in the Goldberg data
datadir <- "../../data/Goldberg/Figure2"

# convert the data to the "standard format"
goldberg <- data.frame()
for(focal in 1:7){
	for(neigh in 1:7){
		fn <- as.data.frame(read.csv(paste0(datadir,"/","figure2_",focal,neigh,".csv")))
		colnames(fn) <- c(paste0("sp.",neigh),"biomass.inv")
		fn$focal <- focal
		goldberg <- plyr::rbind.fill(goldberg,fn)
	}
}
goldberg <- goldberg[,c("biomass.inv","focal",paste0("sp.",1:7))]
goldberg$biomass <- 1 / goldberg$biomass.inv

# focal column is a factor
goldberg$focal <- as.factor(goldberg$focal)

# par(mfrow=c(7,7))
# for(i in 1:7){
# 	for(j in 1:7){
# 		d = subset(goldberg, focal==i)
# 		plot(d$biomass.inv ~ d[,paste0("sp.",j)], xlim=c(0, 450), ylim=c(0, 20))
# 	}
# }

# slopes <- paste0("focal:",paste0("sp.",1:7))

goldberg2 <- goldberg
goldberg2[is.na(goldberg2)] <- 0

# mf <- as.formula(paste0("biomass.inv ~ 0 + focal + ",paste0(slopes, collapse=" + ")))

# m1 <- glm(mf, data=goldberg2)

# mf <- as.formula(paste0("biomass ~ 0 + focal + ",paste0(slopes, collapse=" + ")))

# m3 <- glm(mf, data=goldberg2, family=Gamma())

# m4 <- glm(mf, data=goldberg2, family=gaussian(link=log))

# mf <- as.formula(paste0("biomass ~ 0 + ",paste0(slopes, collapse=" + ")))

# m5 <- glm(mf, data=goldberg2, offset=rep(1,nrow(goldberg)), family=Gamma())

cc <- colnames(goldberg2)
cc[2] <- "target"
colnames(goldberg2) <- cc

# rename the core data frame
fecundity.data <- goldberg2

# we need a variable called targets for things to work
targets <- levels(fecundity.data$target)

# we need a variable called competitors for things to work
competitors <- paste0("sp.",1:7)

# we need to know what column has the fecundities
fecundity <- "biomass"

# specify the model family to fit
which.family <- Gamma()

# number of random starts for optimization at each dimension
n.random <- 25

# the data should now be primed for analysis; woohoo!

# run the fitting code
source('../Mayfield/fit.machine.R')

# save the fits
Goldberg.optim.lowD <- optim.lowD
save(Goldberg.optim.lowD, file="../../results/Goldberg/Goldberg.optim.lowD.Rdata", ascii = TRUE)

# write out a table of the AICs
Goldberg.AICs <- cbind(0:length(optim.lowD), c(summary(null.fit)$aic, unlist(lapply(optim.lowD, function(x){x$aic}))))
write.table(Goldberg.AICs, "../../results/Goldberg/goldberg.AICs.csv", quote=FALSE, col.names=FALSE, sep=" ", row.names=FALSE)
