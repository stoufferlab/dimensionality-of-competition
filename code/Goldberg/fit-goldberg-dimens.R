
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

competitors <- paste0("sp.",1:7)

# the data should now be primed for analysis; woohoo!

# run the fitting code
source('fit.machine.R')

# save the fits and write out a table of the AICs
Goldberg.optim.lowD <- optim.lowD
save(Goldberg.optim.lowD, file="Goldberg.optim.lowD.Rdata")

Goldberg.AICs <- cbind(0:length(optim.lowD), c(summary(null.fit)$aic, unlist(lapply(optim.lowD, function(x){x$aic}))))
write.table(Goldberg.AICs, "../../data/Goldberg/goldberg.AICs.csv", quote=FALSE, col.names=FALSE, sep=" ", row.names=FALSE)