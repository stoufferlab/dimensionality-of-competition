
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

# rename the data frame for some reason I cannot recall
goldberg2 <- goldberg
goldberg2[is.na(goldberg2)] <- 0

# standardize column names
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
