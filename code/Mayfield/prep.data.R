
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
mayfield <- subset(mayfield, light==which.treatment)

###########
# the data should largely be primed for analysis; woohoo!
###########

# we need to remove species that don't appear in this treatment when we fit the low-dimensional versions
competitors <- names(which(colSums(mayfield[,competitors])>0))

# we need a variable called targets for things to work
targets <- levels(mayfield$target)

# we need to know what column has the fecundities
fecundity <- "seeds"

# rename the core data frame
fecundity.data <- mayfield
