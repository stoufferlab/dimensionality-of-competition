
# read in Claire's data
datadir <- "../../data/Wainwright/"
wainwright <- read.csv(paste0(datadir, "nhood data wTF.csv"))

# relabel "focal" as "target"
cc <- colnames(wainwright)
cc[5] <- "target"
colnames(wainwright) <- cc

# these columns correspond to neighbor densities
competitors <- colnames(wainwright)[8:39]

# remove NA and 0 
# WARNING: ask Margie about zero fecundities
wainwright <- subset(wainwright, !is.na(seeds) & !(seeds==0))

# let's try just one treatment for simplicity in testing regime
wainwright <- subset(wainwright, light==which.treatment)

###########
# the data should largely be primed for analysis; woohoo!
###########

# we need to remove species that don't appear in this treatment when we fit the low-dimensional versions
competitors <- names(which(colSums(wainwright[,competitors])>0))

# we need a variable called targets for things to work
targets <- levels(wainwright$target)

# and collapse all non-focal competitors into one "Other" category
others <- competitors[!competitors %in% targets]

# create an Other column
wainwright$Other <- rowSums(wainwright[,others])

# and remove the members of the Other group
wainwright[,others] <- NULL

# create a new list of competitors
competitors <- c(targets, "Other")

# we need to know what column has the fecundities
fecundity <- "seeds"

# rename the core data frame
fecundity.data <- wainwright
