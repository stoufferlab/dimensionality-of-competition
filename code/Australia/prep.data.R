
# read in Claire's data
wainwright <- read.csv("../../data/Australia/nhood data wTF.csv")

# relabel "focal" as "target"
cc <- colnames(wainwright)
cc[5] <- "target"
colnames(wainwright) <- cc

# these columns correspond to neighbor densities
competitors <- colnames(wainwright)[8:39]

# remove NA seeds
wainwright <- subset(wainwright, !is.na(seeds))

# subset to only the treatment of interest
wainwright <- subset(wainwright, light==which.treatment)

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

# remove unused variables
rm(wainwright, others, cc)
