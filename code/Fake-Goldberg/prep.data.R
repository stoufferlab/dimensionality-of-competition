
# read in the Goldberg data
datadir <- "../../data/Fake-Goldberg/"

# rename the core data frame
fecundity.data <- read.csv(paste0(datadir,"fake_goldberg_",which.fake.data,".csv"),row.names=1)

fecundity.data$target <- as.factor(fecundity.data$target)

# we need a variable called targets for things to work
targets <- levels(fecundity.data$target)

# we need a variable called competitors for things to work
competitors <- paste0("sp.",1:7)

# we need to know what column has the fecundities
fecundity <- "biomass"
