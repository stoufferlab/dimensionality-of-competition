
# read in the synthetic data
fecundity.data <- read.csv(paste0("../../data/Synthetic-Datasets/synthetic_dataset_",which.dataset,".csv"),row.names=1)

# make sure that the target is a factor
fecundity.data$target <- as.factor(fecundity.data$target)

# we need a variable called targets for things to work
targets <- levels(fecundity.data$target)

# we need a variable called competitors for things to work
competitors <- paste0("sp.",1:7)

# we need to know what column has the response variable
fecundity <- "biomass"
