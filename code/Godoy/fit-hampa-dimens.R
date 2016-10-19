
# load in house competition library
library(competition)

# read in the data set and put it in a standar
source('prep-data.R')

# attempt to identify the best fit lowD models
source('fit-lowD-traits.R')

# attempt to bootstrap the lowD traits
source('boot-lowD-traits.R')
