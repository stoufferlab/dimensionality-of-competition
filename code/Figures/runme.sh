#!/bin/bash

# figure 1
Rscript alpha_matrices_spain_treatment.R
# figure 2
Rscript dimensionality.R
# figure 3
Rscript response_effect_spain_treatment.R
# figure 4
#TODO Rscript control_treatment_alphas.R

# supps
# response effect traits for all other datasets
Rscript response_effect_all.R
# observed vs fit matrices for all datasets
Rscript alpha_matrices_obs_fit.R
# R^2 and variance per dimension for all datasets
#TODO Rscript AIC_raw_data.R
#TODO Rscript rsquared.R
#TODO Rscript variance_explained.R

Rscript AIC_randomized.R
# intrinsic species performance
#TODO Rscript control_treatment_fecundity.R
