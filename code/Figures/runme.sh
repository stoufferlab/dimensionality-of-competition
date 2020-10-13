#!/bin/bash

# figure 1
Rscript alpha_matrices_godoy_T.R
# figure 2
Rscript dimensionality.R
# figure 3
Rscript response_effect_godoy_T.R
# figure 4
Rscript control_treatment_godoy.R

# supps
# response effect traits for all other datasets
Rscript response_effect_others.R
# observed vs fit matrices for all datasets
Rscript alpha_matrices_obs_fit.R
# R^2 and variance per dimension for all datasets
Rscript rsquared.R
Rscript variance_explained.R

# testing
Rscript control_treatment_wainwright.R
