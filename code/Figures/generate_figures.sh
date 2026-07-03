#!/bin/bash

###
# generate figures appearing in the main text
###

# main text figure 1
Rscript alpha_matrices_spain_control.R
# main text figure 2
Rscript response_effect_spain.R
# main text figure 3
Rscript dimensionality.R

###
# generate figures found in the supplementary notes
###

# supplementary note 2 (figures S1-S8)
Rscript macarthur_dimensionality_all_independent.R
Rscript macarthur_dimensionality_RIO.R
Rscript macarthur_dimensionality_IRO.R
Rscript macarthur_dimensionality_RRO.R
Rscript macarthur_dimensionality_S10_resources.R
Rscript macarthur_dimensionality_S10_species_hierarchical.R
Rscript macarthur_dimensionality_S10_species_hierarchical2.R
Rscript macarthur_dimensionality_S10_species_sequential.R

# supplementary note 4 (figures S9-S10)
Rscript rsquared_with_null.R
Rscript variance_explained_with_null.R

# supplementary note 5 (figures S11-S14)
Rscript alpha_matrices_obs_fit.R

# supplementary note 6 (figures S15-S16)
Rscript AIC_randomized.R
Rscript dimensionality_with_null.R

# supplementary note 7 (figures S17-S20)
Rscript response_effect_all.R

# supplementary note 8 (figure S21)
Rscript cohen_watkinson.R
