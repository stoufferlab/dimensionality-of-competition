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

# supporting note 2 (figures S1-S8)
XXX add macarthur stuff XXX

# supporting note 4 (figures S9-S10)
Rscript rsquared_with_null.R
Rscript variance_explained_with_null.R

# supporting note 5 (figures S11-S14)
Rscript alpha_matrices_obs_fit.R

# supporting note 6 (figures S15-S16)
Rscript AIC_randomized.R
Rscript dimensionality_with_null.R

# supporting note 7 (figures S17-S20)
Rscript response_effect_all.R

# supporting note 8 (figure S21)
Rscript cohen_watkinson.R
