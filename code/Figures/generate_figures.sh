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
Rscript macarthur_dimensionality_all_independent.R
	XXX add macarthur stuff to generate all following figures XXX
	Figures/macarthur_RIO.pdf
	Figures/macarthur_RIO_mats.pdf
	Figures/macarthur_IRO.pdf
	Figures/macarthur_IRO_mats.pdf
	Figures/macarthur_RRO.pdf
	Figures/macarthur_S10_resources.pdf
	Figures/macarthur_S10_resources_mats.pdf
	Figures/macarthur_S10_species_hierarchical.pdf
	Figures/macarthur_S10_species_hierarchical_mats.pdf
	Figures/macarthur_S10_species_hierarchical2.pdf
	Figures/macarthur_S10_species_hierarchical2_mats.pdf
	Figures/macarthur_S10_species_sequential.pdf
	Figures/macarthur_S10_species_sequential_mats.pdf

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
