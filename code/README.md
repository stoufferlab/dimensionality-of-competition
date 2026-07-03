# Structure of the code

The code is structured in the following fashion.

## Utils

The `Utils` directory contains the internal functions used across different scripts for the different analyses, in particular for the fitting of the low-dimensional interaction model to _Raw_ empirical data.

## Spain

The `Spain` directory contains code associated with the analysis of Dataset 1 (Wet and Dry) from the manuscript. These data derive from the paper:

Matı́as, L., Godoy, O., Gómez-Aparicio, L. & Pérez-Ramos, I. M. "An experimental extreme drought reduces the likelihood of species to coexist despite increasing intransitivity in competitive networks."
_J. Ecol._ *106*, 826–837 (2018). [https://doi.org/10.1111/1365-2745.12962](https://doi.org/10.1111/1365-2745.12962)

and the data can be found [here](https://doi.org/10.5061/dryad.5d1s9).

## Australia

The `Australia` directory contains code associated with the analysis of Dataset 2 (Sun and Shade) from the manuscript. These data derive from the paper:

Wainwright, C. E., HilleRisLambers, J., Lai, H. R., Loy, X. & Mayfield, M. M. "Distinct responses of niche and fitness differences to water availability underlie variable coexistence outcomes in semi-arid annual plant communities." _J. Ecol._ *107*, 293–306 (2019). [https://doi.org/10.1111/1365-2745.13056](https://doi.org/10.1111/1365-2745.13056)

and the data can be found [here](https://doi.org/10.5061/dryad.8v13t2q).

## Kinlock

The `Kinlock` directory contains code associated with the analysis of Datasets 3–12. These data are a subset of the data from the paper:

Kinlock, N. L. "A meta-analysis of plant interaction networks reveals competitive hierarchies as well as facilitation and intransitivity." _Am. Nat._ *194*, 640–653 (2019). [https://doi.org/10.1086/705293](https://doi.org/10.1086/705293)

and the data can be found [here](https://doi.org/10.5061/dryad.1sm06sp).

## Synthetic-Datasets

The `Synthetic-Datasets` directory contains code used to generate synthetic datasets and the analysis thereof. Further details regarding these data can be found in the Supporting Notes.

## Figures

The `Figures` directory contains code with which to generate all figures in the manuscript and supporting notes. This is all automated within the bash script `Figures/generate_figures.sh`.

## Tables

The `Tables` directory contains code with which to generate all Tables in the supporting notes. This is all automated within the bash script `Tables/generate_tables.sh`.
