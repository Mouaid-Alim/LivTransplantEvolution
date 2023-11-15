
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LivTransplantEvolution

<!-- badges: start -->

[![R-CMD-check](https://github.com/Mouaid-Alim/LivTransplantEvolution/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Mouaid-Alim/LivTransplantEvolution/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Description

The goal of `LivTransplantEvolution` is to an relevant biomarkers,
specifically those used in the MELD, MELD-na and MELD-3.0 risk score
calculations. It visualizes trajectories of those risk scores and
extracts the biomarkers most impactful of each longitudinal change.

- R Development Version: 4.1.2
- Development Platform: Mac

## Installation

You can install the development version of LivTransplantEvolution from
[GitHub](https://github.com/Mouaid-Alim/LivTransplantEvolution) with:

``` r
install.packages("devtools")
library("devtools")
devtools::install_github("Mouaid-Alim/LivTransplantEvolution", build_vignettes = TRUE)
library("LivTransplantEvolution")
```

## Overview

``` r
ls("package:LivTransplantEvolution")
data(package = "LivTransplantEvolution") # optional
browseVignettes("LivTransplantEvolution")
```

`LivTransplantEvolution` contains 6 functions. *prepareData* Prepares
and cleans up raw clinical serum blood test result results for a single
patient to extract Liver transplantation relevant clinical serum
biomarkers, specifically those used in the MELD score calculations.
*calcMELD* Will calculate the MELD score for a patient *calcMELDna* Will
calulcate the MELD-na score for a patient *calcMELDthree* Will calculate
the MELD-3.0 score for a patient *createMELDdf* Will create a dataframe
from the cleaned up data to contain longitudinal MELD scores values at
each timepoint *visualizeMELD* Creates a visual with all 3 MELD score
evolutions together.

Package visual result: ![](./inst/MELD_scores.png)

## Contributions

This Package is Created by Mouaid Alim

## Refernces

## Acknowledgements

This package was developed as part of an assessment for 2023 BCB410H:
Applied Bioinformatics course at the University of Toronto, Toronto,
CANADA. LivTransplantEvolution welcomes issues, enhancement requests,
and other contributions. To submit an issue, use the GitHub issues.
