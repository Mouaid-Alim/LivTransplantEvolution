
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LivTransplantEvolution

<!-- badges: start -->
<!-- badges: end -->

## Description

End-Stage Liver Disease (ESLD) is a terminal diagnosis that encompasses
advanced liver disease, liver failure, and decompensated cirrhosis.
Liver transplantation, either from living or deceased donors, remains
the only life saving therapeutic intervention available. Transplanted
organs can be Living Donor Liver transplantation (LDLT) or Deceased
Donor Liver transplantation (DDLT). DDLT is the major form of
transplantation in Canada accounting for 78% of Liver Transplants (LT)
in 2021. Various allocaiton systems exist but the main one is the Model
for End-stage Liver Disease (MELD) score, a linear model that uses the
log of the Serum Creatinine, International Normalized Ratio (INR) and
Bilirubin. It is the the adopted method of severity calculation in the
united states and Canada. MELD-na is an augmentation to the MELD score
calculation that takes into account Serum Sodium and MELD 3.0 is an
improvement on the MELD score that takes into account Albumin among
other factors.

The Scientific Registry of Transplant Recipients (SRTR) database is the
database containing information largely collected by the Organ
Procurement and Transplantation Network (OPTN) in the United States.
This very Large database contains the information for all patients who
have been added to a transplant wait list in the United States. Moreover
the patient data within the SRTR database is the base for a great deal
of the transplantation research going on in North America.

The goal of `LivTransplantEvolution` is to be an anlysis tool for
clinicians using the SRTR database and conducing research on Liver
transplantation who might not have the greatest familiarity with
technical analysis and computation. What it will do is analyze the Liver
transplantation relevant biomarkers, specifically those used in the
MELD, MELD-na and MELD-3.0 risk score calculations (Creatinine,
Bilirubin INR, Albumin & Sodium). It will visualize the trajectories of
those risk scores, compare them and extract the biomarkers most
impactful of each longitudinal change.

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

`LivTransplantEvolution` contains 6 functions.

*prepareData* Prepares and cleans up raw patient data from SRTR data
base to extract Liver transplantation relevant clinical serum
biomarkers, specifically those used in the MELD, MELD-na and MELD 3.0
risk score calculations.

*calcMELD* Will calculate the MELD score for a patient

*calcMELDna* Will calulcate the MELD-na score for a patient

*calcMELDthree* Will calculate the MELD-3.0 score for a patient

*createMELDdf* Will create a dataframe from the cleaned up data to
contain longitudinal MELD scores values at each timepoint

*visualizeMELD* Creates a visual with all 3 MELD score evolutions
together.

Package visual result:

![](./inst/MELD_scores.png)

## Contributions

This Package is Created by Mouaid Alim

## Refernces

Bibliography

1.  Potosek, J., Curry, M., Buss, M. & Chittenden, E. Integration of
    palliative care in end-stage liver disease and liver
    transplantation. J. Palliat. Med. 17, 1271–1277 (2014).
2.  Summary statistics on organ transplants, wait-lists and donors.
    Cihi.ca
    <https://www.cihi.ca/en/summary-statistics-on-organ-transplants-wait-lists-and-donors>.
3.  Kwong, A. J. et al. OPTN/SRTR 2019 annual data report: Liver. Am. J.
    Transplant 21 Suppl 2, 208–315 (2021).
4.  Kamath, P. et al. A model to predict survival in patients with
    end-stage liver disease. Gastroenterology 120, A76–A77 (2001).
5.  Biggins, S. W. et al. Serum sodium predicts mortality in patients
    listed for liver transplantation. Hepatology 41, 32–39 (2005).
6.  Kim, W. R. et al. MELD 3.0: The model for end-Stage Liver Disease
    updated for the modern era. Gastroenterology 161, 1887-1895.e4
    (2021).
7.  Liver transplant risk stratification. R-project.org
    <https://cran.r-project.org/web/packages/transplantr/vignettes/liver_recipient_scoring.html>.
8.  UCI Machine Learning. Indian Liver Patient Records. (2017).

## Acknowledgements

This package was developed as part of an assessment for 2023 BCB410H:
Applied Bioinformatics course at the University of Toronto, Toronto,
CANADA. LivTransplantEvolution welcomes issues, enhancement requests,
and other contributions. To submit an issue, use the GitHub issues.
