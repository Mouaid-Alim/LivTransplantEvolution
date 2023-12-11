
#' Create a data frame with copmuted MELD, MELD-na and MELD3.0 scores for each
#' entry
#'
#' This function uses the calcMELD, calcMELDna and calcMELDThree functions to
#' calculate the different MELD scores for each row entry in the provided
#' dataframe. The provided dataframe should correspond to the dataframe created
#' by running the prepareData function on the raw SRTR stathiist_liin datafile
#' in csv form. The input dataframe needs to have the following columns:
#' Patient_ID, Data, Albumin, Bilirubin, INR, Serum_Creatinine, Serum_Sodium,
#' Gender, Dialysis_In_Past_Week.
#'
#' @param data Dataframe created by the prepareData function. Needs to contain
#' the following columns:
#' Patient_ID, Data, Albumin, Bilirubin, INR, Serum_Creatinine, Serum_Sodium,
#' Gender, Dialysis_In_Past_Week.
#'
#' @return MELDdf, a dataframe containing the following columns: Patient_ID,
#' Date, MELD, MELDna, MELD3_0.
#' It will contain the MELD, MELD-na and MELD-3.0 scores from the input
#' dataframe containing the Liver transplantation biomarkers and clinical data
#' @export
#' @import dplyr
#' @import purrr
#' @import devtools
#' @import tidyr
#'
#' @examples
#' # Example 1
#' # Load the sample cleaned up data from the CleanedUpSamplePatientData.rda
#' # file,  then use it ro run the createMELDdf function
#' \dontrun{
#' load("../data/cleanedUpSamplePatientData.rda")
#' createMELDdf(cleanedUpSamplePatientData)
#' }
#'
#' @references
#'Wickham H, François R, Henry L, Müller K, Vaughan D (2023).
#'   *dplyr: A Grammar of Data Manipulation.* R package version 1.1.4,
#'   https://github.com/tidyverse/dplyr, https://dplyr.tidyverse.org.
#'
#'Wickham H, Henry L (2023). *purrr: Functional Programming Tools.*
#'   R package version 1.0.2,
#'   https://github.com/tidyverse/purrr, https://purrr.tidyverse.org/.
#'
#'Wickham H, Hester J, Chang W, Bryan J (2022).
#'   *devtools: Tools to Make Developing R Packages Easier.*
#'   https://devtools.r-lib.org/, https://github.com/r-lib/devtools.
#'
#'Wickham H, Vaughan D, Girlich M (2023). *tidyr: Tidy Messy Data.*
#'   R package version 1.3.0,
#'   https://github.com/tidyverse/tidyr, https://tidyr.tidyverse.org.
#'
createMELDdf <- function(data) {

  # Input Checks
  # check if all coloumns present
  if (!("Patient_ID" %in% colnames(data))) {
    stop("Your dataset does not have the Patient_ID column, make sure it does
         before running this function")
  }
  if (!("Date" %in% colnames(data))) {
    stop("Your dataset does not have the Date column, make sure it does
         before running this function")
  }
  if (!("Albumin" %in% colnames(data))) {
    stop("Your dataset does not have the Albumin column, make sure it does
         before running this function")
  }
  if (!("INR" %in% colnames(data))) {
    stop("Your dataset does not have the INR column, make sure it
         does before running this function")
  }
  if (!("Serum_Creatinine" %in% colnames(data))) {
    stop("Your dataset does not have the Serum_Creatinine column, make sure it
         does before running this function")
  }
  if (!("Serum_Sodium" %in% colnames(data))) {
    stop("Your dataset does not have the Serum_Sodium column, make sure it
         does before running this function")
  }
  if (!("Gender" %in% colnames(data))) {
    stop("Your dataset does not have the Gender column, make sure
         it does before running this function")
  }
  if (!("Dialysis_In_Past_Week" %in% colnames(data))) {
    stop("Your dataset does not have the Dialysis_In_Past_Week column, make
         sure it does before running this function")
  }

  # install dplyr package if it is not already installed
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    install.packages("dplyr")
    library(dplyr)
  }
  # install purrr package if it is not already installed
  if (!requireNamespace("purrr", quietly = TRUE)) {
    install.packages("purrr")
    library(purrr)
  }
  # install tidyr package if it is not already installed
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    install.packages("tidyr")
    library(tidyr)
  }
  # install devtools package if it is not already installed
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
    library(devtools)
  }
  # load MELD, MELD-na and MELD-3.0 functions
  source("./calcMELD.R")
  source("./calcMELDna.R")
  source("./calcMELDThree.R")


  # Drop rows with na values from the data frame
  data <- data %>% drop_na()

  # Add entries to each row in the input data frame for the
  # calculated MELD, MELD-na and MELD-3.0 risk score values using the functions
  # created specifcally to calculate them in the LivTransplantEvolution package
  MELDdf <- data %>%
    dplyr::mutate(
      MELD = purrr::pmap_dbl(list(Dialysis_In_Past_Week, Serum_Creatinine, Bilirubin, INR), LivTransplantEvolution::calcMELD),
      MELDna = purrr::pmap_dbl(list(Dialysis_In_Past_Week, Serum_Creatinine, Bilirubin, INR, Serum_Sodium), LivTransplantEvolution::calcMELDna),
      MELD3_0 = purrr::pmap_dbl(list(Gender, Serum_Creatinine, Bilirubin, INR, Serum_Sodium, Albumin), LivTransplantEvolution::calcMELDThree)
    ) %>%
    # return a dataframe containing the patient_Id, the date of the entry and
    # the MELD scores
    dplyr::select(Patient_ID, Date, MELD, MELDna, MELD3_0)

  return(MELDdf)
}


