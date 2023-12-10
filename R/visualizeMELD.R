#' Function to plot MELD scores
#'
#' A function that takes a dataframe created by the createMELDdf function and a
#' valid patient ID and returns the plotted MELD, MELD-na and MELD-3.0
#' trajectories for that patient using ggplot2. The output is a longitudinal
#' line graph of each different MELD score on the same line graph.
#' A valid input data frame must have the following non-empty columns:
#' Patient_ID (patient ID of that row's entries), Date (date of entry),
#' MELD (MELD score for that entry), MELDna (MELD-na score for that entry),
#' MELD3_0 (MELD-3.0 score for that entry).
#'
#'
#' @param meldScoresdf data frame generated using the createMELDdf function
#' containing the following non-empty columns:
#' Patient_ID (patient ID of that row's entries), Date (date of entry),
#' MELD (MELD score for that entry), MELDna (MELD-na score for that entry),
#' MELD3_0 (MELD-3.0 score for that entry). The dataframe would have MELD,
#' MELD-na and MELD-3.0 risk score values at different dates for different
#' patients
#' @param patient_id A valid numerical patient ID that represents some row
#' entries in the MELDdf provided
#'
#' @return no return, plots longitudinal trajectory of MELD, MELD-na and
#' MELD-3.0 risk score trajectories for specified patient with patient_id using
#' ggplot2
#'
#' @export
#' @import tidyr
#' @import ggplot2
#'
#' @examples
#' # Example 1
#' # load the sample MELDdf from the sampleMELDdf.rda file then run visuaizeMELD
#' # on it to visualize the MELD score for that patient
#' \dontrun{
#' load("../data/sampleMELDdf.rda")
#' visualizeMELD(sampleMELDdf)
#' }
#'
#' @references
#' Wickham H (2016). *ggplot2: Elegant Graphics for Data Analysis.*
#'     Springer-Verlag New York. ISBN 978-3-319-24277-4,
#'     https://ggplot2.tidyverse.org.
#'
#' Wickham H, Vaughan D, Girlich M (2023). *tidyr: Tidy Messy Data. R package*
#'     version 1.3.0, https://github.com/tidyverse/tidyr,
#'     https://tidyr.tidyverse.org.
visualizeMELD <- function(meldScoresdf, patient_id) {

  # Input Checks
  # check if all coloumns present
  if (!("Patient_ID" %in% colnames(meldScoresdf))) {
    stop("Your dataset does not have the Patient_ID column, make sure it does
         before running this function")
  }
  if (!("Date" %in% colnames(meldScoresdf))) {
    stop("Your dataset does not have the Date column, make sure it does
         before running this function")
  }
  if (!("MELD" %in% colnames(meldScoresdf))) {
    stop("Your dataset does not have the MELD column, make sure it does
         before running this function")
  }
  if (!("MELDna" %in% colnames(meldScoresdf))) {
    stop("Your dataset does not have the MELDna column, make sure it
         does before running this function")
  }
  if (!("MELD3_0" %in% colnames(meldScoresdf))) {
    stop("Your dataset does not have the MELD3_0 column, make sure it
         does before running this function")
  }

  # if non-valid patient_id terminate
  if (!(any(meldScoresdf == patient_id))) {
    stop("Your patient_id is invalid and is not present in your provided
         meldscoresdf. Please provide a valud patient_id and try again")
  }


  # install tidyr package if it is not already installed
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    install.packages("tidyr")
    library(tidyr)
  }
  # install ggplot2 package if it is not already installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
    library(ggplot2)
  }

  # keep entries for patient with provided patient_id
  meldScoresdf <- meldScoresdf[meldScoresdf$Patient_ID == patient_id, ]

  # Reshaping the data for plotting
  longMeldScoresdf <- tidyr::pivot_longer(meldScoresdf,
                                             cols = c("MELD", "MELDna", "MELD3_0"),
                                             names_to = "MELDType",
                                             values_to = "Score")

  # Create plot title
  plot_title <- sprintf("MELD Scores Over Time for patient: %d", patient_id)
  # Creating the plot
  ggplot(longMeldScoresdf, aes(x = Date, y = Score, color = MELDType, group = MELDType)) +
    geom_line() +
    labs(title = plot_title,
         x = "Date",
         y = "MELD Score") +
    theme_minimal() +
    scale_color_brewer(palette = "Set1")
}
