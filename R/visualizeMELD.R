

library(ggplot2)
library(tidyr)

# Function to plot MELD scores
#'
#'
#' @param meldScoresdf a data frame containing the MELD scores organized by date
#' for MELD, MELDna and MELD-3.0 generated using the createMELDdf function
#'
#' @return no return, plots MELD scores using ggplot2
#' @export
#' @import tidyr
#' @import ggplot2
#'
#' @examples
#' #Example 1
#' visualizeMELD(MELD_scores_df)
#'
#' @references
#'
visualizeMELD <- function(meldScoresdf) {
  # Reshaping the data for plotting
  longMeldScoresdf <- tidyr::pivot_longer(meldScoresdf,
                                             cols = c("MELD", "MELDna", "MELD3_0"),
                                             names_to = "MELDType",
                                             values_to = "Score")

  # Creating the plot
  ggplot(longMeldScoresdf, aes(x = Date, y = Score, color = MELDType, group = MELDType)) +
    geom_line() +
    labs(title = "MELD Scores Over Time",
         x = "Date",
         y = "MELD Score") +
    theme_minimal() +
    scale_color_brewer(palette = "Set1")
}
