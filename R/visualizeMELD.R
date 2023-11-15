

library(ggplot2)
library(tidyr)

#' Function to plot MELD scores
#'
#' Visualize all the MELD scores for the patietns
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
#' Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis.
#' Springer-Verlag New York. ISBN 978-3-319-24277-4,
#' https://ggplot2.tidyverse.org.
#'
#' Wickham H, Vaughan D, Girlich M (2023). tidyr: Tidy Messy Data. R package
#' version 1.3.0, https://github.com/tidyverse/tidyr,
#' https://tidyr.tidyverse.org.
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
