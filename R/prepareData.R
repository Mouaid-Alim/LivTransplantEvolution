#' Process data and extract relevant biomarkers
#'
#' process the clincial serum blood test and relevant data from a csv data set
#' and return a dataframe containing only coloumns relevant to the calculation
#' of MELD scores
#'
#' @param file_path the csv file path for the data set containing the data. It
#' should have the the following coloumns at least Date, Sex,
#' DialysisTwiceInPastWeek, Creatinine, INR, Total_Bilirubin, Sodium, Albumin
#'
#' @return selectedData, a dataframe containing the relevant columns to
#' calculate MELD scores
#' @export
#' @import readr
#' @import dplyr
#' @examples
#' #Example 1
#' prepareDate(../data/liver_data)
#'
#' @references
#' UCI Machine Learning. (2017). Indian Liver Patient Records [Data set].
#'
prepareData <- function(filePath) {
  if (!requireNamespace("readr", quietly = TRUE)) {
    install.packages("readr")
    library(readr)
  }

  data <- readr::read_csv(filePath)

  selectedData <- data %>%
    dplyr::select(Date, Sex, DialysisTwiceInPastWeek, Creatinine, INR,
                  Total_Bilirubin, Sodium, Albumin)
  selectedData[rowSums(is.na(selectedData)) != ncol(selectedData),]
  return(selectedData)
}
