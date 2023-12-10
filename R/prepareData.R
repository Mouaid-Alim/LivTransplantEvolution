#' Process raw patient data and extract relevant biomarkers
#'
#' This function Process data from the STATHIIST_LIIN data file from the SRTR
#' transplantation dataset. The data must be in csv format.
#' The function extracts the relevant columns to calculate the MELD, MELD-na
#' and MELD 3.0 risk scores. MELD, MELD-na and MELD 3.0 are all risk scores used to
#' calculate the severity of end stage liver disease or liver dysfunction and
#' are used for the allocation of deceased liver organs in North America.
#' The extracted columns are CANHX_BEGIN_DT which
#' contains the date of each entry, CANHX_ALBUMIN which contains data for the
#' Albumin biomarker a protein which low levels of can indicate chronic liver
#' disease or liver dysfunction as is used in the MELD 3.0 calculation,
#' CANHX_BILI which contains data for the bilirubin biomarker a byproduct of red
#' blood cell breakdown in the liver which elevated levels of can indicate poor
#' liver function and is used in all calculating all three MELD scores,
#' CANHX_INR a biomarker which measures how long it takes for blood to clot and
#' is used in all three MELD score calculations as elveated levels off it
#' suggest impaired liver function, CANHX_SERUM_CREAT  which contains data for
#' the creatinine biomarker, which although primarily a marker of kidney
#' function, kidney function declines as liver disease progresses and so it is
#' used in calculating all three MELD scores; CANHX_SERUM_SODIUM which contains
#' the serum sodium data and is used in the MELD-na and MELD 3.0 calculations;
#' CANHX_DIAL_PRIOR_WEEK' which contains data on whether the
#' patient has had dialysis in the past week and is also a indication of kidney
#' function, it is used in the MELD and MELD-na calculations; CAN_GENDER which
#' contains the gender of the patient and is taken into account in the MELD 3.0
#' calculation; PX_ID which contains the ID of the patient the info in the row's
#' entry is for.
#'
#' @param file_path The csv file path for the data set containing the data. It
#' should have the the following columns PX_ID (The patients ID for that entry),
#' CANHX_BEGIN_DT (date of that specific entry into the dataset), CANHX_ALBUMIN
#' (ALbumin data), CANHX_BILI (Bilirubin data), CANHX_INR (INR data),
#' CANHX_SERUM_CREAT (Creatinine data), CANHX_SERUM_SODIUM (Serum Sodium data)
#' and 'CANHX_DIAL_PRIOR_WEEK' (Has the patient had dialysis in the past week)
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
