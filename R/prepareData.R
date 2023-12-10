#' Process raw patient SRTR data stathiist_liin data file and extract relevant
#'  biomarkers
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
#' # Example 1
#' # provide the prepareData function with the path to a csv folder containg the
#' # data and it will return a dataframe with the relevant data
#' \dontrun{
#' cleanedData = prepareData(../inst//extdata/updated_meld_patient_data.csv)
#'}
#' @references
#' The SRTR database.
#'     Srtr.org https://www.srtr.org/about-the-data/the-srtr-database/.
#'
#' Kamath, P. et al. A model to predict survival in patients with end-stage
#'     liver disease. Gastroenterology 120, A76–A77 (2001).
#'
#' Kim, W. R. et al. MELD 3.0: The model for end-Stage Liver Disease updated for
#'      the modern era. Gastroenterology 161, 1887-1895.e4 (2021).
#'
#' Kwong, A. J. et al. OPTN/SRTR 2019 annual data report: Liver. Am. J.
#'     Transplant 21 Suppl 2, 208–315 (2021).
#'
#' Slack, A., Yeoman, A. & Wendon, J. Renal dysfunction in chronic liver
#'     disease. Crit. Care 14, 214 (2010).
#'
#' Biggins, S. W. et al. Serum sodium predicts mortality in patients listed for
#'     liver transplantation. Hepatology 41, 32–39 (2005).
#'
#' Guerra Ruiz, A. R. et al. Measurement and clinical usefulness of bilirubin in
#'      liver disease. Adv. Lab. Med. 2, 352–361 (2021).
#'
#' Kalas, M. A., Chavez, L., Leon, M., Taweesedt, P. T. & Surani, S. Abnormal
#'     liver enzymes: A review for clinicians. World J. Hepatol.
#'     13, 1688–1698 (2021).
#'
#' Tufoni, M., Zaccherini, G., Caraceni, P. & Bernardi, M. Albumin: Indications
#'     in chronic liver disease. United European Gastroenterol. J.
#'     8, 528–535 (2020).
#'
#'
#'
prepareData <- function(filePath) {
  # install reader package if it is not already installed
  if (!requireNamespace("readr", quietly = TRUE)) {
    install.packages("readr")
    library(readr)
  }

  data <- readr::read_csv(filePath)

  # Input Checks
  # check if all coloumns present
  if (!("CANHX_BEGIN_DT" %in% colnames(data))) {
    stop("Your dataset does not have the CANHX_BEGIN_DT column, make sure it does
         before running this function")
  }
  if (!("CANHX_ALBUMIN" %in% colnames(data))) {
    stop("Your dataset does not have the CANHX_ALBUMIN column, make sure it does
         before running this function")
  }
  if (!("CANHX_BILI" %in% colnames(data))) {
    stop("Your dataset does not have the CANHX_BILI column, make sure it does
         before running this function")
  }
  if (!("CANHX_SERUM_CREAT" %in% colnames(data))) {
    stop("Your dataset does not have the CANHX_SERUM_CREAT column, make sure it
         does before running this function")
  }
  if (!("CANHX_INR" %in% colnames(data))) {
    stop("Your dataset does not have the CANHX_INR column, make sure it does
         before running this function")
  }
  if (!("CANHX_SERUM_SODIUM" %in% colnames(data))) {
    stop("Your dataset does not have the CANHX_SERUM_SODIUM column, make sure it
         does before running this function")
  }
  if (!("CANHX_DIAL_PRIOR_WEEK" %in% colnames(data))) {
    stop("Your dataset does not have the CANHX_DIAL_PRIOR_WEEK column, make sure
         it does before running this function")
  }
  if (!("CAN_GENDER" %in% colnames(data))) {
    stop("Your dataset does not have the CAN_GENDER column, make sure it does
         before running this function")
  }

  # Extract Relevant columns to create new data frame
  selectedData <- data %>%
    dplyr::select(PX_ID, CANHX_BEGIN_DT, CANHX_ALBUMIN, CANHX_BILI, CANHX_INR,
                  CANHX_SERUM_CREAT, CANHX_SERUM_SODIUM, CAN_GENDER,
                  CANHX_DIAL_PRIOR_WEEK)
  # remove rows with al na values
  selectedData[rowSums(is.na(selectedData)) != ncol(selectedData),]

  #rename columns to be more intuitive
  colnames(selectedData) <- c('Patient_ID', 'Data', 'Albumin', 'Bilirubin', 'INR',
                           'Serum_Creatinine', 'Serum_Sodium', 'Gender',
                           'Dialysis_In_Past_Week')
  return(selectedData)
}
