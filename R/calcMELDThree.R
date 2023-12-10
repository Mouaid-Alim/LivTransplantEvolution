
#' Calculate the Model for End-Stage Liver Disease-3.0 (MELD-3.0) risk score
#'
#' A function that calculates the MELD-3.0 score using the formula
#' introduced by Kim et all.
#' Model for End-Stage Liver Disease-3.0 (MELD-3.0) risk score is used to
#' measure theseverity of Liver dysfunction. It indicates the estimated 90-day
#' chances of survival for this patient. MELD-3.0 was created as an attempt to
#' overcome some of the equity issues associated with the MELD and MELD-na
#' scores. The calculation of MELD-3.0 requires 5 biomarkers, the
#' patient's Creatinine, Bilirubin and International Normalized Ratio (INR),
#' Albumin and Serum Sodium levels. The MELD-3.0 informs the estimated 90 day
#' chances of survival of that patient and can be interpreted using this
#' formula: 0.946^((0.17698 \* MELD 3.0) – 3.56) \* 100
#'
#' @param Sex a boolean value indicating whether the patient is female or not.
#' As females get extra points in the calculation to account for biases.
#' @param creatinine a numeric value of type 'double' for the creatinine
#' biomarker which reflects kidney function, as kidney dysfunction is often
#' associated with worsening Liver function. Unit: mg/dl
#' @param bilirubin  a numeric value of type 'double' for the bilirubin biomarker
#' that measures the ability of the liver to process byproducts from the
#' breakdown of old red blood cells. Elevated levels in the blood
#' (hyperbilirubinemia) indicate poor liver function. Unit: mg/dl
#' @param inr  a numeric value of type 'double' for International Normalized
#' Ratio (INR). INR is a blood clotting test that is a measure how quickly your
#' blood clots and basically how thin your blood is. It is unitless
#' @param sodium a numeric value of type 'double' for the serum sodium level.
#' Another indicator of Kidney function. Sodium is only taken into account for
#' the MELD-na calculation if the MELD is > 11. Unit: mEq/L
#' @param albumin a numeric value of type 'double' for the albumin biomarker.
#' Albumin is a liver synthesized protein which low levels of can indicate
#' chronic liver disease or liver dysfunction. Unit: mg/dL
#'
#' @return meldThreeScore The 'double' value returned from calculating the
#' MELD-3.0 score calculation based on the paper by Kim et al.
#' @export
#'
#' @examples
#' #Example 1
#' # Calculate the MELD-3.0 score for a female
#'
#' calcMELDThree('F', 2.3, 1.9, 2, 145, 2)
#'
#' @references
#' Kim WR, Mannalithara A, Heimbach JK, Kamath PS, Asrani SK, Biggins SW,
#'  Wood NL, Gentry SE, Kwong AJ. MELD 3.0: The Model for End-Stage Liver Disease
#'  Updated for the Modern Era. Gastroenterology. 2021 Dec;161(6):1887-1895.e4.
#'  doi: 10.1053/j.gastro.2021.08.050. Epub 2021 Sep 3. PMID: 34481845;
#'  PMCID: PMC8608337.
#'
#' Kamath PS, Wiesner RH, Malinchoc M, Kremers W, Therneau TM, Kosberg CL,
#'   D'Amico G, Dickson ER, Kim WR. A model to predict survival in patients with
#'   end-stage liver disease. Hepatology. 2001 Feb;33(2):464-70.
#'   doi: 10.1053/jhep.2001.22172. PMID: 11172350.
#'
calcMELDThree <- function(sex = 'M', creatinine = 1.3,
                          bilirubin = 1.2, inr = 1.1, sodium = 140,
                          albumin = 4){

  # Input Checks
  if (!(typeof(creatinine) %in% "double")) {
    stop("The value for the creatinine parameter is not of type double. It needs
         to be of type double. Make sure you enter a value of type double for
         this variable and try again")
  }
  if (!(typeof(bilirubin) %in% "double")) {
    stop("The value for the bilirubin parameter is not of type double. It needs
         to be of type double. Make sure you enter a value of type double for
         this variable and try again")
  }
  if (!(typeof(inr) %in% "double")) {
    stop("The value for the inr parameter is not of type double. It needs
         to be of type double. Make sure you enter a value of type double for
         this variable and try again")
  }
  if (!(typeof(sodium) %in% "double")) {
    stop("The value for the sodium parameter is not of type double. It needs
         to be of type double. Make sure you enter a value of type double for
         this variable and try again")
  }
  if (!(typeof(albumin) %in% "double")) {
    stop("The value for the albumin parameter is not of type double. It needs
         to be of type double. Make sure you enter a value of type double for
         this variable and try again")
  }

  # Boundary checks for the input values. If they fall outside the below
  # boundaries the values are just set to the closest boundary
  if (creatinine > 3) {
    creatinine = 3
  }
  if (creatinine < 1) {
    creatinine = 1
  }
  if (bilirubin < 1) {
    bilirubin = 1
  }
  if (inr < 1) {
    inr = 1
  }
  if (sodium < 125) {
    sodium = 125
  }
  if (sodium > 137) {
    sodium = 137
  }
  if (albumin < 1.5) {
    albumin = 1.5
  }
  if (albumin > 3.5) {
    albumin = 3.5
  }

  # calculate offset based on whether the patien is male or female
  if (sex == 'F') {
    femaleOffset <- 1.33
  }  else {
    femaleOffset <- 0
  }

  # Calculate different components of the MELD-3.0 calculation
  creatValue <- 11.14 * log(creatinine)
  biliValue <- 4.56 * log(bilirubin)
  inrValue <- 9.09 * log(inr)
  sodValue <- 0.82 * (137 - sodium)
  albValue <- 1.85 * (3.5 - albumin)
  sodBiliValue <- 0.24 * (137 - sodium) * log(bilirubin)
  albCreatValue <-  1.83 * (3.5 - albumin) * log(creatinine)

  # Calculat the MELD-3.0 score
  meldThreeScore <- femaleOffset + creatValue + biliValue + inrValue + albValue +
    sodValue - sodBiliValue - albCreatValue + 6

  # Round the MELD-3.0 risk score value to the closest integer
  meldThreeScore <- round(meldThreeScore, digits = 0)

  # Set boundary if MELD-3.0 score below 1
  if (meldThreeScore <  1) {
    meldThreeScore <- 1
  }

  return(meldThreeScore)
}
