
#' Calculate MELD-3.0 score
#'
#' A function that calculates the MELD-3.0 score per Kim et al.
#'
#' @param Sex a boolean value indicating whether the patient is female or not.
#' As females get extra points in the calculation to account for biases.
#' @param creatinine a serum biomarker that reflects the kidney function. Unit:
#' mg/dl
#' @param bilirubin a biomarker that measures the ability of the liver to
#' process this byproduct from the breakdown of old red blood cells.  Elevated
#' levels in the blood (hyperbilirubinemia) indicate poor liver function
#' unit: mg/dl
#' @param inr International Normalized Ratio is a blood clotting test that is a
#' measure how quickly your blood clots and basically how thin your blood is.
#' @param sodium serum sodium level. Unit: mEq/L
#' @param albumin A liver synthesized protein which low levels of can indicate
#' chronic liver disease or liver dysfunction. Unit: mg/dL
#'
#' @return meldThreeScore The MELD-3.0 score calculation based on the paper by
#' Kim et al.
#' @export
#'
#' @examples
#' #Example 1
#' # Calculate the MELD-3.0 score for a female
#'
#' calcMELDThree(True, 2.3, 1.9, 2, 145)
#'
#' @references
#' Kim WR, Mannalithara A, Heimbach JK, Kamath PS, Asrani SK, Biggins SW,
#' Wood NL, Gentry SE, Kwong AJ. MELD 3.0: The Model for End-Stage Liver Disease
#'  Updated for the Modern Era. Gastroenterology. 2021 Dec;161(6):1887-1895.e4.
#'  doi: 10.1053/j.gastro.2021.08.050. Epub 2021 Sep 3. PMID: 34481845;
#'  PMCID: PMC8608337.
#'
#' Kamath PS, Wiesner RH, Malinchoc M, Kremers W, Therneau TM, Kosberg CL,
#'   D'Amico G, Dickson ER, Kim WR. A model to predict survival in patients with
#'   end-stage liver disease. Hepatology. 2001 Feb;33(2):464-70.
#'   doi: 10.1053/jhep.2001.22172. PMID: 11172350.
#'
calcMELDThree <- function(sex, creatinine, bilirubin, inr, sodium, albumin){

  if (creatinine > 3) {
    creatinine = 3
  } else if (creatinine < 1) {
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
  } else if (sodium > 137) {
    sodium = 137
  }
  if (albumin < 1.5) {
    albumin = 1.5
  } else if (albumin > 3.5) {
    albumin = 3.5
  }

  femaleOffset = 1.33 * sex
  creatValue = 11.14 * log(creatinine)
  biliValue = 4.56 * log(bilirubin)
  inrValue = 9.09 * log(inr)
  sodValue = 0.82 * (137 - sodium)
  albValue = 1.85 * (3.5 - albumin)
  sodBiliValue = 0.24 * (137 - sodium) * log(bilirubin)
  albCreatValue =  1.83 * (3.5 - albumin) * log(creatinine)

  meldThreeScore = femaleOffset + creatValue + biliValue + inrValue + albValue +
    sodValue - sodBiliValue - albCreatValue + 6

  meldThreeScore = round(meldThreeScore, digits = 0)

  if (meldThreeScore <  1) {
    meldThreeScore = 1
  }

  if (meldThreeScore > 40){
    meldThreeScore = 40
  }

  return(meldThreeScore)
}
