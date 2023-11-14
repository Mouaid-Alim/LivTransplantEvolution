
#' Calculate MELD-na score
#'
#' A function that calculates the MELD-na score per the UNOS/OPTN 2016 policy
#'
#' @param dialyisTwoTimeWeek a boolean value indicating whether the patient has
#' had dialysis at least twice in the past week. An indication of creatinine
#' level and kidney function
#' @param creatinine a serum biomarker that reflects the kidney function. Unit:
#' mg/dl
#' @param bilirubin a biomarker that measures the ability of the liver to
#' process this byproduct from the breakdown of old red blood cells.  Elevated
#' levels in the blood (hyperbilirubinemia) indicate poor liver function
#' unit: mg/dl
#' @param inr International Normalized Ratio is a blood clotting test that is a
#' measure how quickly your blood clots and basically how thin your blood is.
#' @param sodium serum sodium level. Unit: mEq/L
#'
#' @return meldNaScore The MELD-na score calculation based on the formula
#' per pages 4-5 in the January 2016 OPTN policy
#' @export
#'
#' @examples
#' #Example 1
#' # Calculate the MELDna score for a person who has dialyisTwoTimeWeek as True
#'
#' calcMELDna(True, 2.3, 1.9, 2, 145)
#'
#' @references
#' Kamath PS, Wiesner RH, Malinchoc M, Kremers W, Therneau TM, Kosberg CL,
#'   D'Amico G, Dickson ER, Kim WR. A model to predict survival in patients with
#'   end-stage liver disease. Hepatology. 2001 Feb;33(2):464-70.
#'   doi: 10.1053/jhep.2001.22172. PMID: 11172350.
#'
#' Wiesner R, Edwards E, Freeman R, Harper A, Kim R, Kamath P, Kremers W, Lake J,
#'  Howard T, Merion RM, Wolfe RA, Krom R; United Network for Organ Sharing
#'  Liver Disease Severity Score Committee. Model for end-stage liver disease
#'  (MELD) and allocation of donor livers. Gastroenterology.
#'  2003 Jan;124(1):91-6. doi: 10.1053/gast.2003.50016. PMID: 12512033.
#'
#'MELD serum sodium policy changes - OPTN. (n.d.). Hrsa.gov. Retrieved
#' November 14, 2023, from
#' https://optn.transplant.hrsa.gov/news/meld-serum-sodium-policy-changes/
calcMELDna <- function(dialyisTwoTimeWeek, creatinine, bilirubin, inr, sodium){

  if (dialyisTwoTimeWeek == 1 || creatinine > 4) {
    creatinine = 4
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
  } else if (sodium > 137) {
    sodium = 137
  }

  creatValue = 0.957*log(creatinine)
  biliValue = 0.378*log(bilirubin)
  inrValue = 1.120*log(inr)

  meldNaScore = (creatValue + biliValue + inrValue + 0.643)*10

  meldNaScore = round(meldNaScore, digits = 0)

  if (meldNaScore <  1) {
    meldNaScore = 1
  } else if (meldNaScore > 11){
    meldNaScore = meldNaScore + 1.32*(137 - sodium) -
      (0.033*meldNaScore*(137 - sodium))
  }

  if (meldNaScore > 40){
    meldNaScore = 40
  }

  return(meldNaScore)
}
