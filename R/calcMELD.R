
#' Calculate MELD score
#'
#' A function that calculates the original MELD score
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
#' @param inr International NOrmalized Ratio is a blood clotting test that is a
#' measure how quickly your blood clots and basically how thin your blood is.
#'
#' @return meldScore the MELD score calculation based on the formula devised by
#' Kamath et al.
#' @export
#'
#' @examples
#' #Example 1
#' # Calculate the MELD score for a person who has dialyisTwoTimeWeek as True
#'
#' calcMELD(True, 2.3, 1.9, 2)
#'
#' @references
#' Kamath PS, Wiesner RH, Malinchoc M, Kremers W, Therneau TM, Kosberg CL,
#' D'Amico G, Dickson ER, Kim WR. A model to predict survival in patients with
#' end-stage liver disease. Hepatology. 2001 Feb;33(2):464-70.
#' doi: 10.1053/jhep.2001.22172. PMID: 11172350.
#'
#' Wiesner R, Edwards E, Freeman R, Harper A, Kim R, Kamath P, Kremers W, Lake J,
#'  Howard T, Merion RM, Wolfe RA, Krom R; United Network for Organ Sharing
#'  Liver Disease Severity Score Committee. Model for end-stage liver disease
#'  (MELD) and allocation of donor livers. Gastroenterology.
#'  2003 Jan;124(1):91-6. doi: 10.1053/gast.2003.50016. PMID: 12512033.
#'
#'
calcMELD <- function(dialyisTwoTimeWeek, creatinine, bilirubin, inr){

  if (dialyisTwoTimeWeek == 1) {
    creatinine = 4
  }

  creatValue = 0.957*log(creatinine)
  biliValue = 0.378*log(bilirubin)
  inrValue = 1.120*log(inr)

  meldScore = (creatValue + biliValue + inrValue + 0.643)*10

  meldScore = round(meldScore, digits = 0)

  if (meldScore <  1) {
    meldScore = 1
  }
  if (meldScore > 40) {
    meldScore = 40
  }

  return(meldScore)
}
