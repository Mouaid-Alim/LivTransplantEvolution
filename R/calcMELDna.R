
#' Calculate the Model for End-Stage Liver Disease-Sodium (MELD-na) risk score
#'
#' A function that calculates the MELD-na score using the formula
#' introduced by kamath et al and augmented by Biggins, S. W. et al.
#' Model for End-Stage Liver Disease (MELD) risk score is used to measure the
#' severity of Liver dysfunction and is used for the allocation of Deceased
#' Liver Organs in North America. It indicates the 90 day chances of mortality
#' for this patient. The calculation of MELD-na requires 4 biomarkers, the
#' patient's Creatinine, Bilirubin and International
#' Normalized Ratio (INR) and Serum Sodium level.  Whether the patient has had
#' dialyis in the past Week is also taken as input. Although it is not directly
#' used in the formula but is an indication of creatinine level and kidney
#' function. It is used to augment creatinine value and automatically set it
#' to 4 if TRUE. The MELD-na score calculation based on the formula per pages
#' 4-5 in the January 2016 OPTN policy. The MELD-na score informs the 90 day
#' mortality of that patient and can be interpreted as follows:
#' MELD-na is ≤9 = 1.9% Mortality,MELD-na is 10–19 = 6.0% Mortality,
#' MELD-na is 20–29 = 19.6% Mortality, MELD-na is 30–39 = 52.6% Mortality,
#' MELD-na is ≥40 = 71.3% Mortality.
#'
#' @param dialyisTwoTimeWeek a boolean value indicating whether the patient has
#' had dialysis at least twice in the past week. An indication of creatinine
#' level and kidney function. Set Creatine value automatically to 4 if value is
#' TRUE
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
#'
#' @return meldNaScore the MELD-na score calculation based on the formula devised by
#' Kamath et al. and augmented by Biggins, S. W. et al.The MELD-na score
#' calculation used here is based on the formula per pages 4-5 in the
#' January 2016 OPTN policy.
#' MELD-na score can be between 1 and 40 and any calculated value above or below
#' these boundary values are set as closest boundary value and returned.
#' The MELD-na score informs the 90 day mortality of that patient and can be
#' interpreted as follows: MELD-na is ≤9 = 1.9% Mortality,
#' MELD-na is 10–19 = 6.0% Mortality, MELD-na is 20–29 = 19.6% Mortality,
#' MELD-na is 30–39 = 52.6% Mortality, MELD-na is ≥40 = 71.3% Mortality.
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
#'   Howard T, Merion RM, Wolfe RA, Krom R; United Network for Organ Sharing
#'   Liver Disease Severity Score Committee. Model for end-stage liver disease
#'   (MELD) and allocation of donor livers. Gastroenterology.
#'   2003 Jan;124(1):91-6. doi: 10.1053/gast.2003.50016. PMID: 12512033.
#'
#' Biggins, S. W. et al. Serum sodium predicts mortality in patients listed for
#'   liver transplantation. *Hepatology* 41, 32–39 (2005).
#'
#' MELD serum sodium policy changes - OPTN. (n.d.). Hrsa.gov. Retrieved
#'   November 14, 2023, from
#'   https://optn.transplant.hrsa.gov/news/meld-serum-sodium-policy-changes/
calcMELDna <- function(dialyisTwoTimeWeek = 'FALSE', creatinine = 1.3,
                       bilirubin = 1.2, inr = 1.1, sodium = 140){

  # Input Checks
  if (!(dialyisTwoTimeWeek %in% c('TRUE', 'FALSE'))) {
    stop("The dialysisTwoTimeWeek value is not in c('TRUE', 'FALSE'). Make sure
         you enter a string in c('TRUE', 'FALSE') for this variable and try
         again")
  }
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

  # Boundary checks for the input values. If they fall outside the below
  # boundaries the values are just set to the closest boundary
  if (dialyisTwoTimeWeek == 'TRUE' || creatinine > 4) {
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


  # Calculate Creatinine portion of MELD calculation
  creatValue = 0.957*log(creatinine)

  # Calculate Bilirubin portion of MELD calculation
  biliValue = 0.378*log(bilirubin)

  # Calculate INR portion of MELD calculation
  inrValue = 1.120*log(inr)

  # Calculate MELD score using above values
  meldNaScore = (creatValue + biliValue + inrValue + 0.643)*10

  # Checking Boundary values for MELDna score and calculating MELD-na only if
  # the above MELD is above 11 as sodium is not impactful for MELD below 11
  if (meldNaScore <  1) {
    meldNaScore <- 1
  } else if (meldNaScore > 11){
    meldNaScore = meldNaScore + 1.32*(137 - sodium) -
      (0.033*meldNaScore*(137 - sodium))
  }
  if (meldNaScore > 40){
    meldNaScore = 40
  }

  # Round MELD-na score to whole number
  meldNaScore = round(meldNaScore, digits = 0)

  return(meldNaScore)
}
