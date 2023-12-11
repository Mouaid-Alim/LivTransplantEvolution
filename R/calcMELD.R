
#' Calculate the Model for End-Stage Liver Disease (MELD) risk score
#'
#' A function that calculates the MELD-na score using the formula
#' introduced by Kamath et al.
#' Model for End-Stage Liver Disease (MELD) risk score is used to measure the
#' severity of Liver dysfunction and is used for the allocation of Deceased
#' Liver Organs in North America. It indicates the 90 day chances of mortality
#' for this patient. The calculation of MELD requires 3 biomarkers, the
#' patient's Creatinine, Bilirubin and International Normalized Ratio (INR).
#' Whether the patient has had dialysis in the past Week is also taken as input.
#' Although it is not directly used in the formula but is an indication of
#' creatinine level and kidney function. It is used to augment creatinine value
#' and automatically set it to 4 if TRUE. The MELD score informs the 90 day
#' mortality of that patient and can be interpreted as follows:
#' MELD is ≤9 = 1.9% Mortality, MELD is 10–19 = 6.0% Mortality,
#' MELD is 20–29 = 19.6% Mortality, MELD is 30–39 = 52.6% Mortality,
#' MELD is ≥40 = 71.3% Mortality.
#'
#' @param dialyisTwoTimeWeek a boolean value indicating whether the patient has
#' had dialysis at least twice in the past week. An indication of creatinine
#' level and kidney function. Set Creatinine value automatically to 4 if value is
#' TRUE
#' @param creatinine a numeric value of type 'double' for the creatinine
#' biomarker which reflects kidney function, as kidney dysfunction is often
#' associated with worsening Liver function. Unit: mg/dl
#' @param bilirubin a numeric value of type 'double' for the bilirubin biomarker
#' that measures the ability of the liver to process byproducts from the
#' breakdown of old red blood cells. Elevated levels in the blood
#' (hyperbilirubinemia) indicate poor liver function. Unit: mg/dl
#' @param inr a numeric value of type 'double' for International Normalized
#' Ratio (INR). INR is a blood clotting test that is a measure how quickly your
#' blood clots and basically how thin your blood is. It is unitless
#'
#' @return meldScore the MELD score calculation based on the formula devised by
#' Kamath et al. MELD score is between 1 and 40 and any calculated value above
#' or below these boundary values are set as closest boundary value and returned
#' The MELD score informs the 90 day mortality of that patient and can be
#' interpreted as follows: MELD is ≤9 = 1.9% Mortality,
#' MELD is 10–19 = 6.0% Mortality, MELD is 20–29 = 19.6% Mortality,
#' MELD is 30–39 = 52.6% Mortality, MELD is ≥40 = 71.3% Mortality.
#' @export
#'
#' @examples
#' #Example 1
#' # Calculate the MELD score for a person who has dialyisTwoTimeWeek as True
#'
#' calcMELD('TRUE', 2.3, 1.9, 2)
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
#' MELD serum sodium policy changes - OPTN. (n.d.). Hrsa.gov. Retrieved
#'   November 14, 2023, from
#'   https://optn.transplant.hrsa.gov/news/meld-serum-sodium-policy-changes/
#'
calcMELD <- function(dialysisTwoTimeWeek = 'FALSE', creatinine = 1.3,
                     bilirubin = 1.2, inr = 1.1){

  # Input Checks
  if (!(dialysisTwoTimeWeek %in% c('TRUE', 'FALSE') && typeof(dialysisTwoTimeWeek) == 'character')) {
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

  # If patient had dialysis in the past week set creatinine value to 4
  # automatically
  if (dialysisTwoTimeWeek == 'TRUE') {
    creatinine = 4
  }

  # Calculate Creatinine portion of MELD calculation
  creatValue = 0.957*log(creatinine)

  # Calculate Bilirubin portion of MELD calculation
  biliValue = 0.378*log(bilirubin)

  # Calculate INR portion of MELD calculation
  inrValue = 1.120*log(inr)

  # Calculate MELD score using above values
  meldScore = (creatValue + biliValue + inrValue + 0.643)*10

  # Round MELD score to whole number
  meldScore = round(meldScore, digits = 0)

  # MELD score is between 1 and 40 so if resulting value is outside these
  # boundaries, set MELD score to closest boundary value
  if (meldScore <  1) {
    meldScore = 1
  }
  if (meldScore > 40) {
    meldScore = 40
  }

  return(meldScore)
}
