
library(dplyr)
library(purrr)
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

calcMELDThree <- function(sex, creatinine, inr, bilirubin, sodium, albumin){

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



#' Create a data frame with all MELD scores
#'
#' This function uses the functions to calculate the different MELD scores to
#' to then create a dataframe containing MELD score values corresopnding to each
#' row in the dataframe created by the preapreData function
#'
#' @param data dataframe created by the preapreData function
#'
#' @return MELDdf, a dataframe containing the different values of the differnet
#' MELD scores by date
#' @export
#' @import dplyr
#' @import purrr
#'
#' @examples
#' #Example 1
#' createMELDdf(liver_df)
#'
createMELDdf <- function(data) {
  MELDdf <- data %>%
    dplyr::mutate(
      MELD = purrr::pmap_dbl(list(DialysisTwiceInPastWeek, Creatinine, Total_Bilirubin, INR), calcMELD),
      MELDna = purrr::pmap_dbl(list(DialysisTwiceInPastWeek, Creatinine, Total_Bilirubin, INR, Sodium), calcMELDna),
      MELD3_0 = purrr::pmap_dbl(list(Sex, Creatinine, Total_Bilirubin, INR, Sodium, Albumin), calcMELDThree)
    ) %>%
    dplyr::select(Date, MELD, MELDna, MELD3_0)

  return(MELDdf)
}


