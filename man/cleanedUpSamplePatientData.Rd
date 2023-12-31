% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{cleanedUpSamplePatientData}
\alias{cleanedUpSamplePatientData}
\title{Cleaned up sample patient data generated by the prepareData function and made
for users to use as input to the createMELDdf function}
\format{
A dataframe with columns:
\describe{
\item{Patient_ID}{Contains the numeric patient ID of the patient the data in entries
in that row are for}
\item{Date}{The date that corresponding to the data in that row}
\item{Albumin}{Contains numeric data for the Albumin biomarker, a protein which low
levels of can indicate chronic liver disease or liver dysfunction. It is
used in the MELD 3.0 calculation,}
\item{Bilirubin}{Contains numberic data for the bilirubin biomarker
a byproduct of red blood cell breakdown in the liver which elevated levels
of can indicate poor liver function and is used in all calculating all
three MELD scores}
\item{INR}{Contains numeric data for the International Normalized
Ratio (INR) biomarker. This biomarker measures how long it takes for blood
to clot and is used in all three MELD score calculations as elveated
levels off it suggest impaired liver function}
\item{Serum_Creatinine}{Contains data for the creatinine biomarker, which
although primarily a marker of kidney function, kidney function declines
as liver disease progresses and so it is used in calculating all three
MELD scores}
\item{Serum_Sodium}{contains the serum sodium data which is used in the
MELD-na and MELD 3.0 calculations}
\item{Gender}{Contains the gender of the patient and is taken into account
in the MELD 3.0 calculation}
\item{Dialysis_In_past_Week}{Contains data on whether the
patient has had dialysis in the past week and is also a indication of kidney
function, it is used in the MELD and MELD-na calculations}
}
}
\source{
Created by Mouaid Alim the package author.
}
\usage{
cleanedUpSamplePatientData
}
\description{
Cleaned up sample patient data generated by calling the prepareData function
on the updated_meld_patient_Data.csv file found under the inst/extdata
directory. This sample data is ideal data that would be created from running
the prepareData function on the stathiist_liin data file from the SRTR
database. This sample dataframe is ideal to use to test out the createMELDdf
function
}
\examples{
\dontrun{
 cleanedUpSamplePatientData
}
}
\keyword{datasets}
