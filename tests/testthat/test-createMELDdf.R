
library(testthat)
library(dplyr)
library(purrr)
library(tidyr)
library(LivTransplantEvolution)

# Load the createMELDdf function source file
source("../../R/createMELDdf.R")

context("Testing createMELDdf function")

# Test 1: Column Name Checks
test_that("Column name checks for input dataframe", {
  temp_data <- data.frame(Patient_ID = 1:5, Date = Sys.Date(), Albumin = runif(5),
                          Bilirubin = runif(5), INR = runif(5), Serum_Creatinine = runif(5),
                          Serum_Sodium = runif(5), Gender = c('M', 'F', 'M', 'F', 'M'),
                          Dialysis_In_Past_Week = c('TRUE', 'FALSE', 'TRUE', 'FALSE', 'TRUE'))
  expect_error(createMELDdf(temp_data[, -which(names(temp_data) == 'Albumin')]))  # Removing Albumin column
  expect_error(createMELDdf(temp_data[, -which(names(temp_data) == 'INR')]))  # Removing INR column
})

# Test 2: Correct MELD Scores Computation
test_that("MELD scores are correctly computed for each row", {
  temp_data <- data.frame(Patient_ID = 1:5, Date = Sys.Date(), Albumin = runif(5),
                          Bilirubin = runif(5), INR = runif(5), Serum_Creatinine = runif(5),
                          Serum_Sodium = runif(5), Gender = c('M', 'F', 'M', 'F', 'M'),
                          Dialysis_In_Past_Week = c('TRUE', 'FALSE', 'TRUE', 'FALSE', 'TRUE'))
  meld_df <- createMELDdf(temp_data)
  expect_true(all(c('MELD', 'MELDna', 'MELD3_0') %in% names(meld_df)))
  # Add specific checks for expected MELD score values for sample rows
})

# Test 3: Function Output Structure
test_that("Output structure is correct", {
  temp_data <- data.frame(Patient_ID = 1:5, Date = Sys.Date(), Albumin = runif(5),
                          Bilirubin = runif(5), INR = runif(5), Serum_Creatinine = runif(5),
                          Serum_Sodium = runif(5), Gender = c('M', 'F', 'M', 'F', 'M'),
                          Dialysis_In_Past_Week = c('TRUE', 'FALSE', 'TRUE', 'FALSE', 'TRUE'))
  meld_df <- createMELDdf(temp_data)
  expect_true(is.data.frame(meld_df))
  expect_equal(ncol(meld_df), 5)  # Checking for correct number of columns
})
