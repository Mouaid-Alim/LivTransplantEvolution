library(testthat)
library(readr)
library(dplyr)

# Load the prepareData function source file
source("../../R/prepareData.R")

context("Testing prepareData function")

# Path to the sample CSV file
sample_file_path <- "../../inst/extdata/updated_meld_patient_data.csv"

# Test 1: Input File Path Validation
test_that("Invalid file paths are handled", {
  expect_error(prepareData("non_existent_file.csv"))
})

# Test 2: Data Column Validation
test_that("Data missing required columns is handled", {
  temp_file <- tempfile(fileext = ".csv")
  data.frame(A = 1:5, B = 1:5) %>% write_csv(temp_file)
  expect_error(prepareData(temp_file))
  unlink(temp_file)
})

# Test 3: Data Transformation Accuracy
test_that("Data is correctly transformed", {
  transformed_data <- prepareData(sample_file_path)
  expected_columns <- c('Patient_ID', 'Date', 'Albumin', 'Bilirubin', 'INR',
                        'Serum_Creatinine', 'Serum_Sodium', 'Gender',
                        'Dialysis_In_Past_Week')
  expect_true(all(expected_columns %in% colnames(transformed_data)))
})

# Test 4: Handling of Missing Values
test_that("Rows with all NA values are removed", {
  data <- prepareData(sample_file_path)
  expect_false(any(rowSums(is.na(data)) == ncol(data)))
})

# Test 5: Return Type
test_that("Function returns a dataframe", {
  result <- prepareData(sample_file_path)
  expect_true(is.data.frame(result))
})
