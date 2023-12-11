
library(testthat)
library(ggplot2)
library(tidyr)
library(dplyr)

# Load the visualizeMELD function file
source("../../R/visualizeMELD.R")

context("Testing visualizeMELD function")

# Test 1: Column Name Checks
test_that("Column name checks for input dataframe", {
  temp_data <- data.frame(Patient_ID = 1:5, Date = Sys.Date(), MELD = runif(5),
                          MELDna = runif(5), MELD3_0 = runif(5))
  expect_error(visualizeMELD(temp_data[, -which(names(temp_data) == 'MELD')], 1))  # Removing MELD column
  expect_error(visualizeMELD(temp_data[, -which(names(temp_data) == 'MELDna')], 1))  # Removing MELDna column
  expect_error(visualizeMELD(temp_data[, -which(names(temp_data) == 'MELD3_0')], 1))  # Removing MELD3_0 column
})

# Test 2: Valid Patient ID Check
test_that("Valid patient ID is required", {
  temp_data <- data.frame(Patient_ID = 1:5, Date = Sys.Date(), MELD = runif(5),
                          MELDna = runif(5), MELD3_0 = runif(5))
  expect_error(visualizeMELD(temp_data, 100))  # Invalid patient ID
})

# Test 3: Plot Creation
test_that("Plot is created successfully", {
  temp_data <- data.frame(Patient_ID = 1:5, Date = Sys.Date(), MELD = runif(5),
                          MELDna = runif(5), MELD3_0 = runif(5))
  expect_silent(visualizeMELD(temp_data, 1))  # Valid patient ID
})
