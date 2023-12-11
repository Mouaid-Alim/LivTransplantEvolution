
library(testthat)
library(dplyr)

# Load the calcMELDThree function file
source("../../R/calcMELDThree.R")

context("Testing calcMELDThree function")

# Test 1: Input Validation
test_that("Input validation for biomarker parameters", {
  expect_error(calcMELDThree('Invalid', 2.4, 1.2, 1.1, 140, 4))
  expect_error(calcMELDThree('M', 'Invalid', 1.2, 1.1, 140, 4))
  expect_error(calcMELDThree('M', 1.3, 'Invalid', 1.1, 140, 4))
  expect_error(calcMELDThree('M', 1.3, 1.2, 'Invalid', 140, 4))
  expect_error(calcMELDThree('M', 1.3, 1.2, 1.1, 'Invalid', 4))
  expect_error(calcMELDThree('M', 1.3, 1.2, 1.1, 140, 'Invalid'))
})

# Test 2: Correct MELD-3.0 Score Calculation
test_that("Correct MELD-3.0 Score Calculation", {
  expect_equal(calcMELDThree('F', 2.3, 1.9, 2, 145, 2), 26)
  # Same example as above but the gender is male
  expect_equal(calcMELDThree('M', 2.3, 1.9, 2, 145, 2), 25)
})

# Test 3: MELD-3.0 Score Boundary Conditions
test_that("MELD-3.0 Score Boundary Conditions", {
  expect_equal(calcMELDThree('M', 0, 0, 0, 160, 4), 6)  # Minimum boundary condition
})

# Test 4: Gender Based Adjustment
test_that("Gender-based adjustment in MELD-3.0 calculation", {
  expect_failure(expect_equal(calcMELDThree('F', 2.3, 1.9, 2, 145, 2), calcMELDThree('M', 2.3, 1.9, 2, 145, 2)))
})

# Test 5: Boundary Checks for Input Values
test_that("Boundary checks for input values", {
  # checks if the input value boundary checks are met for creatine, sodium and albumin
  expect_equal(calcMELDThree('M', 4, 1, 1, 137, 3.5), calcMELDThree('M', 5, 1, 1, 138, 4))
})

# Test 6: Check default values
test_that("default input values check out", {
  # checks if the default input values work as they should when no explicit input is provided
  expect_equal(calcMELDThree(), calcMELDThree('M', 1.3, 1.2, 1.1, 140, 4))
})
